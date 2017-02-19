import scala.io.StdIn
import Util._
import scala.util.{Try, Success, Failure}

/**
 * Before looking at this, we recommend seeing how to use this framework. A
 * detailed example is available in LambdaCalculus.scala.
 *
 * This is the entry point. It parses the arguments, parses the input file and
 * launches an abstract machine on the parsed expression (or launches a REPL if no
 * input file is given). The pipeline goes as follows:
 *   1. The input program is parsed. For Scheme programs, it is done by:
 *      - Parsing the file as a list of s-expressions (exp/SExp.scala,
 *        exp/SExpParser.scala)
 *      - Compiling these s-expressions into Scheme expressions
 *        (exp/scheme/Scheme.scala)

 *   2. To run the program, we need an abstract machine and some
 *      semantics. Semantics definitions have to implement the Semantics
 *      interface (semantics/Semantics.scala).

 *   3. Once the abstract machine is created and we have a semantics for the
 *      program we want to analyze, the abstract machine can perform its
 *      evaluation, relying on methods of the semantics class to know how to
 *      evaluate expressions. The abstract machine only deals with which states
 *      to evaluate in which order, where to store values, where to store
 *      continuations, how to push and pop continuations, etc. The semantics
 *      encode what to do when encountering a program construct. For example,
 *      the semantics can tell what to evaluate next, that a continuation needs
 *      to be pushed, or that a variable needs to be updated. The abstract
 *      machine will then respectively evaluate the expression needed, push the
 *      continuation, or update the variable.
 *
 *      Multiple abstract machine implementations are available, defined in the
 *      machine/ directory. Every abstract machine implementation has to
 *      implement the AbstractMachine interface (machine/AbstractMachine.scala).
 *
 *      The abstract machine also uses a lattice to represent values. Lattices
 *      should implement the JoinLattice trait that can be found in
 *      JoinLattice.scala, which provides the basic features of a lattice.
 *
 *  If you want to:
 *  - Support a new language: you will need:
 *    - A parser, you can look into exp/SExpParser.scala as an inspiration. If
 *      your language is s-expression based, you can use this parser and compile
 *      s-expressions into your abstract grammar. To do so, look at
 *      exp/scheme/Scheme.scala.
 *    - An abstract grammar, look at exp/SExp.scala or the SchemeExp class in
 *      exp/scheme/Scheme.scala.
 *    - A semantics, look at semantics/anf/ANFSemantics.scala for a simple example.
 *    - Support for your language operations at the lattice level. For this,
 *      you'll probably need to extend the lattices (see
 *      lattice/scheme/SchemeLattice.scala, lattice/scheme/ModularLattice.scala)
 *  - Play with abstract machines, you can look into AAM.scala.
 *  - Implement some kind of analysis, look at examples/LambdaCalculus.scala and
 *    examples/TaintAnalysis.scala.
 */
object Main {
  /** Run a machine on a program with the given semantics. If @param output is
    * set, generate a dot graph visualizing the computed graph in the given
    * file. Return the number of states and time taken. */
  def run[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp](machine: AbstractMachine[Exp, Abs, Addr, Time], sem: Semantics[Exp, Abs, Addr, Time])(program: String, outputDot: Option[String], outputJSON: Option[String], timeout: Option[Long], inspect: Boolean): (Int, Double) = {
    println(s"Running ${machine.name} with lattice ${JoinLattice[Abs].name} and address ${Address[Addr].name}")
    val result = machine.eval(sem.parse(program), sem, !outputDot.isEmpty || !outputJSON.isEmpty, Timeout.start(timeout))
    outputDot.foreach(result.toFile(_)(GraphDOTOutput))
    outputJSON.foreach(result.toFile(_)(GraphJSONOutput))
    if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
    println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
    (result.numberOfStates, result.time)
  }

  def main(args: Array[String]) {
    import scala.util.control.Breaks._
    Config.parser.parse(args, Config.Config()).foreach(config => {
        val lattice: SchemeLattice = config.lattice match {
          case Config.Lattice.Concrete => new MakeSchemeLattice[Concrete.S, Concrete.B, Concrete.I, Concrete.F, Concrete.C, Concrete.Sym](config.counting)
          case Config.Lattice.TypeSet => new MakeSchemeLattice[Type.S, Concrete.B, Type.I, Type.F, Type.C, Type.Sym](config.counting)
          case Config.Lattice.BoundedInt =>
            val bounded = new BoundedInteger(config.bound)
                new MakeSchemeLattice[Type.S, Concrete.B, bounded.I, Type.F, Type.C, Type.Sym](config.counting)
          case Config.Lattice.ConstantPropagation => new MakeSchemeLattice[ConstantPropagation.S, Concrete.B, ConstantPropagation.I, ConstantPropagation.F, ConstantPropagation.C, ConstantPropagation.Sym](config.counting)
        }
        implicit val isSchemeLattice: IsSchemeLattice[lattice.L] = lattice.isSchemeLattice

        config.language match {
          case Config.Language.Scheme =>
            val time: TimestampWrapper = if (config.concrete) ConcreteTimestamp else ZeroCFA
            implicit val isTimestamp = time.isTimestamp

            val address: AddressWrapper = config.address match {
              case Config.Address.Classical => ClassicalAddress
              case Config.Address.ValueSensitive => ValueSensitiveAddress
            }
            implicit val isAddress = address.isAddress

            val machine = config.machine match {
              case Config.Machine.AAM => new AAM[SchemeExp, lattice.L, address.A, time.T]
              case Config.Machine.AAMGlobalStore => new AAMAACP4F[SchemeExp, lattice.L, address.A, time.T](AAMKAlloc)
              case Config.Machine.ConcreteMachine => new ConcreteMachine[SchemeExp, lattice.L, address.A, time.T]
              case Config.Machine.AAC => new AAMAACP4F[SchemeExp, lattice.L, address.A, time.T](AACKAlloc)
              case Config.Machine.Free => new AAMAACP4F[SchemeExp, lattice.L, address.A, time.T](P4FKAlloc)
            }

            val sem = new SchemeSemantics[lattice.L, address.A, time.T](new SchemePrimitives[address.A, lattice.L])

            replOrFile(config.file, program => run(machine, sem)(program, config.dotfile, config.jsonfile, config.timeout.map(_.toNanos), config.inspect))
          case Config.Language.CScheme =>
            val clattice: CSchemeLattice = new MakeCSchemeLattice[lattice.L]
            implicit val isCSchemeLattice = clattice.isCSchemeLattice

            val time: TimestampWrapper = if (config.concrete) ConcreteTimestamp else ZeroCFA
            implicit val isTimestamp = time.isTimestamp

            val address: AddressWrapper = config.address match {
              case Config.Address.Classical => ClassicalAddress
              case Config.Address.ValueSensitive => ValueSensitiveAddress
            }
            implicit val isAddress = address.isAddress

            val machine = config.machine match {
              case Config.Machine.AAM => new ConcurrentAAM[SchemeExp, clattice.L, address.A, time.T, ContextSensitiveTID](AllInterleavings)
              case _ => throw new Exception(s"unsupported machine for CScheme: ${config.machine}")
            }

            val sem = new CSchemeSemantics[clattice.L, address.A, time.T, ContextSensitiveTID](new CSchemePrimitives[address.A, clattice.L])
            replOrFile(config.file, program => run(machine, sem)(program, config.dotfile, config.jsonfile, config.timeout.map(_.toNanos), config.inspect))
          case Config.Language.AScheme =>
            val alattice: ASchemeLattice = new MakeASchemeLattice[lattice.L]
            implicit val isASchemeLattice = alattice.isASchemeLattice

            import ActorTimestamp.fromTime
            val time: ActorTimestampWrapper = if (config.concrete) ConcreteTimestamp else KMessageTagSensitivity(1)
            implicit val isTimestamp = time.isActorTimestamp

            val address: AddressWrapper = config.address match {
              case Config.Address.Classical => ClassicalAddress
              case Config.Address.ValueSensitive => ValueSensitiveAddress
            }
            implicit val isAddress = address.isAddress

            val mbox = config.mbox match {
              case Config.Mbox.Powerset => new PowersetMboxImpl[ContextSensitiveTID, alattice.L]
              case Config.Mbox.BoundedList => new BoundedListMboxImpl[ContextSensitiveTID, alattice.L](config.mboxBound)
              case Config.Mbox.BoundedMultiset => new BoundedMultisetMboxImpl[ContextSensitiveTID, alattice.L](config.mboxBound)
              case Config.Mbox.Graph => new GraphMboxImpl[ContextSensitiveTID, alattice.L]
            }

            val machine = config.machine match {
              case Config.Machine.AAM => new ActorsAAM[SchemeExp, alattice.L, address.A, time.T, ContextSensitiveTID](mbox)
              case Config.Machine.AAMGlobalStore => new ActorsAAMGlobalStore[SchemeExp, alattice.L, address.A, time.T, ContextSensitiveTID](mbox)
              case _ => throw new Exception(s"unsupported machine for AScheme: ${config.machine}")
            }

            val visitor = new RecordActorVisitor[SchemeExp, alattice.L, address.A]
            val sem = new ASchemeSemanticsWithVisitorAndOptimization[alattice.L, address.A, time.T, ContextSensitiveTID](new SchemePrimitives[address.A, alattice.L], visitor)
            val N = 1
            val warmup = if (N > 1) 2 else 0 // 2 runs that are ignored to warm up
            val (states, times) = (1 to N+warmup).map(i =>
              runOnFile(config.file.get, program => run(machine, sem)(program, config.dotfile, config.jsonfile, config.timeout.map(_.toNanos), config.inspect))).unzip
            println("States: " + states.mkString(", "))
            println("Time: " + times.drop(warmup).mkString(","))
            if (N == 1) visitor.print
        }
      })
    Profiler.print
  }
}

object ScalaAM {
  /** Simply run a program and return the result. Compute the graph is @param graph is true. */
  def run[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp](machine: AbstractMachine[Exp, Abs, Addr, Time], sem: Semantics[Exp, Abs, Addr, Time])(program: String, graph: Boolean = true, timeout: Option[Long] = None): AbstractMachine[Exp, Abs, Addr, Time]#Output = {
    val result = machine.eval(sem.parse(program), sem, graph, Timeout.start(timeout))
    if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
    result
  }
  /** Some lattice instanciations */
  val typeLattice = new MakeSchemeLattice[Type.S, Concrete.B, Type.I, Type.F, Type.C, Type.Sym](false)
  val concreteLattice = new MakeSchemeLattice[Concrete.S, Concrete.B, Concrete.I, Concrete.F, Concrete.C, Concrete.Sym](false)
  val cpLattice = new MakeSchemeLattice[ConstantPropagation.S, Concrete.B, ConstantPropagation.I, ConstantPropagation.F, ConstantPropagation.C, ConstantPropagation.Sym](false)

  /* You can then launch a console to load ScalaAM (sbt console), and perform the following:
   * > import ScalaAM._
   * > run(new AAM[SchemeExp, cpLattice.L, ClassicalAddress.A, ZeroCFA.T], new SchemeSemantics[cpLattice.L, ClassicalAddress.A, ZeroCFA.T](new SchemePrimitives[ClassicalAddress.A, cpLattice.L]))("(* 2 3)")
   * From there on, you can inspect the result.
   */

  /* Or you can use one of the preinstantiated machines:
   * > ScalaAM.FastConcrete.eval("(+ 1 2 3)")
   * Or with a REPL:
   * > ScalaAM.repl(ScalaAM.FastConcrete.eval _)
   */
  object FastConcrete {
    def eval(program: String, timeout: Option[Long] = None): Option[concreteLattice.L] = {
      val output = run[SchemeExp, concreteLattice.L, ClassicalAddress.A, ConcreteTimestamp.T](
        new ConcreteMachine[SchemeExp, concreteLattice.L, ClassicalAddress.A, ConcreteTimestamp.T],
        new SchemeSemantics[concreteLattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, concreteLattice.L]))(program, false, timeout)
      assert(output.finalValues.size <= 1)
      output.finalValues.headOption
    }
  }

  object ConstantPropagationAAM {
    def eval(program: String, timeout: Option[Long] = None): Set[cpLattice.L] = {
      val output = run[SchemeExp, cpLattice.L, ClassicalAddress.A, ZeroCFA.T](
        new AAM[SchemeExp, cpLattice.L, ClassicalAddress.A, ZeroCFA.T],
        new SchemeSemantics[cpLattice.L, ClassicalAddress.A, ZeroCFA.T](new SchemePrimitives[ClassicalAddress.A, cpLattice.L]))(program, false, timeout)
      output.finalValues
    }
  }

  object TypeAAM {
    def eval(program: String, timeout: Option[Long] = None): Set[typeLattice.L] = {
      val output = run[SchemeExp, typeLattice.L, ClassicalAddress.A, ZeroCFA.T](
        new AAM[SchemeExp, typeLattice.L, ClassicalAddress.A, ZeroCFA.T],
        new SchemeSemantics[typeLattice.L, ClassicalAddress.A, ZeroCFA.T](new SchemePrimitives[ClassicalAddress.A, typeLattice.L]))(program, false, timeout)
      output.finalValues
    }
  }

  def repl[A](eval: (String, Option[Long]) => A): Unit = {
    Util.replOrFile(None, p => println(eval(p, None)))
  }
}

object ActorExperiments {
  val lat = new MakeASchemeLattice[ScalaAM.typeLattice.L]
  val timeout: Option[Long] = None // Some(1000000000)
  implicit val isASchemeLattice = lat.isASchemeLattice
  val time: ActorTimestampWrapper = KMessageTagSensitivity(0)
  implicit val isActorTimestamp = time.isActorTimestamp
  //val mbox = new PowersetMboxImpl[ContextSensitiveTID, lat.L]
  val mbox = new BoundedListMboxImpl[ContextSensitiveTID, lat.L](1)
  //val mbox = new GraphMboxImpl[ContextSensitiveTID, lat.L]
  val machine = new ActorsAAMGlobalStoreUnboundedActors[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](mbox)
  // val machine = new ActorsAAMGlobalStore[SchemeExp, lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](mbox)
  val sem = new ASchemeSemantics[lat.L, ClassicalAddress.A, time.T, ContextSensitiveTID](new SchemePrimitives[ClassicalAddress.A, lat.L])

  def run(file: String) = {
    Util.runOnFile(file, ScalaAM.run[SchemeExp, lat.L, ClassicalAddress.A, time.T](machine, sem)(_, true, timeout))
  }
  def main(args: Array[String]): Unit = {
    val result = run(args(0))
    val graph = args(1)
    if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
    println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
    result.toFile(graph)(GraphDOTOutput)
  }
  // TODO: instead of using mailbox as actor context, use set of sent messages, or k last messages
  // why? Because, with powerset we seem to maintain a low state space overhead of using an actor context, but the others (L1, G) have a much bigger overhead
}
