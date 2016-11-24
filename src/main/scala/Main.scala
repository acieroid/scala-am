import scala.io.StdIn
import Util._
import scala.util.{Try, Success, Failure}

/**
 * Before looking at this, we recommend seeing how to use this framework. A
 * detailed example is available in LambdaCalculus.scala.
 *
 * This is the entry point. It parses the arguments, parses the input file and
 * launch an abstract machine on the parsed expression (or launches a REPL if no
 * input file is given). The code in this file isn't very clean and I'd like to
 * improve it at some point, but my scala-fu isn't good enough to do it now. The
 * pipeline goes as follows:
 *   1. The input program is parsed. For Scheme programs, it is done by:
 *      - Parsing the file as a list of s-expressions (exp/SExp.scala,
 *        exp/SExpParser.scala)
 *      - Compiling these s-expressions into Scheme expressions
 *        (exp/scheme/Scheme.scala)
 *      - Optionally, converting Scheme expressions into ANF form
 *        (exp/anf/ANF.scala) to have a simpler interpreter (but longer
 *        programs)

 *   2. To run the program, we need an abstract machine and some semantics. For
 *      now, the only semantics available are for ANF Scheme and Scheme
 *      (semantics/anf/ANFSemantics.scala,
 *      semantics/scheme/SchemeSemantics.scala). Semantics definitions have to
 *      implement the Semantics interface (semantics/Semantics.scala).

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
 *      Multiple abstract machine implementations are available, including:
 *      - The classical Abstracting Abstract Machine of Might and Van Horn (machine/AAM.scala)
 *      - Johnson's Abstracting Abstract Control (machine/AAC.scala)
 *      - Gilrey's Pushdown Control-Flow Analysis for Free (machine/Free.scala)
 *      - A fast concrete interpreter (machine/ConcreteMachine.scala)
 *      Every abstract machine implementation has to implement the AbstractMachine
 *      interface (machine/AbstractMachine.scala).
 *
 *      The abstract machine also uses a lattice to represent values. Lattices
 *      should implement the AbstractValue trait that can be found in
 *      AbstractValue.scala. The following lattices are available:
 *      - A modular lattice (lattice/scheme/ModularLattice.scala) where every component
 *        (numbers, strings, ...) can be specified independently of each
 *        other. It can then automatically transform a lattice into a powerset
 *        lattice. There are implementations for concrete values, type
 *        representations of a value, and bounded integers.
 *      - A product lattice, combining two lattices together as a cartesian
 *        product. Example: one could combine the type lattice with a sign
 *        lattice, getting abstract values such as (Int, +), (String, bottom),
 *        ...
 *
 *  If you want to:
 *  - Support a new language: you will need:
 *    - A parser, you can look into exp/SExpParser.scala as an inspiration. If your
 *      language is s-expression based, you can use this parser and compile
 *      s-expressions into your abstract grammar. To do so, look at exp/scheme/Scheme.scala.
 *    - An abstract grammar, look at exp/SExp.scala or the SchemeExp class in exp/scheme/Scheme.scala.
 *    - A semantics, look at semantics/anf/ANFSemantics.scala for a simple example.
 *    - Support for your language operations at the lattice level. For this,
 *      you'll probably need to extend the lattices (lattice/AbstractValue.scala,
 *      lattice/ModularLattice.scala, ...)
 *  - Play with abstract machines, you can look into AAM.scala, AAC.scala or
 *    Free.scala (AAM is the simplest machine).
 *  - Implement some kind of analysis, you'll probably need to design a lattice
 *    that is suited for your analysis. You can use an existing lattice as an
 *    inspiration.
 */

object Main {
  /** Run a machine on a program with the given semantics. If @param output is
    * set, generate a dot graph visualizing the computed graph in the given
    * file. */
  def run[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp](machine: AbstractMachine[Exp, Abs, Addr, Time], sem: Semantics[Exp, Abs, Addr, Time])(program: String, outputDot: Option[String], outputJSON: Option[String], timeout: Option[Long], inspect: Boolean): Unit = {
    println(s"Running ${machine.name} with lattice ${JoinLattice[Abs].name} and address ${Address[Addr].name}")
    val result = machine.eval(sem.parse(program), sem, !outputDot.isEmpty || !outputJSON.isEmpty, timeout)
    outputDot.foreach(result.toDotFile _)
    outputJSON.foreach(result.toJSONFile _)
    if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
    println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
    if (inspect) {
      replOrFile(None, input => input.indexOf(".") match {
        case -1 => println(s"Unknown inspection query: $input")
        case n => Try(input.subSequence(0, n).toString.toInt) match {
          case Success(state) => result.inspect(state, input.subSequence(n + 1, input.size).toString)
          case Failure(e) => println(s"Cannot parse state number (${input.subSequence(0, n)}): $e")
        }
      })
    }
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

            val time: TimestampWrapper = if (config.concrete) ConcreteTimestamp else ZeroCFA
            implicit val isTimestamp = time.isTimestamp

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

            val visitor = new RecordActorVisitor[alattice.L]
            val sem = new ASchemeSemantics[alattice.L, address.A, time.T, ContextSensitiveTID](new SchemePrimitives[address.A, alattice.L], visitor)
            replOrFile(config.file, program => run(machine, sem)(program, config.dotfile, config.jsonfile, config.timeout.map(_.toNanos), config.inspect))
            visitor.print
        }
      })
    Profiler.print
  }
}
