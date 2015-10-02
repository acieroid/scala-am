import AbstractValue._

/**
 * This is the entry point. It parses the arguments, parses the input file and
 * launch an abstract machine on the parsed expression. The code isn't very
 * clean and I'd like to improve it at some point, but my scala-fu isn't good
 * enough to do it now. The pipeline goes as follows:
 *   1. The input file is parsed. For Scheme files, it is done by:
 *      - Parsing the file as a list of s-expressions (SExp.scala, SExpParser.scala)
 *      - Compiling these s-expressions into Scheme expressions (Scheme.scala)
 *      - Optionally, converting Scheme expressions into ANF form (ANF.scala) to
 *        have a simpler interpreter (but longer programs)

 *   2. The abstract machine is created by giving it some semantics. For now,
 *      the only semantics available are ANF Scheme semantics and Scheme
 *      semantics (Semantics.scala)

 *   3. The abstract machine performs its evaluation, relying on methods of the
 *      semantics class to know how to evaluate expressions. The abstract
 *      machine only deals with which states to evaluate in which order, where
 *      to store values, where to store continuations, how to push and pop
 *      continuations, etc. The semantics encode what to do when encountering a
 *      program construct. For example, the semantics can tell what to evaluate
 *      next, that a continuation needs to be pushed, or that a variable needs
 *      to be updated. The abstract machine will then respectively evaluate the
 *      expression needed, push the continuation, or update the variable.
 *
 *      Three abstract machine implementations are available:
 *      - The classical Abstracting Abstract Machine of Might and Van Horn (AAM.scala)
 *      - Johnson's Abstracting Abstract Control (AAC.scala)
 *      - Gilrey's Pushdown Control-Flow Analysis for Free (Free.scala)
 *
 *      The abstract machine also uses a lattice to represent values. Lattices
 *      should implement some traits that can be found in
 *      AbstractValue.scala. The following lattices are available:
 *      - A concrete lattice, AbstractConcrete.scala
 *      - A type lattice, representing each value by its type, AbstractType.scala
 *      - A type set lattice, representing each value by a set of its possible
 *        types (to avoid having a top element that loses all precision),
 *        AbstractTypeSet.scala
 *
 *  If you want to:
 *  - Support a new language: you will need:
 *    - A parser, you can look into SExpParser.scala as an inspiration. If your
 *      language is s-expression based, you can use this parser and compile
 *      s-expressions into your abstract grammar. To do so, look at Scheme.scala
 *    - An abstract grammar, look at SExp.scala or the SchemeExp class in Scheme.scala
 *    - A semantics, look at Semantics.scala
 *    - Support for your language operations at the lattice level. For this,
 *      you'll probably need to extend the lattices (AbstractValue.scala,
 *      AbstractConcrete.scala, AbstractType.scala, AbstractTypeSet.scala)
 *  - Play with abstract machines, you can look into AAM.scala, AAC.scala or Free.scala
 *  - Implement some kind of analysis, you'll probably need to design a lattice
 *    that is suited for your analysis. You can use an existing lattice as an
 *    inspiration (AbstractType.scala is a good lattice to start with, even
 *    though its very imprecise, it's simple. AbstractTypeSet.scala for
 *    example).
 */
object Config {
  object Machine extends Enumeration {
    type Machine = Value
    val AAC, AAM, Free = Value
  }
  implicit val machineRead: scopt.Read[Machine.Value] = scopt.Read.reads(Machine withName _)

  object Lattice extends Enumeration {
    type Lattice = Value
    val Concrete, Type, TypeSet = Value
  }
  implicit val latticeRead: scopt.Read[Lattice.Value] = scopt.Read.reads(Lattice withName _)

  case class Config(machine: Machine.Value = Machine.Free, lattice: Lattice.Value = Lattice.TypeSet, concrete: Boolean = false, file: String = "", dotfile: Option[String] = None, anf: Boolean = false, diff: Option[(Int, Int)] = None)

  val parser = new scopt.OptionParser[Config]("scala-am") {
    head("scala-ac", "0.0")
    opt[Machine.Value]('m', "machine") action { (x, c) => c.copy(machine = x) } text("Abstract machine to use (AAM, AAC, Free)")
    opt[Lattice.Value]('l', "lattice") action { (x, c) => c.copy(lattice = x) } text("Lattice to use (Concrete, Type, TypeSet)")
    opt[Unit]('c', "concrete") action { (_, c) => c.copy(concrete = true) } text("Run in concrete mode")
    opt[String]('d', "dotfile") action { (x, c) => c.copy(dotfile = Some(x)) } text("Dot file to output graph to")
    opt[Unit]("anf") action { (_, c) => c.copy(anf = true) } text("Desugar program into ANF")
    opt[(Int, Int)]("diff") action { (x, c) => c.copy(diff = Some(x)) } text("States to diff") /* TODO: take this into account */
    arg[String]("<file>") required() maxOccurs(1) action { (x, c) => c.copy(file = x) } text("File to read program from")
  }
}

object Main {

  def runAAM[Abs, Addr](exp: SchemeExp, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running AAM with lattice ${absi.name} and address ${addri.name}")
    val machine = new AAM[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])
    val result = machine.eval(exp, output)
    println(s"${result.size} possible results: $result")
  }

  def runAAC[Abs, Addr](exp: SchemeExp, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running AAC with lattice ${absi.name} and address ${addri.name}")
    val machine = new AAC[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])
    val result = machine.eval(exp, output)
    println(s"${result.size} possible results: $result")
  }

  def runFree[Abs, Addr](exp: SchemeExp, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running Free with lattice ${absi.name} and address ${addri.name}")
    val machine = new Free[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])
    val result = machine.eval(exp, output)
    println(s"${result.size} possible results: $result")
  }

  def runAAMANF[Abs, Addr](exp: ANFExp, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running AAM (ANF) with lattice ${absi.name} and address ${addri.name}")
    val machine = new AAM[Abs, Addr, ANFExp](new ANFSemantics[Abs, Addr])
    val result = machine.eval(exp, output)
    println(s"${result.size} possible results: $result")
  }

  def runAACANF[Abs, Addr](exp: ANFExp, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running AAC (ANF) with lattice ${absi.name} and address ${addri.name}")
    val machine = new AAC[Abs, Addr, ANFExp](new ANFSemantics[Abs, Addr])
    val result = machine.eval(exp, output)
    println(s"${result.size} possible results: $result")
  }

  def runFreeANF[Abs, Addr](exp: ANFExp, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running Free (ANF) with lattice ${absi.name} and address ${addri.name}")
    val machine = new Free[Abs, Addr, ANFExp](new ANFSemantics[Abs, Addr])
    val result = machine.eval(exp, output)
    println(s"${result.size} possible results: $result")
  }

  def main(args: Array[String]) {
    Config.parser.parse(args, Config.Config()) match {
      case Some(config) => {
        /* ugly as fuck, but I don't find a simpler way to pass type parameters that are computed at runtime */
        if (config.anf) {
          val f = (config.machine, config.lattice, config.concrete) match {
            case (Config.Machine.AAM, Config.Lattice.Concrete, true) => runAAMANF[AbstractConcrete, ConcreteAddress] _
            case (Config.Machine.AAM, Config.Lattice.Concrete, false) => runAAMANF[AbstractConcrete, ClassicalAddress] _
            case (Config.Machine.AAM, Config.Lattice.TypeSet, true) => runAAMANF[AbstractTypeSet, ConcreteAddress] _
            case (Config.Machine.AAM, Config.Lattice.TypeSet, false) => runAAMANF[AbstractTypeSet, ClassicalAddress] _
            case (Config.Machine.AAM, Config.Lattice.Type, true) => runAAMANF[AbstractType, ConcreteAddress] _
            case (Config.Machine.AAM, Config.Lattice.Type, false) => runAAMANF[AbstractType, ClassicalAddress] _
            case (Config.Machine.AAC, Config.Lattice.Concrete, true) => runAACANF[AbstractConcrete, ConcreteAddress] _
            case (Config.Machine.AAC, Config.Lattice.Concrete, false) => runAACANF[AbstractConcrete, ClassicalAddress] _
            case (Config.Machine.AAC, Config.Lattice.TypeSet, true) => runAACANF[AbstractTypeSet, ConcreteAddress] _
            case (Config.Machine.AAC, Config.Lattice.TypeSet, false) => runAACANF[AbstractTypeSet, ClassicalAddress] _
            case (Config.Machine.AAC, Config.Lattice.Type, true) => runAACANF[AbstractType, ConcreteAddress] _
            case (Config.Machine.AAC, Config.Lattice.Type, false) => runAACANF[AbstractType, ClassicalAddress] _
            case (Config.Machine.Free, Config.Lattice.Concrete, true) => runFreeANF[AbstractConcrete, ConcreteAddress] _
            case (Config.Machine.Free, Config.Lattice.Concrete, false) => runFreeANF[AbstractConcrete, ClassicalAddress] _
            case (Config.Machine.Free, Config.Lattice.TypeSet, true) => runFreeANF[AbstractTypeSet, ConcreteAddress] _
            case (Config.Machine.Free, Config.Lattice.TypeSet, false) => runFreeANF[AbstractTypeSet, ClassicalAddress] _
            case (Config.Machine.Free, Config.Lattice.Type, true) => runFreeANF[AbstractType, ConcreteAddress] _
            case (Config.Machine.Free, Config.Lattice.Type, false) => runFreeANF[AbstractType, ClassicalAddress] _
            case _ => throw new Exception(s"Impossible configuration: $config")
          }
          val program = ANF.parse(config.file)
          println(program)
          f(program, config.dotfile)
        } else {
          val f = (config.machine, config.lattice, config.concrete) match {
            case (Config.Machine.AAM, Config.Lattice.Concrete, true) => runAAM[AbstractConcrete, ConcreteAddress] _
            case (Config.Machine.AAM, Config.Lattice.Concrete, false) => runAAM[AbstractConcrete, ClassicalAddress] _
            case (Config.Machine.AAM, Config.Lattice.TypeSet, true) => runAAM[AbstractTypeSet, ConcreteAddress] _
            case (Config.Machine.AAM, Config.Lattice.TypeSet, false) => runAAM[AbstractTypeSet, ClassicalAddress] _
            case (Config.Machine.AAM, Config.Lattice.Type, true) => runAAM[AbstractType, ConcreteAddress] _
            case (Config.Machine.AAM, Config.Lattice.Type, false) => runAAM[AbstractType, ClassicalAddress] _
            case (Config.Machine.AAC, Config.Lattice.Concrete, true) => runAAC[AbstractConcrete, ConcreteAddress] _
            case (Config.Machine.AAC, Config.Lattice.Concrete, false) => runAAC[AbstractConcrete, ClassicalAddress] _
            case (Config.Machine.AAC, Config.Lattice.TypeSet, true) => runAAC[AbstractTypeSet, ConcreteAddress] _
            case (Config.Machine.AAC, Config.Lattice.TypeSet, false) => runAAC[AbstractTypeSet, ClassicalAddress] _
            case (Config.Machine.AAC, Config.Lattice.Type, true) => runAAC[AbstractType, ConcreteAddress] _
            case (Config.Machine.AAC, Config.Lattice.Type, false) => runAAC[AbstractType, ClassicalAddress] _
            case (Config.Machine.Free, Config.Lattice.Concrete, true) => runFree[AbstractConcrete, ConcreteAddress] _
            case (Config.Machine.Free, Config.Lattice.Concrete, false) => runFree[AbstractConcrete, ClassicalAddress] _
            case (Config.Machine.Free, Config.Lattice.TypeSet, true) => runFree[AbstractTypeSet, ConcreteAddress] _
            case (Config.Machine.Free, Config.Lattice.TypeSet, false) => runFree[AbstractTypeSet, ClassicalAddress] _
            case (Config.Machine.Free, Config.Lattice.Type, true) => runFree[AbstractType, ConcreteAddress] _
            case (Config.Machine.Free, Config.Lattice.Type, false) => runFree[AbstractType, ClassicalAddress] _
            case _ => throw new Exception(s"Impossible configuration: $config")
          }
          val program = Scheme.parse(config.file)
          println(program)
          f(program, config.dotfile)
        }
      }
      case None => ()
    }
  }
}
