import AbstractValue._
import scala.io.StdIn

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
 *      semantics (ANFSemantics.scala, SchemeSemantics.scala)

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
 *    - A semantics, look at ANFSemantics.scala for a simple example
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

  case class Config(machine: Machine.Value = Machine.Free, lattice: Lattice.Value = Lattice.TypeSet, concrete: Boolean = false, file: Option[String] = None, dotfile: Option[String] = None, anf: Boolean = false, diff: Option[(Int, Int)] = None)

  val parser = new scopt.OptionParser[Config]("scala-am") {
    head("scala-ac", "0.0")
    opt[Machine.Value]('m', "machine") action { (x, c) => c.copy(machine = x) } text("Abstract machine to use (AAM, AAC, Free)")
    opt[Lattice.Value]('l', "lattice") action { (x, c) => c.copy(lattice = x) } text("Lattice to use (Concrete, Type, TypeSet)")
    opt[Unit]('c', "concrete") action { (_, c) => c.copy(concrete = true) } text("Run in concrete mode")
    opt[String]('d', "dotfile") action { (x, c) => c.copy(dotfile = Some(x)) } text("Dot file to output graph to")
    opt[Unit]("anf") action { (_, c) => c.copy(anf = true) } text("Desugar program into ANF")
    // opt[(Int, Int)]("diff") action { (x, c) => c.copy(diff = Some(x)) } text("States to diff") /* TODO: take this into account */
    opt[String]('f', "file") action { (x, c) => c.copy(file = Some(x)) } text("File to read program from")
  }
}

object Main {

  def run[Exp, Abs, Addr](machine: AbstractMachine[Exp, Abs, Addr], sem: Semantics[Exp, Abs, Addr])(program: String, output: Option[String])
  (implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
    addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running ${machine.name} with lattice ${absi.name} and address ${addri.name}")
    val result = machine.eval(sem.parse(program), sem, !output.isEmpty)
    output match {
      case Some(f) => result.toDotFile(f)
      case None => ()
    }
    println(result) // TODO: print stats & final results
  }

  object Done extends Exception

  def fileContent(file: String): String = {
    val f = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    content
  }

  def main(args: Array[String]) {
    import scala.util.control.Breaks._
    Config.parser.parse(args, Config.Config()) match {
      case Some(config) => {
        /* ugly as fuck, but I don't find a simpler way to pass type parameters that are computed at runtime */
        val f = (config.anf, config.machine, config.lattice, config.concrete) match {
          case (true, Config.Machine.AAM, Config.Lattice.Concrete, true) => run[ANFExp, AbstractConcrete, ConcreteAddress](new AAM[ANFExp, AbstractConcrete, ConcreteAddress], new ANFSemantics[AbstractConcrete, ConcreteAddress]) _
          case (false, Config.Machine.AAM, Config.Lattice.Concrete, true) => run[SchemeExp, AbstractConcrete, ConcreteAddress](new AAM[SchemeExp, AbstractConcrete, ConcreteAddress], new SchemeSemantics[AbstractConcrete, ConcreteAddress]) _
          case (true, Config.Machine.AAM, Config.Lattice.Concrete, false) => run[ANFExp, AbstractConcrete, ClassicalAddress](new AAM[ANFExp, AbstractConcrete, ClassicalAddress], new ANFSemantics[AbstractConcrete, ClassicalAddress]) _
          case (false, Config.Machine.AAM, Config.Lattice.Concrete, false) => run[SchemeExp, AbstractConcrete, ClassicalAddress](new AAM[SchemeExp, AbstractConcrete, ClassicalAddress], new SchemeSemantics[AbstractConcrete, ClassicalAddress]) _
          case (true, Config.Machine.AAM, Config.Lattice.Type, true) => run[ANFExp, AbstractType, ConcreteAddress](new AAM[ANFExp, AbstractType, ConcreteAddress], new ANFSemantics[AbstractType, ConcreteAddress]) _
          case (false, Config.Machine.AAM, Config.Lattice.Type, true) => run[SchemeExp, AbstractType, ConcreteAddress](new AAM[SchemeExp, AbstractType, ConcreteAddress], new SchemeSemantics[AbstractType, ConcreteAddress]) _
          case (true, Config.Machine.AAM, Config.Lattice.Type, false) => run[ANFExp, AbstractType, ClassicalAddress](new AAM[ANFExp, AbstractType, ClassicalAddress], new ANFSemantics[AbstractType, ClassicalAddress]) _
          case (false, Config.Machine.AAM, Config.Lattice.Type, false) => run[SchemeExp, AbstractType, ClassicalAddress](new AAM[SchemeExp, AbstractType, ClassicalAddress], new SchemeSemantics[AbstractType, ClassicalAddress]) _
          case (true, Config.Machine.AAM, Config.Lattice.TypeSet, true) => run[ANFExp, AbstractTypeSet, ConcreteAddress](new AAM[ANFExp, AbstractTypeSet, ConcreteAddress], new ANFSemantics[AbstractTypeSet, ConcreteAddress]) _
          case (false, Config.Machine.AAM, Config.Lattice.TypeSet, true) => run[SchemeExp, AbstractTypeSet, ConcreteAddress](new AAM[SchemeExp, AbstractTypeSet, ConcreteAddress], new SchemeSemantics[AbstractTypeSet, ConcreteAddress]) _
          case (true, Config.Machine.AAM, Config.Lattice.TypeSet, false) => run[ANFExp, AbstractTypeSet, ClassicalAddress](new AAM[ANFExp, AbstractTypeSet, ClassicalAddress], new ANFSemantics[AbstractTypeSet, ClassicalAddress]) _
          case (false, Config.Machine.AAM, Config.Lattice.TypeSet, false) => run[SchemeExp, AbstractTypeSet, ClassicalAddress](new AAM[SchemeExp, AbstractTypeSet, ClassicalAddress], new SchemeSemantics[AbstractTypeSet, ClassicalAddress]) _
          case (true, Config.Machine.AAC, Config.Lattice.Concrete, true) => run[ANFExp, AbstractConcrete, ConcreteAddress](new AAC[ANFExp, AbstractConcrete, ConcreteAddress], new ANFSemantics[AbstractConcrete, ConcreteAddress]) _
          case (false, Config.Machine.AAC, Config.Lattice.Concrete, true) => run[SchemeExp, AbstractConcrete, ConcreteAddress](new AAC[SchemeExp, AbstractConcrete, ConcreteAddress], new SchemeSemantics[AbstractConcrete, ConcreteAddress]) _
          case (true, Config.Machine.AAC, Config.Lattice.Concrete, false) => run[ANFExp, AbstractConcrete, ClassicalAddress](new AAC[ANFExp, AbstractConcrete, ClassicalAddress], new ANFSemantics[AbstractConcrete, ClassicalAddress]) _
          case (false, Config.Machine.AAC, Config.Lattice.Concrete, false) => run[SchemeExp, AbstractConcrete, ClassicalAddress](new AAC[SchemeExp, AbstractConcrete, ClassicalAddress], new SchemeSemantics[AbstractConcrete, ClassicalAddress]) _
          case (true, Config.Machine.AAC, Config.Lattice.Type, true) => run[ANFExp, AbstractType, ConcreteAddress](new AAC[ANFExp, AbstractType, ConcreteAddress], new ANFSemantics[AbstractType, ConcreteAddress]) _
          case (false, Config.Machine.AAC, Config.Lattice.Type, true) => run[SchemeExp, AbstractType, ConcreteAddress](new AAC[SchemeExp, AbstractType, ConcreteAddress], new SchemeSemantics[AbstractType, ConcreteAddress]) _
          case (true, Config.Machine.AAC, Config.Lattice.Type, false) => run[ANFExp, AbstractType, ClassicalAddress](new AAC[ANFExp, AbstractType, ClassicalAddress], new ANFSemantics[AbstractType, ClassicalAddress]) _
          case (false, Config.Machine.AAC, Config.Lattice.Type, false) => run[SchemeExp, AbstractType, ClassicalAddress](new AAC[SchemeExp, AbstractType, ClassicalAddress], new SchemeSemantics[AbstractType, ClassicalAddress]) _
          case (true, Config.Machine.AAC, Config.Lattice.TypeSet, true) => run[ANFExp, AbstractTypeSet, ConcreteAddress](new AAC[ANFExp, AbstractTypeSet, ConcreteAddress], new ANFSemantics[AbstractTypeSet, ConcreteAddress]) _
          case (false, Config.Machine.AAC, Config.Lattice.TypeSet, true) => run[SchemeExp, AbstractTypeSet, ConcreteAddress](new AAC[SchemeExp, AbstractTypeSet, ConcreteAddress], new SchemeSemantics[AbstractTypeSet, ConcreteAddress]) _
          case (true, Config.Machine.AAC, Config.Lattice.TypeSet, false) => run[ANFExp, AbstractTypeSet, ClassicalAddress](new AAC[ANFExp, AbstractTypeSet, ClassicalAddress], new ANFSemantics[AbstractTypeSet, ClassicalAddress]) _
          case (false, Config.Machine.AAC, Config.Lattice.TypeSet, false) => run[SchemeExp, AbstractTypeSet, ClassicalAddress](new AAC[SchemeExp, AbstractTypeSet, ClassicalAddress], new SchemeSemantics[AbstractTypeSet, ClassicalAddress]) _
          case (true, Config.Machine.Free, Config.Lattice.Concrete, true) => run[ANFExp, AbstractConcrete, ConcreteAddress](new Free[ANFExp, AbstractConcrete, ConcreteAddress], new ANFSemantics[AbstractConcrete, ConcreteAddress]) _
          case (false, Config.Machine.Free, Config.Lattice.Concrete, true) => run[SchemeExp, AbstractConcrete, ConcreteAddress](new Free[SchemeExp, AbstractConcrete, ConcreteAddress], new SchemeSemantics[AbstractConcrete, ConcreteAddress]) _
          case (true, Config.Machine.Free, Config.Lattice.Concrete, false) => run[ANFExp, AbstractConcrete, ClassicalAddress](new Free[ANFExp, AbstractConcrete, ClassicalAddress], new ANFSemantics[AbstractConcrete, ClassicalAddress]) _
          case (false, Config.Machine.Free, Config.Lattice.Concrete, false) => run[SchemeExp, AbstractConcrete, ClassicalAddress](new Free[SchemeExp, AbstractConcrete, ClassicalAddress], new SchemeSemantics[AbstractConcrete, ClassicalAddress]) _
          case (true, Config.Machine.Free, Config.Lattice.Type, true) => run[ANFExp, AbstractType, ConcreteAddress](new Free[ANFExp, AbstractType, ConcreteAddress], new ANFSemantics[AbstractType, ConcreteAddress]) _
          case (false, Config.Machine.Free, Config.Lattice.Type, true) => run[SchemeExp, AbstractType, ConcreteAddress](new Free[SchemeExp, AbstractType, ConcreteAddress], new SchemeSemantics[AbstractType, ConcreteAddress]) _
          case (true, Config.Machine.Free, Config.Lattice.Type, false) => run[ANFExp, AbstractType, ClassicalAddress](new Free[ANFExp, AbstractType, ClassicalAddress], new ANFSemantics[AbstractType, ClassicalAddress]) _
          case (false, Config.Machine.Free, Config.Lattice.Type, false) => run[SchemeExp, AbstractType, ClassicalAddress](new Free[SchemeExp, AbstractType, ClassicalAddress], new SchemeSemantics[AbstractType, ClassicalAddress]) _
          case (true, Config.Machine.Free, Config.Lattice.TypeSet, true) => run[ANFExp, AbstractTypeSet, ConcreteAddress](new Free[ANFExp, AbstractTypeSet, ConcreteAddress], new ANFSemantics[AbstractTypeSet, ConcreteAddress]) _
          case (false, Config.Machine.Free, Config.Lattice.TypeSet, true) => run[SchemeExp, AbstractTypeSet, ConcreteAddress](new Free[SchemeExp, AbstractTypeSet, ConcreteAddress], new SchemeSemantics[AbstractTypeSet, ConcreteAddress]) _
          case (true, Config.Machine.Free, Config.Lattice.TypeSet, false) => run[ANFExp, AbstractTypeSet, ClassicalAddress](new Free[ANFExp, AbstractTypeSet, ClassicalAddress], new ANFSemantics[AbstractTypeSet, ClassicalAddress]) _
          case (false, Config.Machine.Free, Config.Lattice.TypeSet, false) => run[SchemeExp, AbstractTypeSet, ClassicalAddress](new Free[SchemeExp, AbstractTypeSet, ClassicalAddress], new SchemeSemantics[AbstractTypeSet, ClassicalAddress]) _
          case _ => throw new Exception(s"Impossible configuration: $config")
        }
        try {
          do {
            val program = config.file match {
              case Some(file) => fileContent(file)
              case None => StdIn.readLine(">>> ")
            }
            if (program == null) throw Done
            f(program, config.dotfile)
          } while (config.file.isEmpty)
        } catch {
          case Done => ()
        }
      }
      case None => ()
    }
  }
}
