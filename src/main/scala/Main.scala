import scala.io.StdIn

/**
 * This is the entry point. It parses the arguments, parses the input file and
 * launch an abstract machine on the parsed expression (or launches a REPL if no
 * input file is given). The code in this file isn't very clean and I'd like to
 * improve it at some point, but my scala-fu isn't good enough to do it now. The
 * pipeline goes as follows:
 *   1. The input program is parsed. For Scheme programs, it is done by:
 *      - Parsing the file as a list of s-expressions (SExp.scala, SExpParser.scala)
 *      - Compiling these s-expressions into Scheme expressions (Scheme.scala)
 *      - Optionally, converting Scheme expressions into ANF form (ANF.scala) to
 *        have a simpler interpreter (but longer programs)

 *   2. To run the program, we need an abstract machine and some semantics. For
 *      now, the only semantics available are for ANF Scheme and Scheme
 *      (ANFSemantics.scala, SchemeSemantics.scala). Semantics definitions have
 *      to implement the Semantics interface (Semantics.scala).

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
 *      Three abstract machine implementations are available:
 *      - The classical Abstracting Abstract Machine of Might and Van Horn (AAM.scala)
 *      - Johnson's Abstracting Abstract Control (AAC.scala)
 *      - Gilrey's Pushdown Control-Flow Analysis for Free (Free.scala) Every
 *      abstract machine implementation has to implement the AbstractMachine
 *      interface (AbstractMachine.scala).
 *
 *      The abstract machine also uses a lattice to represent values. Lattices
 *      should implement some traits that can be found in
 *      AbstractValue.scala. The following lattices are available:
 *      - A concrete lattice, ConcreteLattice.scala
 *      - A type lattice, representing each value by its type, TypeLattice.scala
 *      Other lattices are made available by combining existing lattices, using:
 *      - A powerset lattice, representing elements as sets of values. Example:
 *        the type lattice can represent Int, or String, but not join the two
 *        elements together (and raises a CannotJoin exception). The powerset
 *        lattice of the type lattice will join them together in a set,
 *        representing the result as {Int, String}. The TypeSet lattice is in
 *        fact the lattice PowerSet(Type).
 *      - A product lattice, combining two lattices together as a cartesian
 *        product. Example: one could combine the type lattice with a sign
 *        lattice, getting abstract values such as (Int, +), (String, bottom),
 *        ...
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
 *      ConcreteLattice.scala, TypeLattice.scala)
 *  - Play with abstract machines, you can look into AAM.scala, AAC.scala or
 *    Free.scala (AAM is the simplest machine).
 *  - Implement some kind of analysis, you'll probably need to design a lattice
 *    that is suited for your analysis. You can use an existing lattice as an
 *    inspiration.
 */

/**
 * This is where we parse the arguments given to the implementation
 */
object Config {
  object Machine extends Enumeration {
    val AAC, AAM, Free, ConcurrentAAM = Value
  }
  implicit val machineRead: scopt.Read[Machine.Value] = scopt.Read.reads(Machine withName _)

  object Lattice extends Enumeration {
    val Concrete, Type, TypeSet = Value
  }
  implicit val latticeRead: scopt.Read[Lattice.Value] = scopt.Read.reads(Lattice withName _)

  object Language extends Enumeration {
    val Scheme, ANF, ParSimple = Value
  }
  implicit val languageRead: scopt.Read[Language.Value] = scopt.Read.reads(Language withName _)

  implicit val explorationTypeRead: scopt.Read[ExplorationType.Value] = scopt.Read.reads(ExplorationType withName _)

  trait Time {
    def nanoSeconds: Long
  }
  case class Hours(n: Long) extends Time {
    def nanoSeconds = n * 60 * 60 * Math.pow(10, 9).toLong
    override def toString = if (n == 1) "1 hour" else s"$n hours"
  }
  case class Minutes(n: Long) extends Time {
    def nanoSeconds = n * 60 * Math.pow(10, 9).toLong
    override def toString = if (n == 1) "1 minute" else s"$n minutes"
  }
  case class Seconds(n: Long) extends Time {
    def nanoSeconds = n * Math.pow(10, 9).toLong
    override def toString = if (n == 1) "1 second" else s"$n seconds"
  }
  case class Milliseconds(n: Long) extends Time {
    def nanoSeconds = n * Math.pow(10, 6).toLong
    override def toString = if (n == 1) "1 millisecond" else s"$n milliseconds"
  }
  case class Nanoseconds(nanoSeconds: Long) extends Time {
    override def toString = if (nanoSeconds == 1) "1 nanosecond" else s"$nanoSeconds nanoseconds"
  }

  object TimeParser extends scala.util.parsing.combinator.RegexParsers {
    val number = "[0-9]+".r
    def hours: Parser[Time] = (number <~ "h") ^^ ((s) => Hours(s.toLong))
    def minutes: Parser[Time] = (number <~ "min") ^^ ((s) => Minutes(s.toLong))
    def seconds: Parser[Time] = (number <~ "s") ^^ ((s) => Seconds(s.toLong))
    def milliseconds: Parser[Time] = (number <~ "ms") ^^ ((s) => Milliseconds(s.toLong))
    def nanoseconds: Parser[Time] = (number <~ "ns") ^^ ((s) => Nanoseconds(s.toLong))
    def time: Parser[Time] = hours | minutes | seconds | milliseconds | nanoseconds
    def parse(s: String): Time = parseAll(time, s) match {
      case Success(res, _) => res
      case Failure(msg, _) => throw new Exception(s"cannot parse time: $msg")
      case Error(msg, _) => throw new Exception(s"cannot parse time: $msg")
    }
  }

  implicit val timeRead: scopt.Read[Time] = scopt.Read.reads(TimeParser.parse _)

  case class Config(machine: Machine.Value = Machine.Free,
    lattice: Lattice.Value = Lattice.TypeSet, concrete: Boolean = false,
    file: Option[String] = None, dotfile: Option[String] = None,
    language: Language.Value = Language.Scheme,
    exploration: ExplorationType.Value = ExplorationType.InterferenceTracking,
    timeout: Option[Long] = None)

  val parser = new scopt.OptionParser[Config]("scala-am") {
    head("scala-am", "0.0")
    opt[Machine.Value]('m', "machine") action { (x, c) => c.copy(machine = x) } text("Abstract machine to use (AAM, AAC, Free, ConcurrentAAM)")
    opt[Lattice.Value]('l', "lattice") action { (x, c) => c.copy(lattice = x) } text("Lattice to use (Concrete, Type, TypeSet)")
    opt[Unit]('c', "concrete") action { (_, c) => c.copy(concrete = true) } text("Run in concrete mode")
    opt[String]('d', "dotfile") action { (x, c) => c.copy(dotfile = Some(x)) } text("Dot file to output graph to")
    opt[Language.Value]("language") action { (x, c) => c.copy(language = x) } text("The language to analyze")
    opt[String]('f', "file") action { (x, c) => c.copy(file = Some(x)) } text("File to read program from")
    opt[ExplorationType.Value]('e', "exploration") action { (x, c) => c.copy(exploration = x) } text("Exloration type for concurrent programs (OneInterleaving, RandomInterleaving, AllInterleavings, InterferenceTracking)")
    opt[Time]('t', "timeout") action { (x, c) => c.copy(timeout = Some(x.nanoSeconds)) } text("Timeout (none by default)")
  }
}

object Main {
  /** Run a machine on a program with the given semantics. If @param output is
    * set, generate a dot graph visualizing the computed graph in the given
    * file. */
  def run[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp](machine: AbstractMachine[Exp, Abs, Addr, Time], sem: Semantics[Exp, Abs, Addr, Time])(program: String, output: Option[String], timeout: Option[Long]): Unit = {
    val abs = implicitly[AbstractValue[Abs]]
    val addr = implicitly[Address[Addr]]
    println(s"Running ${machine.name} with lattice ${abs.name} and address ${addr.name}")
    val result = machine.eval(sem.parse(program), sem, !output.isEmpty, timeout)
    output match {
      case Some(f) => result.toDotFile(f)
      case None => ()
    }
    if (result.timedOut) println(s"${scala.io.AnsiColor.RED}Timeout was reached${scala.io.AnsiColor.RESET}")
    println(s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}")
  }

  object Done extends Exception

  def fileContent(file: String): String = {
    val f = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    content
  }

  trait TimestampWrapper {
    type T
    val isTimestamp: Timestamp[T]
  }
  trait ExpressionWrapper {
    type T
    val isExpression: Expression[T]
  }

  def main(args: Array[String]) {
    import scala.util.control.Breaks._
    Config.parser.parse(args, Config.Config()) match {
      case Some(config) if (config.language == Config.Language.Scheme) => {
        val lattice: Lattice = config.lattice match {
          case Config.Lattice.Concrete => ConcreteLattice
          case Config.Lattice.Type => TypeLattice
          case Config.Lattice.TypeSet => TypeSetLattice
        }
        implicit val isAbstractValue = lattice.isAbstractValue

        val time: TimestampWrapper = if (config.concrete) {
          new TimestampWrapper {
            type T = ConcreteTimestamp
            val isTimestamp = implicitly[Timestamp[T]]
          }
        } else {
          new TimestampWrapper {
            type T = CFA.ZeroCFA
            val isTimestamp = implicitly[Timestamp[T]]
          }
        }
        implicit val isTimestamp = time.isTimestamp

        val machine = config.machine match {
          case Config.Machine.AAM => new AAM[SchemeExp, lattice.L, ClassicalAddress, time.T]
          case Config.Machine.AAC => new AAC[SchemeExp, lattice.L, ClassicalAddress, time.T]
          case Config.Machine.Free => new Free[SchemeExp, lattice.L, ClassicalAddress, time.T]
          case Config.Machine.ConcurrentAAM => new ConcurrentAAM[SchemeExp, lattice.L, ClassicalAddress, time.T, ContextSensitiveTID](config.exploration)
        }

        val sem = if (config.machine == Config.Machine.ConcurrentAAM) {
          new ConcurrentSchemeSemantics[lattice.L, ClassicalAddress, time.T, ContextSensitiveTID]
        } else {
          new SchemeSemantics[lattice.L, ClassicalAddress, time.T]
        }

        try {
          do {
            val program = config.file match {
              case Some(file) => fileContent(file)
              case None => StdIn.readLine(">>> ")
            }
            if (program == null) throw Done
            if (program.size > 0)
              run(machine, sem)(program, config.dotfile, config.timeout)
          } while (config.file.isEmpty);
        } catch {
          case Done => ()
        }
      }
      case Some(config) if (config.language == Config.Language.ParSimple) => {
        try {
          do {
            val machine = new ConcurrentAAM[ParSimpleExp, ConcreteLattice.L, ClassicalAddress, CFA.ZeroCFA, ContextSensitiveTID](config.exploration)
            val sem = new ParSimpleSemantics[ConcreteLattice.L, ClassicalAddress, CFA.ZeroCFA, ContextSensitiveTID]
            val program = config.file match {
              case Some(file) => fileContent(file)
              case None => StdIn.readLine(">>> ")
            }
            if (program == null) throw Done
            if (program.size > 0)
              run(machine, sem)(program, config.dotfile, config.timeout)
          } while (config.file.isEmpty);
        } catch {
          case Done => ()
        }
      }
      case None => ()
    }
  }
}
