import scala.io.StdIn
import scala.concurrent.duration.Duration

object Util {
  def fileContent(file: String): String = {
    val f = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    content
  }

  def writeToFile(path: String, content: String): Unit = {
    val f = new java.io.File(path)
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
    bw.write(content)
    bw.close()
  }


  object Done extends Exception
  /** Either run cb on the content of the given file, or run a REPL, each line being sent to cb */
  def replOrFile(file: Option[String], cb: String => Unit): Unit = {
    lazy val reader = new jline.console.ConsoleReader()
    @scala.annotation.tailrec
    def loop(): Unit = {
      val program = reader.readLine(">>> ")
      if (program != null) {
        if (program.size > 0) {
          cb(program)
        }
        loop()
      }
    }
    file match {
      case Some(file) => cb(fileContent(file))
      case None => loop()
    }
  }

  def timeoutReached(timeout: Option[Long], startingTime: Long): Boolean =
    timeout.map(System.nanoTime - startingTime > _).getOrElse(false)
  def timeElapsed(startingTime: Long): Double = (System.nanoTime - startingTime) / Math.pow(10, 9)

  /**
   * This is where we parse the arguments given to the implementation
   */
  object Config {
    object Machine extends Enumeration {
      val AAC, AAM, AAMGlobalStore, Free, ConcreteMachine = Value
    }
    implicit val machineRead: scopt.Read[Machine.Value] = scopt.Read.reads(Machine withName _)

    object Lattice extends Enumeration {
      val Concrete, TypeSet, BoundedInt, ConstantPropagation = Value
    }
    implicit val latticeRead: scopt.Read[Lattice.Value] = scopt.Read.reads(Lattice withName _)

    object Language extends Enumeration {
      val Scheme, AScheme, CScheme = Value
    }
    implicit val languageRead: scopt.Read[Language.Value] = scopt.Read.reads(Language withName _)

    object Address extends Enumeration {
      val Classical, ValueSensitive = Value
    }
    implicit val addressRead: scopt.Read[Address.Value] = scopt.Read.reads(Address withName _)

    case class Config(machine: Machine.Value = Machine.Free,
      language: Language.Value = Language.Scheme,
      lattice: Lattice.Value = Lattice.TypeSet, concrete: Boolean = false,
      file: Option[String] = None, dotfile: Option[String] = None, jsonfile: Option[String] = None,
      address: Address.Value = Address.Classical,
      inspect: Boolean = false,
      counting: Boolean = false,
      bound: Int = 100,
      timeout: Option[Duration] = None,
      workers: Int = 1)

    private val separator = ", "
    val parser = new scopt.OptionParser[Config]("scala-am") {
      head("scala-am", "0.0")
      opt[Machine.Value]('m', "machine") action { (x, c) => c.copy(machine = x) } text(s"Abstract machine to use (${Machine.values.mkString(separator)})")
      opt[Lattice.Value]('l', "lattice") action { (x, c) => c.copy(lattice = x) } text(s"Lattice to use (${Lattice.values.mkString(separator)})")
      opt[Language.Value]("lang") action { (x, c) => c.copy(language = x) } text(s"Language to analyze (${Language.values.mkString(separator)})")
      opt[Unit]('c', "concrete") action { (_, c) => c.copy(concrete = true) } text("Run in concrete mode")
      opt[String]('d', "dotfile") action { (x, c) => c.copy(dotfile = Some(x)) } text("Dot file to output graph to")
      opt[String]('j', "jsonfile") action { (x, c) => c.copy(jsonfile = Some(x)) } text("JSON file to output graph to")
      opt[String]('f', "file") action { (x, c) => c.copy(file = Some(x)) } text("File to read program from")
      opt[Duration]('t', "timeout") action { (x, c) => c.copy(timeout = if (x.isFinite) Some(x) else None) } text("Timeout (none by default)")
      opt[Unit]('i', "inspect") action { (x, c) => c.copy(inspect = true) } text("Launch inspection REPL (disabled by default)")
      opt[Address.Value]('a', "address") action { (x, c) => c.copy(address = x) } text(s"Addresses to use (${Address.values.mkString(separator)})")
      opt[Unit]("counting") action { (x, c) => c.copy(counting = true) } text("Use absstract counting (on for concrete lattices)")
      opt[Int]('b', "bound") action { (x, c) => c.copy(bound = x) } text("Bound for bounded lattice (defaults to 100)")
      opt[Int]('w', "workers") action { (x, c) => c.copy(workers = x) } text("Number of workers (defaults to 1)")
    }
  }

  /* From http://stackoverflow.com/questions/7539831/scala-draw-table-to-console */
  object Tabulator {
    def format(table: Seq[Seq[Any]]) = table match {
      case Seq() => ""
      case _ =>
        val sizes = for (row <- table) yield (for (cell <- row) yield if (cell == null) 0 else cell.toString.length)
        val colSizes = for (col <- sizes.transpose) yield col.max
        val rows = for (row <- table) yield formatRow(row, colSizes)
        formatRows(rowSeparator(colSizes), rows)
    }

    def formatRows(rowSeparator: String, rows: Seq[String]): String = (
      rowSeparator ::
        rows.head ::
        rowSeparator ::
        rows.tail.toList :::
        rowSeparator ::
        List()).mkString("\n")

    def formatRow(row: Seq[Any], colSizes: Seq[Int]) = {
      val cells = (for ((item, size) <- row.zip(colSizes)) yield if (size == 0) "" else ("%" + size + "s").format(item))
      cells.mkString("|", "|", "|")
    }

    def rowSeparator(colSizes: Seq[Int]) = colSizes map { "-" * _ } mkString("+", "+", "+")
  }
}
