import scala.io.StdIn

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
