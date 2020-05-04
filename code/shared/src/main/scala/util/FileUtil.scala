package scalaam.util

import java.io.{BufferedWriter, FileWriter}
import java.text.SimpleDateFormat
import java.util.Calendar

object Reader {

  def loadFile(file: String): String = {
    val fHandle = scala.io.Source.fromFile(file)
    val content = fHandle.getLines.mkString("\n")
    fHandle.close()
    content
  }

}

/**
 * Utility to write to plain text files. Can also be modifief to write to csv files using
 * a CSVWriter, CSVWriter.NO_QUOTE_CHARACTER and the writeNext method.
 */
object Writer {

  type Writer = BufferedWriter

  val  calendar: Calendar         = Calendar.getInstance()
  val    format: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd-HH'h'mm")

  var defaultWriter: Writer = _

  def open(path: String): Writer = {
    new BufferedWriter(new FileWriter(path))
  }

  def openTimeStamped(dir: String, file: String): Writer =
    open(dir + format.format(calendar.getTime) + "_" + file)

  def close(writer: Writer): Unit = writer.close()

  def setDefaultWriter(writer: Writer): Unit = defaultWriter = writer
  def closeDefaultWriter(): Unit = defaultWriter.close()

  // Avoid output being buffered.
  def write(writer: Writer, data: String): String = {
    print(data)
    Console.out.flush()
    writer.write(data)
    writer.flush()
    data
  }

  def writeln(writer: Writer, data: String): String = write(writer, data + "\n")

  def write(data: String = "\n"): String = write(defaultWriter, data)
  def writeln(data: String = "\n"): String = writeln(defaultWriter, data)

  def writeErr(writer: Writer, data: String): String = {
    System.err.print(data)
    System.err.flush()
    writer.write(data)
    writer.flush()
    data
  }

  def writeErrln(writer: Writer, data: String): String = writeErr(writer, data + "\n")

  def writeErr(data: String = "\n"): String = writeErr(defaultWriter, data)
  def writeErrln(data: String = "\n"): String = writeErrln(defaultWriter, data)

}
