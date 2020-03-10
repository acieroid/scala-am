package scalaam.io

import java.io.{BufferedWriter, FileWriter}
import java.text.SimpleDateFormat
import java.util.Calendar

import au.com.bytecode.opencsv.CSVWriter

object FileReader {

  def loadFile(file: String): String = {
    val fHandle = scala.io.Source.fromFile(file)
    val content = fHandle.getLines.mkString("\n")
    fHandle.close()
    content
  }

}

object FileWriter {

  type Writer = CSVWriter

  val  calendar: Calendar         = Calendar.getInstance()
  val    format: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd-HH'h'mm")

  def open(path: String): Writer = {
    new CSVWriter(new BufferedWriter(new FileWriter(path)))
  }

  def openTimeStamped(dir: String, file: String): Writer =
    open(dir + format.format(calendar.getTime) + "_" + file)

  def close(writer: Writer): Unit = writer.close()

  // Avoid output being buffered.
  def write(data: String, writer: Writer): String = {
    print(data)
    Console.out.flush()
    writer.writeNext(data.trim)
    writer.flush()
    data
  }
  def writeErr(data: String, writer: CSVWriter): String = {
    System.err.print(data)
    System.err.flush()
    writer.writeNext(data.trim)
    writer.flush()
    data
  }
}
