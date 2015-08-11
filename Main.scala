import AbstractValue._
import Address._

object Main {
  def fileContent(path: String): String = {
    val f = scala.io.Source.fromFile(path)
    val content = f.getLines.mkString("\n")
    f.close()
    println(content)
    content
  }

  def runScheme[Abs, Addr](file: String, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    /*val p = ANF.compile(Scheme.rename(Scheme.compile(SExpParser.parse(fileContent(file)))))
    var aac = new AAC[Abs, Addr, ANFExp](new ANFSemantics[Abs, Addr]) */
    val p = Scheme.compile(SExpParser.parse(fileContent(file)))
    var aac = new AAC[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])
    println(aac.eval(p, output))
  }

  def main(args: Array[String]) {
    if (args.length == 1) {
      runScheme[AbstractTypeSet, ClassicalAddress](args(0), None)
    } else if (args.length == 2) {
      runScheme[AbstractTypeSet, ClassicalAddress](args(0), Some(args(1)))
    } else {
      println("Scheme file expected as argument")
    }
  }
}
