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

  def runANF[Abs, Addr]()(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    var aam = new AAM[Abs, Addr, ANFExp](new ANFSemantics[Abs, Addr])
    println(aam.eval(ANFFuncall(ANFIdentifier("+"), List(ANFValue(ValueInteger(1)))), Some("foo.dot")))
  }

  def main(args: Array[String]) {
    runANF[AbstractType, ClassicalAddress]()
  }
}
