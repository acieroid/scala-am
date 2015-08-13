import org.scalatest._
import org.scalatest.prop._

class ParserSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val files = Table(
    "file",
    "blur.scm",
    "church.scm",
    "count.scm",
    "cpstak.scm",
    "eta.scm",
    "fact.scm",
    "fib.scm",
    "gcipd.scm",
    "inc.scm",
    "infinite-1.scm",
    "infinite-2.scm",
    "kcfa2.scm",
    "kcfa3.scm",
    "letrec-begin.scm",
    "loop2.scm",
    "mj09.scm",
    "mut-rec.scm",
    "rotate.scm",
    "sq.scm",
    "sym.scm",
    "widen.scm"
    )

  property("parser should parse without error") {
    forAll (files) { (file: String) =>
      val f = scala.io.Source.fromFile("test/" + file)
      val content = f.getLines.mkString("\n")
      f.close()
      Scheme.compile(SExpParser.parse(content))
    }
  }
}
