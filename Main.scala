import AbstractValue._

object Main {
  def fileContent(path: String): String = {
    val f = scala.io.Source.fromFile(path)
    val content = f.getLines.mkString("\n")
    f.close()
    println(content)
    content
  }

  import AbstractType._
  def test[A]()(implicit spec: AbstractValue[A], i: AbstractInjection[A]) = {
    val v1 = i.inject(1)
    val v2 = i.inject("foo")
  }

  def main(args: Array[String]) {
    test()
  }
}
