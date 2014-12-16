object Main {
  def fileContent(path: String): String = {
    val f = scala.io.Source.fromFile(path)
    val content = f.getLines.mkString("\n")
    f.close()
    println(content)
    content
  }

  def main(args: Array[String]) {
    if (args.length != 1) {
      println("Scheme file expected as argument")
    } else {
      // val exp = SExpParser.parse(fileContent(args(0)))
      val exp = SExpParser.parse("(letrec ((count (lambda (n) (if (= n 0) 123 (count (- n 1)))))) (count 8))")
      println(exp)
      println(Scheme.compile(exp))
      println(Scheme.rename(Scheme.compile(exp)))
      println(ANF.compile(Scheme.rename(Scheme.compile(exp))))
      println(AAM.eval(ANF.compile(Scheme.rename(Scheme.compile(exp)))))
    }
  }
}
