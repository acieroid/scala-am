package scalaam.modular

import scalaam.language.scheme._

object MainModular {

  def main(args: Array[String]): Unit = {
    val program = loadSchemeFile(s"test/${args(0)}.scm")
    val analysis = new SchemeModFAnalysis(program)
    analysis.analyze()
    analysis.results.foreach { case(cmp,value) => println(s"$cmp\t -> $value") }
  }

  def loadSchemeFile(file: String): SchemeExp = {
    val f       = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    SchemeParser.parse(content)
  }
}
