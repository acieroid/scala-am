package scalaam.cli

import incremental.ModuleDifferencer
import scalaam.core.Identity
import scalaam.core.Identity.Position
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = """
      (define lst '(1 2 3))
      (define p `(,@lst 4 5))
      (length p)
    """
    val prg = SchemeParser.parse(txt)
    val analysis = new IncrementalModAnalysis(prg) with IncrementalSchemeModFSemantics
                                                   with BigStepSemantics
                                                   with ConstantPropagationDomain
                                                   with NoSensitivity
    analysis.analyze()
    debugResults(analysis)
  }

  type SchemeModFAnalysis = ModAnalysis[SchemeExp] with SchemeModFSemantics

  def debugResults(machine: SchemeModFAnalysis): Unit = {
    machine.store.foreach {
      case (machine.ReturnAddr(cmp),result) =>
        println(s"$cmp => $result")
      case _ =>
    }
  }
}

object Incrementor extends App {

  type Analysis = IncrementalModAnalysis[SchemeExp] with SmallStepSemantics with ConstantPropagationDomain with NoSensitivity with IncrementalSchemeModFSemantics
  var analyzer: Analysis = _

  def analyze(file: String): Unit = analyze(SchemeParser.parse(FileUtil.loadFile(file)))

  private def analyze(text: SchemeExp): Unit  = {
    analyzer = new IncrementalModAnalysis(text) with SmallStepSemantics
                                                    with ConstantPropagationDomain
                                                    with NoSensitivity
                                                    with IncrementalSchemeModFSemantics
    analyzer.analyze()//Timeout.start(Duration(2, "MINUTES")))
    println(s"Number of components: ${analyzer.allComponents.size}")
  }

  def reanalyse(text: SchemeExp): Unit = analyzer.updateAnalysis(text)

  val a = ModuleDifferencer.inferModules(SchemeParser.parse(FileUtil.loadFile("./test/ad/inssort.scm")))
  val b = ModuleDifferencer.inferModules(SchemeParser.parse(FileUtil.loadFile("./test/ad/inssort.scm")))
  println(ModuleDifferencer.mapModules(a, b).map{ case (a, b) => (a.name, b.name) })

}

object FileUtil {

  def loadFile(file: String): String = {
    val fHandle = scala.io.Source.fromFile(file)
    val content = fHandle.getLines.mkString("\n")
    fHandle.close()
    content
  }

}
