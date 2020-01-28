package scalaam.cli

import scalaam.incremental.ModuleDifferencer
import scalaam.modular.adaptive.AdaptiveModAnalysis
import scalaam.modular.adaptive.scheme.AdaptiveSchemeModFSemantics
import scalaam.modular.incremental.IncrementalModAnalysis
import scalaam.modular.incremental.scheme.IncrementalSchemeModFSemantics
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = FileUtil.loadFile("test/mceval.scm")
    val prg = SchemeParser.parse(txt)
    val analysis = new AdaptiveModAnalysis(prg)
                                          with AdaptiveSchemeModFSemantics
                                          with BigStepSemantics
                                          with AdaptiveConstantPropagationDomain
                                          with SimpleAdaptiveArgumentSensitivity {
      val limit = 5
      override def alphaValue(v: Value) = super.alphaValue(v)
    }
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

  private def analyze(text: SchemeExp): Unit = {
    analyzer = new IncrementalModAnalysis(text) with SmallStepSemantics
                                                    with ConstantPropagationDomain
                                                    with NoSensitivity
                                                    with IncrementalSchemeModFSemantics
    analyzer.analyze()//Timeout.start(Duration(2, "MINUTES")))
    println(s"Number of components: ${analyzer.allComponents.size}")
  }

  def reanalyse(text: SchemeExp): Unit = analyzer.updateAnalysis(text)

  val a = ModuleDifferencer.inferModules(SchemeParser.parse(FileUtil.loadFile("./test/ad/inssort.scm")))
  val b = ModuleDifferencer.inferModules(SchemeParser.parse(FileUtil.loadFile("./test/ad/inssort2.scm")))
  println(a)
  println(b)
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
