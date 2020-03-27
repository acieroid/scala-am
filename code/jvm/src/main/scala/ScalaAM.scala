/*package scalaam.cli


import scalaam.diff.ModuleInferencer
import scalaam.incremental._
import scalaam.io.Reader
import scalaam.modular.incremental.IncrementalModAnalysis
import scalaam.modular.incremental.scheme.IncrementalSchemeModFSemantics
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = { /*
    val txt = FileUtil.loadFile("test/primtest.scm")
    val prg = SchemeParser.parse(txt)
    val analysis = new AdaptiveModAnalysis(prg)
                                          with AdaptiveSchemeModFSemantics
                                          with BigStepSemantics
                                          with AdaptiveConstantPropagationDomain {
      val limit = 5
      override def alphaValue(v: Value) = super.alphaValue(v)
    }
    analysis.analyze()
    debugResults(analysis) */
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

  def analyze(file: String): Unit = analyze(SchemeParser.parse(Reader.loadFile(file)))

  private def analyze(text: SchemeExp): Unit = {
    analyzer = new IncrementalModAnalysis(text) with SmallStepSemantics
                                                    with ConstantPropagationDomain
                                                    with NoSensitivity
        with IncrementalSchemeModFSemantics {
      val primitives = ??? // TODO
    }
    analyzer.analyze()//Timeout.start(Duration(2, "MINUTES")))
    println(s"Number of components: ${analyzer.allComponents.size}")
  }

  def reanalyse(text: SchemeExp): Unit = analyzer.updateAnalysis(text)

  val a = ModuleInferencer.inferModules(SchemeParser.parse(Reader.loadFile("./test/ad/inssort.scm")))
  val b = ModuleInferencer.inferModules(SchemeParser.parse(Reader.loadFile("./test/ad/inssort2.scm")))
  println(a)
  println(b)
  val mapping = GumtreeModuleDiff.computeMapping(a, b)
  mapping.map(println)

}*/
