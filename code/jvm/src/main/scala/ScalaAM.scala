package scalaam.cli

import scalaam.util._
import scala.concurrent.duration._
import scalaam.diff.ModuleInferencer
import scalaam.incremental._
import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import scalaam.modular.incremental._
import scalaam.modular.incremental.scheme._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = FileUtil.loadFile("test/my-test.scm")
    val prg = SchemeParser.parse(txt)
    val analysis = new AdaptiveModAnalysis(prg) with AdaptiveSchemeModFSemantics
                                                with AdaptiveArgumentSensitivityPolicyAlt1
                                                with ConstantPropagationDomain {
      val limit = 4
      override def allocCtx(clo: lattice.Closure, args: List[Value]) = super.allocCtx(clo,args)
      override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
      override def step(): Unit = {
        println(s"analysing ${view(work.head)}")
        super.step()
      }
    }
    analysis.analyze(Timeout.start(Duration(5,SECONDS)))
    debugResults(analysis)
  }

  type SchemeModFAnalysis = ModAnalysis[SchemeExp] with SchemeModFSemantics

  def debugResults(machine: AdaptiveSchemeModFSemantics): Unit =
    machine.store.foreach {
      case (machine.ReturnAddr(cmp),result) =>
        println(s"[$cmp] ${machine.view(cmp)} => $result")
      case _ =>
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

  val a = ModuleInferencer.inferModules(SchemeParser.parse(FileUtil.loadFile("./test/ad/inssort.scm")))
  val b = ModuleInferencer.inferModules(SchemeParser.parse(FileUtil.loadFile("./test/ad/inssort2.scm")))
  println(a)
  println(b)
  val mapping = GumtreeModuleDiff.computeMapping(a, b)
  mapping.map(println)

}

object FileUtil {

  def loadFile(file: String): String = {
    val fHandle = scala.io.Source.fromFile(file)
    val content = fHandle.getLines.mkString("\n")
    fHandle.close()
    content
  }
}
