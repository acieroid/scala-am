package scalaam.cli

import scalaam.util._
import scala.concurrent.duration._
import scalaam.diff.ModuleInferencer
import scalaam.core.Position._
import scalaam.incremental._
import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import scalaam.modular.incremental._
import scalaam.modular.incremental.scheme._
import scalaam.io.Reader
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = Reader.loadFile("test/mceval.scm")
    val prg = SchemeParser.parse(txt)
    val analysis = new AdaptiveModAnalysis(prg) with AdaptiveSchemeModFSemantics
                                                with AdaptiveArgumentSensitivityPolicy3
                                                with ConstantPropagationDomain {
      val limit = 10
      override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
      override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
      override def step(): Unit = {
        //println(allComponents.size)
        super.step()
      }
    }
    analysis.analyze()
    debugResults(analysis, false)
  }

  type SchemeModFAnalysis = ModAnalysis[SchemeExp] with SchemeModFSemantics

  def debugResults(machine: SchemeModFAnalysis, printMore: Boolean = false): Unit =
    machine.store.foreach {
      case (machine.ReturnAddr(cmp),result) if cmp == machine.initialComponent =>
        println(s"[$cmp] ${machine.view(cmp)} => $result")
      case (machine.ComponentAddr(_, _: machine.PrmAddr),_) => 
        () //don't print primitive addresses
      case (addr,value) if printMore =>
        println(s"$addr => $value")
      case _ => ()
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

}
