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
import scalaam.language.scheme.primitives.SchemePrelude

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = Reader.loadFile("test/gambit/nqueens.scm")
    val prg = SchemeParser.parse(txt)
    val analysis = new AdaptiveModAnalysis(prg) with AdaptiveSchemeModFSemantics
                                                with AdaptiveCallerSensitivity
                                                with ConstantPropagationDomain {
      val limit = 10
      override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
      override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
      override def step(): Unit = {
        //println(allComponents.size)
        super.step()
      }
    }
    analysis.analyze(Timeout.start(Duration(30,SECONDS)))
    debugClosures(analysis)
    //debugResults(analysis, false)
  }

  type SchemeModFAnalysis = ModAnalysis[SchemeExp] with SchemeModFSemantics

  def debugClosures(analysis: SchemeModFAnalysis): Unit = {
    def getClosure(cmp: analysis.Component) = analysis.view(cmp) match {
      case _: analysis.MainComponent => None
      case call: analysis.CallComponent => Some(call.clo)
    }
    def collect() = {
      val allClosures = analysis.allComponents.flatMap(cmp => getClosure(cmp))
      val children = allClosures.foldLeft(Map[Option[analysis.lattice.Closure],Set[analysis.lattice.Closure]]()) { (acc,clo) =>
        val parent = getClosure(clo._2)
        acc + (parent -> (acc.getOrElse(parent, Set()) + clo))
      } 
      children
    }
    def printClosures(children: Map[Option[analysis.lattice.Closure],Set[analysis.lattice.Closure]]) = {
      def display(clo: analysis.lattice.Closure): String =
        s"${clo._1.idn} [${clo._2}]"
      def printClosure(clo: Option[analysis.lattice.Closure], indent: Int): Unit = {
        val str = clo.map(display(_)).getOrElse("main")
        print(" " * indent) // print indentation
        println(s"- $str")  // print the closure
        children.getOrElse(clo,Set()).foreach { child =>  // print its children
          printClosure(Some(child), indent + 2)
        }
      }
      printClosure(None, 0)
    }
    printClosures(collect())
  }

  def debugResults(machine: SchemeModFAnalysis, printMore: Boolean = false): Unit =
    machine.store.foreach {
      case (machine.ReturnAddr(cmp),result) => //if cmp == machine.initialComponent =>
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

object Run extends App {
  val text = Reader.loadFile("test/WeiChenRompf2019/meta-circ.scm")
  val interpreter = new SchemeInterpreter((_, _) => (), true)
  val res = interpreter.run(SchemeUndefiner.undefine(List(SchemePrelude.addPrelude(SchemeParser.parse(text), Set("newline", "display")))), Timeout.none)
  println(res)
}

object Analyze extends App {
  val text = Reader.loadFile("./test/icp/icp_1c_multiple-dwelling.scm") //"""(define result '())
             //  |(define display (lambda (i) (set! result (cons i result))))
             //  |(define (foo x)
             //  |  (display 1))
             //  |(define result2 '())
             //  |(define display2 (lambda (i) (set! result2 (cons i result2))))
             //  |(define (bar x)
             //  |  (display2 (+ x 1)))
             //  |(foo 5)
             //  |(bar 6)
             //  |result2""".stripMargin
  val a = new ModAnalysis(SchemeParser.parse(text)) with SmallStepSemantics with ConstantPropagationDomain with NoSensitivity with StandardSchemeModFSemantics
  a.analyze(Timeout.none)
  val r = a.store(a.ReturnAddr(a.initialComponent))
  println(r)
}