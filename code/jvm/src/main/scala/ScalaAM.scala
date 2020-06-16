package scalaam.cli

import scalaam.util._

import scala.concurrent.duration._
import scalaam.core.Position._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._
import scalaam.util.benchmarks.Timeout

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = Reader.loadFile("test/icp/icp_7_eceval.scm")
    val prg = SchemeParser.parse(txt)
    val analysis = new ModAnalysis(prg) with StandardSchemeModFSemantics 
                                        with BigStepModFSemantics
                                        with ParallelWorklistAlgorithm[SchemeExp]
                                        with NoSensitivity
                                        with ConstantPropagationDomain {
      override def workers = 4
      override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
      override def intraAnalysis(cmp: Component) = new BigStepModFIntra(cmp) with ParallelIntra
      /* 
      var i = 0
      override def step(): Unit = {
        i = i + 1
        val cmp = workList.head
        println(s"[$i] $cmp")
        super.step()
      }
      */     
    }
    analysis.analyze(Timeout.start(Duration(3600,SECONDS)))
    //debugClosures(analysis)
    debugResults(analysis, false)
  }

  type SchemeModFAnalysis = ModAnalysis[SchemeExp] with StandardSchemeModFComponents

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

/*
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
*/

/*
object Run extends App {
  val text = Reader.loadFile("test/SETL/testcase1.scm")
  val interpreter = new SchemeInterpreter((_, _) => (), true)
  val res = interpreter.run(SchemeUndefiner.undefine(List(SchemePrelude.addPrelude(SchemeParser.parse(text), Set("newline", "display")))), Timeout.none)
  println(res)
}

object Analyze extends App {
  val text = Reader.loadFile("test/SETL/testcase1.scm")
  val a = new ModAnalysis(SchemeParser.parse(text)) with SmallStepSemantics with ConstantPropagationDomain with NoSensitivity with StandardSchemeModFSemantics
  a.analyze(Timeout.none)
  val r = a.store(a.ReturnAddr(a.initialComponent))
  println(r)
}
*/