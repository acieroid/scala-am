package scalaam.cli

import scalaam.language.CScheme._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives.SchemePrelude
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.modular.scheme.modconc._
import scalaam.modular.scheme.ssmodconc._
import scalaam.util.Reader
import scalaam.util.benchmarks.Timeout
import scalaam.language.change.CodeVersion._
import scalaam.modular.incremental.scheme.IncrementalSimpleSchemeModFAnalysis

import scala.concurrent.duration._

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = Reader.loadFile("test/R5RS/mceval.scm")
    val prg = SchemeParser.parse(txt)
    val analysis = new SimpleSchemeModConcAnalysis(prg)
                                        with SchemeModConcNoSensitivity
                                        with SchemeConstantPropagationDomain
                                        with LIFOWorklistAlgorithm[SchemeExp] {
      var i = 0
      override def step(t: Timeout.T = Timeout.none): Unit = {
        i = i + 1
        val cmp = workList.head
        println(s"[$i] $cmp")
        super.step(t)
      }
      def modFAnalysis(intra: SchemeModConcIntra) = new InnerModFAnalysis(intra)
                                                        with SchemeModFNoSensitivity
                                                        with RandomWorklistAlgorithm[SchemeExp] {
        var j = 0
        override def step(t: Timeout.T): Unit = {
          j = j + 1
          val cmp = workList.head
          println(s"[$i.$j] $cmp")
          super.step(t)
        } 
      }
    }
    analysis.analyze(Timeout.start(Duration(3,SECONDS)))
    //debugClosures(analysis)
    debugResults(analysis, true)
  }

  def debugResults(machine: SchemeSemantics, printMore: Boolean = false): Unit =
    machine.store.foreach {
      case (ComponentAddr(cmp, addr: ReturnAddr), result) if cmp == machine.initialComponent || printMore =>
        println(s"$cmp => $result")
      case _ => ()
    }
}

object Run extends App {
  val text = Reader.loadFile("test/changes/ring-rotate.scm")
  val interpreter = new SchemeInterpreter((_, _) => (), true)
  val res = interpreter.run(CSchemeUndefiner.undefine(List(SchemePrelude.addPrelude(CSchemeParser.parse(text), Set("newline", "display")))), Timeout.none, New)
  println(res)
}

object Analyze extends App {
  def one(bench: String, timeout: () => Timeout.T): Unit = {
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a = new ModAnalysis(text)
      with OneCFAModConc
      with SchemeConstantPropagationDomain
      with LIFOWorklistAlgorithm[SchemeExp] {}
    a.analyze(timeout())
    val r = a.store(ComponentAddr(a.initialComponent, ReturnAddr(a.expr(a.initialComponent))))
    println(r)
  }

  val bench: List[String] = List(
  "test/concurrentScheme/threads/msort.scm",
  "test/concurrentScheme/threads/qsort.scm",
  "test/concurrentScheme/threads/tsp.scm",
  )

  bench.foreach({b =>
    try {
      print(b + " => ")
      val t0 = System.currentTimeMillis()
      one(b, () => Timeout.start(2, MINUTES))
      val t1 = System.currentTimeMillis()
      println(s"    in ${(t1 - t0)}ms")
    } catch {
      case t: Throwable => println(s"Raised exception.")
        System.err.println(t.getMessage)
        t.printStackTrace()//t.getStackTrace.take(10).foreach(System.err.println)
        System.err.flush()
    }
  })

}