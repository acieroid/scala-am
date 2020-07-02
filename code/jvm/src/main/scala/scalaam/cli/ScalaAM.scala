package scalaam.cli

import scalaam.cli.benchmarks.SchemeBenchmarks
import scalaam.core.Position._
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

import scala.concurrent.duration._

object Main {

  def main(args: Array[String]): Unit = test()

  def test(): Unit = {
    val txt = Reader.loadFile("test/R5RS/fact.scm")
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
      case (ComponentAddr(cmp, ReturnAddr), result) if cmp == machine.initialComponent || printMore =>
        println(s"$cmp => $result")
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


object Run extends App {
  val text = Reader.loadFile("test/concurrentScheme/threads/abp.scm")
  val interpreter = new SchemeInterpreter((_, _) => (), true)
  val res = interpreter.run(CSchemeUndefiner.undefine(List(SchemePrelude.addPrelude(CSchemeParser.parse(text), Set("newline", "display")))), Timeout.none)
  println(res)
}

object Analyze extends App {
  def one(bench: String, timeout: () => Timeout.T): Unit = {
    val text = CSchemeParser.parse(Reader.loadFile(bench))
    val a = new ModAnalysis(text)
      with KAExpressionContext
      with ModConcConstantPropagationDomain
      with LIFOWorklistAlgorithm[SchemeExp] {}
    a.analyze(timeout())
    val r = a.store(ComponentAddr(a.initialComponent, ReturnAddr))
    println(r)
  }

  val bench: List[String] = List(
    "test/concurrentScheme/threads/actors.scm",
    "test/concurrentScheme/threads/atoms.scm",
    "test/concurrentScheme/threads/bchain.scm",
    "test/concurrentScheme/threads/count.scm",
    "test/concurrentScheme/threads/crypt.scm",
    "test/concurrentScheme/threads/dekker.scm",
    "test/concurrentScheme/threads/fact-indep.scm",
    "test/concurrentScheme/threads/fact.scm",
    "test/concurrentScheme/threads/fact2.scm",
    //"test/concurrentScheme/threads/lastzero2.scm", // Uses let*, but should use something like letrec*?
    "test/concurrentScheme/threads/life.scm",
    "test/concurrentScheme/threads/matmul.scm", // StackOverflow in concrete.
    "test/concurrentScheme/threads/mcarlo.scm", // StackOverFlow in concrete.
    "test/concurrentScheme/threads/mceval.scm",
    "test/concurrentScheme/threads/minimax.scm",
    "test/concurrentScheme/threads/msort.scm", // Out of Memory error.
    "test/concurrentScheme/threads/nbody.scm",
    "test/concurrentScheme/threads/pc.scm",
    "test/concurrentScheme/threads/peterson.scm",
    "test/concurrentScheme/threads/phil.scm",
    "test/concurrentScheme/threads/phild.scm",
    "test/concurrentScheme/threads/philosophers2.scm",
    "test/concurrentScheme/threads/pp.scm",
    "test/concurrentScheme/threads/pps.scm",
    "test/concurrentScheme/threads/producer.scm",
    "test/concurrentScheme/threads/qsort.scm",
    "test/concurrentScheme/threads/randomness.scm",
    "test/concurrentScheme/threads/randomness2.scm",
    "test/concurrentScheme/threads/readers2.scm",
    "test/concurrentScheme/threads/ringbuf.scm",
    "test/concurrentScheme/threads/rng.scm",
    "test/concurrentScheme/threads/sieve.scm",
    "test/concurrentScheme/threads/simple.scm",
    "test/concurrentScheme/threads/stm.scm",
    "test/concurrentScheme/threads/sudoku.scm",
    "test/concurrentScheme/threads/trapr.scm",
    "test/concurrentScheme/threads/tsp.scm",
  )

  List("test/concurrentScheme/threads/abp.scm").foreach({b =>
    try {
      print(b + " => ")
      val t0 = System.currentTimeMillis()
      one(b, () => Timeout.start(2, MINUTES))
      val t1 = System.currentTimeMillis()
      println(s"    in ${(t1 - t0)}ms")
    } catch {
      case _: Throwable => println("    Raised exception.")
    }
  })

}

