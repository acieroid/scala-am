package scalaam.test.modular.scheme

import scalaam.language.scheme._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.modular.scheme.modconc._
import scalaam.modular.scheme.ssmodconc._
import scalaam.test._

trait SchemeModConcSoundnessTests extends SchemeSoundnessTests {
  override def testTags(b: Benchmark) = super.testTags(b) :+ SchemeModConcTest
  override def concreteRuns(b: Benchmark) =
    if (SchemeBenchmarks.threads.contains(b)) {
        // run concurrent benchmarks more than once in the concrete to get
        5
    } else {
        // other (sequential) benchmarks are just ran once
        1
    }
}

trait SimpleSchemeModConc extends SchemeModConcSoundnessTests {
  override def testTags(b: Benchmark) = super.testTags(b) :+ BigStepTest
  def name = "ModConc + big-step ModF"
    def analysis(program: SchemeExp) = new SimpleSchemeModConcAnalysis(program) 
                                        with SchemeModConcNoSensitivity
                                        with SchemeConstantPropagationDomain
                                        with LIFOWorklistAlgorithm[SchemeExp] {
        def modFAnalysis(intra: SchemeModConcIntra) = new InnerModFAnalysis(intra)
                                                        with SchemeModFNoSensitivity
                                                        with RandomWorklistAlgorithm[SchemeExp]                                       
    }
}

class SimpleSchemeModConcSoundnessTests extends SimpleSchemeModConc with ThreadBenchmarks
                                                                    with SimpleBenchmarks {
  override def isSlow(b: Benchmark): Boolean =
    SchemeBenchmarks.sequentialBenchmarks.contains(b) ||
      Set("test/concurrentScheme/threads/actors.scm",
        "test/concurrentScheme/threads/fact.scm",
        "test/concurrentScheme/threads/life.scm",
        "test/concurrentScheme/threads/matmul.scm",
        "test/concurrentScheme/threads/mazefun.scm",
        "test/concurrentScheme/threads/mceval.scm",
        "test/concurrentScheme/threads/minimax.scm",
        "test/concurrentScheme/threads/msort.scm",
        "test/concurrentScheme/threads/qsort.scm",
        "test/concurrentScheme/threads/tsp.scm")(b)
}

trait SmallStepSchemeModConc extends SchemeModConcSoundnessTests {
  override def testTags(b: Benchmark) = super.testTags(b) :+ SmallStepTest
  def name = "small-step ModConc"
  def analysis(program: SchemeExp): Analysis = new ModAnalysis(program)
    with kCFAModConc
    with SchemeConstantPropagationDomain
    with LIFOWorklistAlgorithm[SchemeExp] {
    val k = 1
    override def intraAnalysis(component: SchemeComponent): IntraAnalysis = new IntraAnalysis(component) with SmallStepIntra with kCFAIntra
  }
}

class SmallStepSchemeModConcSoundnessTests extends SmallStepSchemeModConc with ThreadBenchmarks
                                                                          with SequentialBenchmarks {
  override def isSlow(b: Benchmark): Boolean =
    // (SchemeBenchmarks.sequentialBenchmarks.contains(b) && !SchemeBenchmarks.other.contains(b)) ||
    SchemeBenchmarks.sequentialBenchmarks.contains(b) ||
    // These tests currently slow down the test suite too much (time out on Bertha in 3 minutes):
      Set("test/concurrentScheme/threads/actors.scm",
          "test/concurrentScheme/threads/fact.scm",
          "test/concurrentScheme/threads/life.scm",
          "test/concurrentScheme/threads/matmul.scm",
          "test/concurrentScheme/threads/mazefun.scm",
          "test/concurrentScheme/threads/mceval.scm",
          "test/concurrentScheme/threads/minimax.scm",
          "test/concurrentScheme/threads/msort.scm",
          "test/concurrentScheme/threads/qsort.scm",
          "test/concurrentScheme/threads/tsp.scm")(b)

    /* On Bertha with a timeout of 3 minutes:
        test/concurrentScheme/threads/fact2.scm finished in 6ms.
        test/concurrentScheme/threads/actors.scm timed out.
        test/concurrentScheme/threads/matmul.scm timed out.
        test/concurrentScheme/threads/phil.scm finished in 12ms.
        test/concurrentScheme/threads/fact-indep.scm finished in 1ms.
        test/concurrentScheme/threads/peterson.scm finished in 1ms.
        test/concurrentScheme/threads/life.scm timed out.
        test/concurrentScheme/threads/trapr.scm finished in 22ms.
        test/concurrentScheme/threads/qsort.scm timed out.
        test/concurrentScheme/threads/crypt.scm finished in 2029ms.
        test/concurrentScheme/threads/stm.scm finished in 2340ms.
        test/concurrentScheme/threads/atoms.scm finished in 48ms.
        test/concurrentScheme/threads/randomness.scm finished in 10ms.
        test/concurrentScheme/threads/mcarlo.scm finished in 55125ms.
        test/concurrentScheme/threads/msort.scm timed out.
        test/concurrentScheme/threads/randomness2.scm finished in 14ms.
        test/concurrentScheme/threads/tsp.scm timed out.
        test/concurrentScheme/threads/count.scm finished in 35ms.
        test/concurrentScheme/threads/sieve.scm finished in 51ms.
        test/concurrentScheme/threads/sudoku.scm finished in 109ms.
        test/concurrentScheme/threads/fact.scm timed out.
        test/concurrentScheme/threads/bchain.scm finished in 117ms.
        test/concurrentScheme/threads/pc.scm finished in 9ms.
        test/concurrentScheme/threads/pp.scm finished in 6ms.
        test/concurrentScheme/threads/producer.scm finished in 8ms.
        test/concurrentScheme/threads/rng.scm finished in 21ms.
        test/concurrentScheme/threads/simple.scm finished in 0ms.
        test/concurrentScheme/threads/philosophers2.scm finished in 6ms.
        test/concurrentScheme/threads/nbody.scm finished in 677ms.
        test/concurrentScheme/threads/minimax.scm timed out.
        test/concurrentScheme/threads/pps.scm finished in 358ms.
        test/concurrentScheme/threads/dekker.scm finished in 7ms.
        test/concurrentScheme/threads/readers2.scm finished in 0ms.
        test/concurrentScheme/threads/ringbuf.scm finished in 53ms.
        test/concurrentScheme/threads/mceval.scm timed out.
   */

}