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
  def name = "simple ModConc"
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
                                                                    with SimpleBenchmarks

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
    // These tests currently slow down the test suite too much:
    Set("test/concurrentScheme/threads/actors.scm",
        "test/concurrentScheme/threads/life.scm",
        "test/concurrentScheme/threads/fact.scm",
        "test/concurrentScheme/threads/matmul.scm",
        "test/concurrentScheme/threads/mazefun.scm",
        "test/concurrentScheme/threads/mceval.scm",
        "test/concurrentScheme/threads/minimax.scm")(b)
}