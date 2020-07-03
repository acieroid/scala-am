package scalaam.test.modular.scheme

import org.scalatest.Tag
import scalaam.core.Position._
import scalaam.core._
import scalaam.language.CScheme._
import scalaam.language.scheme.SchemeInterpreter._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives.SchemePrelude
import scalaam.modular._
import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.modular.scheme.modconc._
import scalaam.modular.scheme.ssmodconc._
import scalaam.test._
import scalaam.util._
import scalaam.util.benchmarks.Timeout


trait SchemeModConcSoundnessTests extends SchemeSoundnessTests {
  override def testTags(b: Benchmark) = super.testTags(b) :+ SchemeModConcTest
}

trait SmallStepSchemeModConc extends SchemeModConcSoundnessTests {
  def name = "small-step ModConc"
  def analysis(program: SchemeExp): Analysis = new ModAnalysis(program)
    with KAExpressionContext
    with SchemeConstantPropagationDomain
    with LIFOWorklistAlgorithm[SchemeExp] {}
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