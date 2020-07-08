package scalaam.test.modular.scheme

import scalaam.core.Position._
import scalaam.language.scheme._
import scalaam.modular._
import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.test._

trait SchemeModFSoundnessTests extends SchemeSoundnessTests {
  override def testTags(b: Benchmark) = super.testTags(b) :+ SchemeModFTest
}

trait BigStepSchemeModF extends SchemeModFSoundnessTests {
  def name = "big-step semantics"
  def analysis(program: SchemeExp) = new SimpleSchemeModFAnalysis(program)
                                      with SchemeConstantPropagationDomain
                                      with SchemeModFNoSensitivity
                                      with LIFOWorklistAlgorithm[SchemeExp]
}

trait BigStepSchemeModFPrimCSSensitivity extends SchemeModFSoundnessTests {
  def name = "big-step semantics with call-site sensitivity for primitives"
  def analysis(program: SchemeExp) = new SimpleSchemeModFAnalysis(program)
      with SchemeConstantPropagationDomain
      with SchemeModFCompoundSensitivities.TrackLowToHighSensitivity.S_CS_0
      with LIFOWorklistAlgorithm[SchemeExp]
}

trait SmallStepSchemeModF extends SchemeModFSoundnessTests {
  def name = "small-step semantics"
  def analysis(program: SchemeExp) = new ModAnalysis(program)
                                      with SchemeModFSemantics
                                      with SmallStepModFSemantics
                                      with StandardSchemeModFComponents
                                      with SchemeConstantPropagationDomain
                                      with SchemeModFNoSensitivity
                                      with LIFOWorklistAlgorithm[SchemeExp] {
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with SmallStepIntra
  }
}

trait ParallelSchemeModF extends SchemeModFSoundnessTests {
  def name = "parallel analysis (n = 4)"
  def analysis(program: SchemeExp) = new SimpleSchemeModFAnalysis(program)
                                      with SchemeConstantPropagationDomain
                                      with SchemeModFNoSensitivity
                                      with ParallelWorklistAlgorithm[SchemeExp] {
      override def workers = 4
      override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra with ParallelIntra
  }
}

trait SimpleAdaptiveSchemeModF extends SchemeModFSoundnessTests {
  def name = "simple adaptive argument sensitivity (limit = 5)"
  def analysis(program: SchemeExp) = new AdaptiveModAnalysis(program)
                                        with AdaptiveArgumentSensitivityPolicy3
                                        with AdaptiveSchemeModFSemantics
                                        with SchemeConstantPropagationDomain
                                        with LIFOWorklistAlgorithm[SchemeExp] {
    val limit = 5
    override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
    override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
  }
}

// concrete test suites to run ...

class BigStepSchemeModFSoundnessTests extends BigStepSchemeModF with SequentialBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarks.other.contains(b)
}
class BigStepSchemeModFPrimCSSensitivitySoundnessTests extends BigStepSchemeModFPrimCSSensitivity with SequentialBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarks.other.contains(b)
}
class SmallStepSchemeModFSoundnessTests extends SmallStepSchemeModF with SequentialBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarks.other.contains(b)
}
class ParallelSchemeModFSoundnessTests extends ParallelSchemeModF with SequentialBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarks.other.contains(b)
}