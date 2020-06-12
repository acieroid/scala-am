package scalaam.test.soundness

import scala.concurrent.duration._
import java.util.concurrent.TimeoutException

import org.scalatest.Tag
import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import scalaam.test._
import scalaam.test.tag._
import scalaam.core._
import scalaam.core.Position._
import scalaam.util._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._
import scalaam.language.scheme.SchemeInterpreter._
import scalaam.language.scheme.primitives.SchemePrelude
import scalaam.util.benchmarks.Timeout

trait SchemeModFSoundnessTests extends SchemeBenchmarkTests {
  // analysis must support Scheme's ModF Semantics
  type Analysis = ModAnalysis[SchemeExp] with SchemeModFSemantics
  // the analysis that is used to analyse the programs
  def name: String
  def analysis(b: SchemeExp): Analysis
  // the timeout for the analysis of a single benchmark program (default: 2min.)
  def timeout(b: Benchmark): Timeout.T = Timeout.start(Duration(2, MINUTES))
  // the actual testing code
  private def evalConcrete(originalProgram: SchemeExp, benchmark: Benchmark): (Option[Value], Map[Identity,Set[Value]]) = {
    val preluded = SchemePrelude.addPrelude(originalProgram)
    val program = SchemeUndefiner.undefine(List(preluded))
    var idnResults = Map[Identity,Set[Value]]().withDefaultValue(Set())
    val interpreter = new SchemeInterpreter((i, v) => idnResults += (i -> (idnResults(i) + v)), false)
    try {
      val endResult = interpreter.run(program, timeout(benchmark))
      (Some(endResult), idnResults)
    } catch {
      case _ : TimeoutException =>
        alert(s"Concrete evaluation of $benchmark timed out.")
        (None, idnResults)
      case e : VirtualMachineError =>
        System.gc()
        alert(s"Concrete evaluation of $benchmark failed with $e")
        (None, idnResults)
    }
  }
  private def runAnalysis(program: SchemeExp, benchmark: Benchmark): Analysis =
    try {
      // analyze the program using a ModF analysis
      val anl = analysis(program)
      anl.analyze(timeout(benchmark))
      assume(anl.finished(), "Analysis timed out")
      anl
    } catch {
      case e: VirtualMachineError => 
        System.gc()
        cancel(s"Analysis of $benchmark encountered an error: $e")
    }
  private def checkSubsumption(analysis: Analysis)(v: Value, abs: analysis.Value) = {
    val lat = analysis.lattice
    v match {
      case Value.Undefined(_)   => true
      case Value.Unbound(_)     => true
      case Value.Clo(lam, _)    => lat.getClosures(abs).exists(_._1._1.idn == lam.idn)
      case Value.Primitive(p)   => lat.getPrimitives(abs).exists(_.name == p.name)
      case Value.Str(s)         => lat.subsumes(abs, lat.string(s))
      case Value.Symbol(s)      => lat.subsumes(abs, lat.symbol(s))
      case Value.Integer(i)     => lat.subsumes(abs, lat.number(i))
      case Value.Real(r)        => lat.subsumes(abs, lat.real(r))
      case Value.Bool(b)        => lat.subsumes(abs, lat.bool(b))
      case Value.Character(c)   => lat.subsumes(abs, lat.char(c))
      case Value.Nil            => lat.subsumes(abs, lat.nil)
      case Value.Pointer(_)     => lat.getPointerAddresses(abs).nonEmpty
      case v                    => throw new Exception(s"Unknown concrete value type: $v.")
    }
  }

  private def compareResult(a: Analysis, concRes: Value) = {
    val aRes = a.store.getOrElse(a.ReturnAddr(a.initialComponent), a.lattice.bottom)
    if (!checkSubsumption(a)(concRes, aRes)) {
      val failureMsg = 
s"""Program result is unsound:
  - concrete value: $concRes
  - abstract value: $aRes
"""
      fail(failureMsg)
    }
  }

  private def compareIdentities(a: Analysis, concIdn: Map[Identity,Set[Value]]): Unit = {
    val absID: Map[Identity, a.Value] = a.store.groupBy({_._1 match {
        case a.ComponentAddr(_, addr) => addr.idn()
        case _                     => Identity.none
      }}).view.mapValues(_.values.foldLeft(a.lattice.bottom)((x,y) => a.lattice.join(x,y))).toMap.withDefaultValue(a.lattice.bottom)
    concIdn.foreach { case (idn,values) =>
      values.foreach { value => 
        if (!checkSubsumption(a)(value, absID(idn))) {
          val failureMsg = 
s"""Intermediate result at $idn is unsound:
  - concrete value: $value
  - abstract value: ${absID(idn)}
"""
          fail(failureMsg)
        }
      }
    }
  }

  // indicate if a benchmark is slow or not
  def isSlow(b: Benchmark) = false 

  private def testTags(b: Benchmark): Seq[Tag] = 
    if (isSlow(b)) {
      Seq(SoundnessTest, SlowTest)
    } else {
      Seq(SoundnessTest)
    }

  def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = SchemeParser.parse(content)
      // run the program using a concrete interpreter
      val (cResult, cPosResults) = evalConcrete(program,benchmark)
      // analyze the program using a ModF analysis
      val anl = runAnalysis(program,benchmark)
      // check if the final result of the analysis soundly approximates the final result of concrete evaluation
      // of course, this can only be done if there was a result.
      if (cResult.isDefined) { compareResult(anl, cResult.get) }
      // check if the intermediate results at various program points are soundly approximated by the analysis
      // this can be done, regardless of whether the concrete evaluation terminated successfully or not
      compareIdentities(anl, cPosResults)
    }
}

trait BigStepSchemeModF extends SchemeModFSoundnessTests {
  def name = "big-step semantics"
  def analysis(program: SchemeExp) = new ModAnalysis(program)
                                      with BigStepSemantics
                                      with StandardSchemeModFSemantics
                                      with ConstantPropagationDomain
                                      with NoSensitivity
                                      with LIFOWorklistAlgorithm[SchemeExp] {
  }
}

trait BigStepSchemeModFPrimCSSensitivity extends SchemeModFSoundnessTests {
  def name = "big-step semantics with call-site sensitivity for primitives"
  def analysis(program: SchemeExp) = new ModAnalysis(program)
      with BigStepSemantics
      with StandardSchemeModFSemantics
      with ConstantPropagationDomain
      with CompoundSensitivities.TrackLowToHighSensitivity.S_CS_0
      with LIFOWorklistAlgorithm[SchemeExp] {
  }
}

trait SmallStepSchemeModF extends SchemeModFSoundnessTests {
  def name = "small-step semantics"
  def analysis(program: SchemeExp) = new ModAnalysis(program)
                                      with SmallStepSemantics
                                      with StandardSchemeModFSemantics
                                      with ConstantPropagationDomain
                                      with NoSensitivity
                                      with LIFOWorklistAlgorithm[SchemeExp] {
  }
}

trait ParallelSchemeModF extends SchemeModFSoundnessTests {
  def name = "parallel analysis"
  def analysis(program: SchemeExp) = new ModAnalysis(program)
                                      with BigStepSemantics
                                      with StandardSchemeModFSemantics
                                      with ConstantPropagationDomain
                                      with NoSensitivity
                                      with ParallelWorklistAlgorithm[SchemeExp] {
      override val workers = 4
      override def intraAnalysis(cmp: Component) = new BigStepIntra(cmp) with ParallelIntra
  }
}

trait SimpleAdaptiveSchemeModF extends SchemeModFSoundnessTests {
  def name = "simple adaptive argument sensitivity (limit = 5)"
  def analysis(program: SchemeExp) = new AdaptiveModAnalysis(program)
                                        with AdaptiveArgumentSensitivityPolicy3
                                        with AdaptiveSchemeModFSemantics
                                        with ConstantPropagationDomain
                                        with LIFOWorklistAlgorithm[SchemeExp] {
    val limit = 5
    override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
    override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
  }
}


// concrete test suites to run ...

class BigStepSchemeModFSoundnessTests extends BigStepSchemeModF with AllBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarks.other.contains(b)
}
class BigStepSchemeModFPrimCSSensitivitySoundnessTests extends BigStepSchemeModFPrimCSSensitivity with AllBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarks.other.contains(b)
}
class SmallStepSchemeModFSoundnessTests extends SmallStepSchemeModF with AllBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarks.other.contains(b)
}
class ParallelSchemeModFSoundnessTests extends ParallelSchemeModF with AllBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarks.other.contains(b)
}

//class SimpleAdaptiveSchemeModFSoundnessTests extends SimpleAdaptiveSchemeModF with AllBenchmarks {
//  override def isSlow(b: Benchmark) = !SchemeBenchmarks.other.contains(b)
//}
