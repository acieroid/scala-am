package scalaam.test.modular.scheme

import java.util.concurrent.TimeoutException

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

import scala.concurrent.duration._

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
    val aRes = a.store.getOrElse(ComponentAddr(a.initialComponent, ReturnAddr), a.lattice.bottom)
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
        case ComponentAddr(_, VarAddr(id))  => id.idn
        case ComponentAddr(_, PtrAddr(ep))  => ep.idn
        case ComponentAddr(_, ReturnAddr)   => Identity.none
        case GlobalAddr(PrmAddr(_))         => Identity.none
        case a                              => throw new Exception(s"Unsupported address $a")
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
      Seq(SchemeTest, SoundnessTest, SlowTest)
    } else {
      Seq(SchemeTest, SoundnessTest)
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
  def analysis(program: SchemeExp) = new SimpleSchemeModFAnalysis(program)
                                      with SchemeConstantPropagationDomain
                                      with NoSensitivity
                                      with LIFOWorklistAlgorithm[SchemeExp]
}

trait BigStepSchemeModFPrimCSSensitivity extends SchemeModFSoundnessTests {
  def name = "big-step semantics with call-site sensitivity for primitives"
  def analysis(program: SchemeExp) = new SimpleSchemeModFAnalysis(program)
      with SchemeConstantPropagationDomain
      with CompoundSensitivities.TrackLowToHighSensitivity.S_CS_0
      with LIFOWorklistAlgorithm[SchemeExp]
}

trait SmallStepSchemeModF extends SchemeModFSoundnessTests {
  def name = "small-step semantics"
  def analysis(program: SchemeExp) = new ModAnalysis(program)
                                      with SchemeModFSemantics
                                      with SmallStepModFSemantics
                                      with StandardSchemeModFComponents
                                      with SchemeConstantPropagationDomain
                                      with NoSensitivity
                                      with LIFOWorklistAlgorithm[SchemeExp] {
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with SmallStepIntra
  }
}

trait ParallelSchemeModF extends SchemeModFSoundnessTests {
  def name = "parallel analysis (n = 4)"
  def analysis(program: SchemeExp) = new SimpleSchemeModFAnalysis(program)
                                      with SchemeConstantPropagationDomain
                                      with NoSensitivity
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

trait SchemeModConcSoundnessTests extends SchemeBenchmarkTests {
  // analysis must support Scheme's ModF Semantics
  type Analysis = ModAnalysis[SchemeExp] with KAExpressionContext with ModConcConstantPropagationDomain
  // the analysis that is used to analyse the programs
  def name: String
  def analysis(b: SchemeExp): Analysis
  // the timeout for the analysis of a single benchmark program (default: 2min.)
  val tAbs: Duration = Duration(2, MINUTES)
  def abstractTimeout(): Timeout.T = Timeout.start(tAbs)
  val tConc: Duration = Duration(1, MINUTES)
  def concreteTimeout(): Timeout.T = Timeout.start(tConc)
  // the actual testing code
  private def evalC(preludedUndefinedProgram: SchemeExp, benchmark: Benchmark, timeout: Timeout.T): (Set[Value], Map[Identity,Set[Value]]) = {
    var idnResults = Map[Identity,Set[Value]]().withDefaultValue(Set())
    val interpreter = new SchemeInterpreter((i, v) => idnResults += (i -> (idnResults(i) + v)), false)
    try {
      val endResult = interpreter.run(preludedUndefinedProgram, timeout)
      (Set(endResult), idnResults)
    } catch {
      case _ : TimeoutException =>
        alert(s"Concrete evaluation of $benchmark timed out.")
        (Set(), idnResults)
      case e : VirtualMachineError =>
        System.gc()
        alert(s"Concrete evaluation of $benchmark failed with $e")
        (Set(), idnResults)
    }
  }

  private def evalConcrete(originalProgram: SchemeExp, benchmark: Benchmark, timeout: Timeout.T = concreteTimeout()): (Set[Value], Map[Identity,Set[Value]]) = {
    val preluded = SchemePrelude.addPrelude(originalProgram)
    val program = CSchemeUndefiner.undefine(List(preluded))
    evalC(program, benchmark, timeout)
  }

  private def repeatConcrete(originalProgram: SchemeExp, benchmark: Benchmark): (Set[Value], Map[Identity,Set[Value]]) = {
    val preluded = SchemePrelude.addPrelude(originalProgram)
    val program = CSchemeUndefiner.undefine(List(preluded))
    var idnResults: Map[Identity,Set[Value]] = Map().withDefaultValue(Set())
    var endResults: Set[Value] = Set()
    var t: Timeout.T = concreteTimeout()
    while (true) {
      val (cResult, cPosResults) = evalC(program,benchmark, t)
      val timeRemaining = t.timeLeft
      cPosResults.foreach({ case (idn, values) => idnResults += (idn -> SmartUnion.sunion(idnResults(idn), values)) })
      if (cResult.nonEmpty)
        endResults = endResults + cResult.head
      else
        return (endResults, idnResults)
      if (timeRemaining.getOrElse(1L) <= 0) return (endResults, idnResults)
      t = Timeout.start(timeRemaining.map(Duration(_, NANOSECONDS)).getOrElse(tConc))
    }
    throw new Exception("repeatConcrete should not reach this point.")
  }

  private def runAnalysis(program: SchemeExp, benchmark: Benchmark): Analysis =
    try {
      // analyze the program using a ModF analysis
      val anl = analysis(program)
      anl.analyze(abstractTimeout())
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
      case Value.Thread(_)      => lat.getThreads(abs).nonEmpty
      case Value.Lock(_)        => lat.getPointerAddresses(abs).nonEmpty
      case v                    => throw new Exception(s"Unknown concrete value type: $v.")
    }
  }

  private def compareResult(a: Analysis, concRes: Value) = {
    val aRes = a.store.getOrElse(ComponentAddr(a.initialComponent,ReturnAddr), a.lattice.bottom)
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
        case ComponentAddr(_, a.VarAddr(id))  => id.idn
        case ComponentAddr(_, a.PtrAddr(ep))  => ep.idn
        case ComponentAddr(_, ReturnAddr)     => Identity.none
        case GlobalAddr(a.PrmAddr(_))         => Identity.none
        case a                              => throw new Exception(s"Unsupported address $a")
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
      Seq(CSchemeTest, SoundnessTest, SlowTest)
    } else {
      Seq(CSchemeTest, SoundnessTest)
    }

  def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = CSchemeParser.parse(content)
      // run the program using a concrete interpreter once if it does not contain threads, or multiple times if it does
      val (cResult, cPosResults) = if (SchemeBenchmarks.threads.contains(benchmark)) repeatConcrete(program, benchmark) else evalConcrete(program,benchmark)
      // analyze the program using a ModF analysis
      val anl = runAnalysis(program,benchmark)
      // check if each final result of the analysis soundly approximates the final result of concrete evaluation
      // of course, this can only be done if there was a result.
      cResult.foreach { compareResult(anl, _) }
      // check if the intermediate results at various program points are soundly approximated by the analysis
      // this can be done, regardless of whether the concrete evaluation terminated successfully or not
      compareIdentities(anl, cPosResults)
    }
}

trait SmallStepSchemeModConc extends SchemeModConcSoundnessTests {
  def name = "small-step ModConc"
  def analysis(program: SchemeExp): Analysis = new ModAnalysis(program)
    with KAExpressionContext
    with ModConcConstantPropagationDomain
    with LIFOWorklistAlgorithm[SchemeExp] {}
}

class SmallStepSchemeModConcSoundnessTests extends SmallStepSchemeModConc with ThreadBenchmarks
                                                                          with AllBenchmarks {
  override def isSlow(b: Benchmark): Boolean =
    (SchemeBenchmarks.sequentialBenchmarks.contains(b) && !SchemeBenchmarks.other.contains(b)) ||
    // these tests currently slow down the test suite too much
    Set("test/concurrentScheme/R5RS/mceval.scm",
        "test/concurrentScheme/threads/minimax.scm",
        "test/concurrentScheme/threads/mceval.scm")(b)
}