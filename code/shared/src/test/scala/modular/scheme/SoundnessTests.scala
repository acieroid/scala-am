package scalaam.test.soundness

import scala.concurrent.duration._
import java.util.concurrent.TimeoutException

import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import scalaam.test._
import scalaam.core._
import scalaam.core.Identity.Position
import scalaam.util._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._
import scalaam.language.scheme.SchemeInterpreter._
import scalaam.language.scheme.primitives.{SchemeLatticePrimitives, SchemePrimitives}

trait SchemeModFSoundnessTests extends SchemeBenchmarkTests {
  // analysis must support Scheme's ModF Semantics
  type Analysis = ModAnalysis[SchemeExp] with SchemeModFSemantics
  // the analysis that is used to analyse the programs
  def name: String
  def analysis(b: SchemeExp): Analysis
  // the timeout for the analysis of a single benchmark program (default: 2min.)
  def timeout(b: Benchmark): Timeout.T = Timeout.start(Duration(2, MINUTES))
  // the actual testing code
  private def evalConcrete(originalProgram: SchemeExp, t: Timeout.T): (Option[Value], Map[Identity,Set[Value]]) = {
    val program = SchemeUndefiner.undefine(List(originalProgram))
    var idnResults = Map[Identity,Set[Value]]().withDefaultValue(Set())
    val interpreter = new SchemeInterpreter((i, v) => idnResults += (i -> (idnResults(i) + v)), false)
    try {
      val endResult = interpreter.run(program, t)
      (Some(endResult), idnResults)
    } catch {
      case _ : TimeoutException =>
        alert(s"Concrete evaluation timed out.")
        (None, idnResults)
      case _ : StackOverflowError =>
        alert(s"Concrete evaluation ran out of stack space.")
        (None, idnResults)
    }
  }
  private def checkSubsumption(analysis: Analysis)(v: Set[Value], abs: analysis.Value): Boolean = {
    val lat = analysis.lattice
    v.forall {
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
      case Value.Cons(_, _)     => lat.getConsCells(abs).nonEmpty
      case Value.Vector(_)      => lat.getPointerAddresses(abs).nonEmpty
      case v                    => throw new Exception(s"Unknown concrete value type: $v.")
    }
  }

  private def compareResult(a: Analysis, concRes: Value) = {
    val aRes = a.store(a.ReturnAddr(a.initialComponent))
    assert(checkSubsumption(a)(Set(concRes), aRes),
      s"program result is not sound: $aRes does not subsume $concRes.")
  }

  private def compareIdentities(a: Analysis, concIdn: Map[Identity,Set[Value]]): Unit = {
    val absID: Map[Identity, a.Value] = a.store.groupBy({_._1 match {
        case a.ComponentAddr(addr) => addr.idn()
        case _                     => Identity.none
      }}).view.mapValues(_.values.foldLeft(a.lattice.bottom)((x,y) => a.lattice.join(x,y))).toMap
    concIdn.foreach { case (idn,values) =>
      assert(checkSubsumption(a)(values, absID(idn)),
            s"intermediate result at $idn is not sound: ${absID(idn)} does not subsume $values.")
    }
  }

  def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.") {
      // load the benchmark program
      val program = loadFile(benchmark)
      // run the program using a concrete interpreter
      val (cResult, cPosResults) = evalConcrete(program,timeout(benchmark))
      // analyze the program using a ModF analysis
      val a = analysis(program)
      a.analyze(timeout(benchmark))
      // assume that the analysis finished
      // if not, cancel the test for this benchmark
      assume(a.finished(), s"Analysis of $benchmark timed out.")
      // check if the final result of the analysis soundly approximates the final result of concrete evaluation
      // of course, this can only be done if the
      if (cResult.isDefined) { compareResult(a, cResult.get) }
      // check if the intermediate results at various program points are soundly approximated by the analysis
      // this can be done, regardless of whether the concrete evaluation terminated successfully or not
      compareIdentities(a, cPosResults)
    }
}

trait BigStepSchemeModF extends SchemeModFSoundnessTests {
  def name = "big-step semantics"
  def analysis(program: SchemeExp) = new ModAnalysis(program)
                                      with BigStepSemantics
                                      with StandardSchemeModFSemantics
                                      with ConstantPropagationDomain
                                      with NoSensitivity {
    val primitives = new SchemeLatticePrimitives[Value, Addr]()
  }
}

trait SmallStepSchemeModF extends SchemeModFSoundnessTests {
  def name = "small-step semantics"
  def analysis(program: SchemeExp) = new ModAnalysis(program)
                                      with SmallStepSemantics
                                      with StandardSchemeModFSemantics
                                      with ConstantPropagationDomain
                                      with NoSensitivity {
    val primitives = new SchemeLatticePrimitives[Value, Addr]()
  }
}

trait SimpleAdaptiveSchemeModF extends SchemeModFSoundnessTests {
  def name = "simple adaptive argument sensitivity (limit = 5)"
  def analysis(program: SchemeExp) = new AdaptiveModAnalysis(program)
    with AdaptiveArgumentSensitivityPolicy1
    with NaiveAdaptiveArgumentSelection
    with AdaptiveSchemeModFSemantics
    with ConstantPropagationDomain {
    val limit = 5
    override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Option[ComponentContext]) = super.allocCtx(nam,clo,args,call,caller)
    override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)

    override val primitives: SchemePrimitives[valueLattice.L, Addr] = new SchemeLatticePrimitives()
    override def getPtrCtx(cmp: Option[ComponentContext]): Any = ???
  }
}

// concrete test suites to run ...
// ... for big-step semantics
class BigStepSchemeModFSoundnessTests extends SchemeModFSoundnessTests
                                         with BigStepSchemeModF
                                         with SimpleBenchmarks
// ... for small-step semantics
class SmallStepSchemeModFSoundnessTests extends SchemeModFSoundnessTests
                                           with SmallStepSchemeModF
                                           with SimpleBenchmarks

class SimpleAdaptiveSchemeModFSoundnessTests extends SchemeModFSoundnessTests
                                                with SimpleAdaptiveSchemeModF
                                                with SimpleBenchmarks
