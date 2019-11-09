import org.scalatest._
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException

import scalaam.core._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._
import scalaam.language.scheme.SchemeInterpreter._

trait SchemeModFSoundnessTests extends PropSpec {
  type Benchmark = String   // a benchmark is just a file name
  type Analysis = ModAnalysis[SchemeExp] with SchemeModFSemantics
  // the table of benchmark programs to execute
  def benchmarks: List[Benchmark]
  // the analysis that is used to analyse the programs
  def analysis(b: Benchmark): Analysis
  // the timeout for the analysis of a single benchmark program (default: 5min.)
  def timeout(b: Benchmark) = Timeout.duration(Duration(5, MINUTES))
  // the actual testing code
  protected def loadFile(file: String): SchemeExp = {
    val f   = scala.io.Source.fromFile(file)
    val exp = SchemeParser.parse(f.getLines().mkString("\n"))
    f.close()
    exp
  }
  private def evalConcrete(benchmark: Benchmark, t: Timeout.T): (Value, Map[Position,Set[Value]]) = try {
    val program = SchemeUndefiner.undefine(List(loadFile(benchmark)))
    var posResults = Map[Position,Set[Value]]().withDefaultValue(Set())
    val interpreter = new SchemeInterpreter((p, v) => posResults += (p -> (posResults(p) + v)), false)
    val endResult = interpreter.run(program, t)
    (endResult, posResults)
  } catch {
    case _ : TimeoutException => cancel(s"Concrete evaluation for $benchmark timed out")
    case _ : Throwable        => cancel(s"Concrete evaluation for $benchmark failed")
  }
  private def checkSubsumption(analysis: Analysis)(v: Set[Value], abs: analysis.Value): Boolean = {
    val lat = analysis.lattice
    v.forall {
      case Value.Undefined(_)   => true
      case Value.Unbound(_)     => true
      case Value.Clo(_, _)      => lat.getClosures(abs).nonEmpty
      case Value.Primitive(_)   => lat.getPrimitives(abs).nonEmpty
      case Value.Str(s)         => lat.subsumes(abs, lat.string(s))
      case Value.Symbol(s)      => lat.subsumes(abs, lat.symbol(s))
      case Value.Integer(i)     => lat.subsumes(abs, lat.number(i))
      case Value.Real(r)        => lat.subsumes(abs, lat.real(r))
      case Value.Bool(b)        => lat.subsumes(abs, lat.bool(b))
      case Value.Character(c)   => lat.subsumes(abs, lat.char(c))
      case Value.Nil            => lat.subsumes(abs, lat.nil)
      case Value.Cons(_, _)     => lat.getPointerAddresses(abs).nonEmpty
      case Value.Vector(_)      => lat.getPointerAddresses(abs).nonEmpty
      case v                    => throw new Exception(s"Unknown concrete value type: $v")
    }
  }

  private def compareResult(a: Analysis, concRes: Value) = {
    val aRes = a.store(a.ReturnAddr(a.MainComponent))
    assert(checkSubsumption(a)(Set(concRes), aRes), "the end result is sound")
  }

  private def comparePositions(a: Analysis, concPos: Map[Position,Set[Value]]) = {
    val absPos: Map[Position, a.Value] = a.store.groupBy({_._1 match {
      case a.ComponentAddr(_, addr) => addr.pos()
      case _                        => Position.none
    }}).mapValues(_.values.foldLeft(a.lattice.bottom)((x,y) => a.lattice.join(x,y)))
    concPos.foreach { case (pos,values) =>
      assert(checkSubsumption(a)(values, absPos(pos)), s"intermediate result at $pos is sound")
    }
  }

  benchmarks.foreach { benchmark =>
    property(s"Analysis of $benchmark is sound") {
      val t = timeout(benchmark)
      val (cResult, cPosResults) = evalConcrete(benchmark,t)
      val a = analysis(benchmark)
      a.analyze(t)
      if (a.finished) {
        compareResult(a, cResult)
        comparePositions(a, cPosResults)
      } else {
        cancel(s"Analysis of $benchmark timed out")
      }
    }
  }
}

trait BigStepSchemeModF extends SchemeModFSoundnessTests {
  def analysis(b: Benchmark) = new ModAnalysis(loadFile(b))
                                  with BigStepSchemeModFSemantics
                                  with ConstantPropagationDomain
                                  with NoSensitivity
}

trait SimpleBenchmarks extends SchemeModFSoundnessTests {
  def benchmarks = Benchmarks.other
}

class BigStepSchemeModFSoundnessTests extends SchemeModFSoundnessTests
                                         with BigStepSchemeModF
                                         with SimpleBenchmarks
