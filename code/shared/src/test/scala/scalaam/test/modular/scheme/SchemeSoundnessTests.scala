package scalaam.test.modular.scheme

import java.util.concurrent.TimeoutException

import org.scalatest.Tag
import scalaam.core._
import scalaam.language.CScheme._
import scalaam.language.scheme.SchemeInterpreter._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives.SchemePrelude
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.test._
import scalaam.util._
import scalaam.util.benchmarks.Timeout

import scala.concurrent.duration._

trait SchemeSoundnessTests extends SchemeBenchmarkTests {
  // analysis must support basic Scheme semantics
  type Analysis = ModAnalysis[SchemeExp] with GlobalStore[SchemeExp]
                                         with ReturnValue[SchemeExp]
                                         with SchemeDomain
  // the analysis that is used to analyse the programs
  def name: String
  def analysis(b: SchemeExp): Analysis
  // the timeout and max number of concrete runs for a single benchmark program (default: 1min.)
  def analysisTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(1, MINUTES))
  def concreteTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(1, MINUTES))
  def concreteRuns(b: Benchmark): Int = 1 // for highly non-deterministic programs, a higher value is recommended
  // the actual testing code
  protected def runInterpreter(i: SchemeInterpreter, p: SchemeExp, t: Timeout.T): Value = i.run(p, t) // If there are code changes in the file, runs the "new" version by default (ensures compatibility with files containing changes).
  protected def evalConcrete(originalProgram: SchemeExp, benchmark: Benchmark): (Set[Value], Map[Identity,Set[Value]]) = {
    val preluded = SchemePrelude.addPrelude(originalProgram)
    val program = CSchemeUndefiner.undefine(List(preluded))
    var endResults = Set[Value]()
    var idnResults = Map[Identity,Set[Value]]().withDefaultValue(Set())
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    try {
      for (_ <- 1 to times) {
        val interpreter = new SchemeInterpreter((i, v) => idnResults += (i -> (idnResults(i) + v)), false)
        endResults += runInterpreter(interpreter, program, timeout)
      }
    } catch {
      case _ : TimeoutException =>
        alert(s"Concrete evaluation of $benchmark timed out.")
      case ChildThreadDiedException(_) =>
        alert(s"Concrete evaluation of $benchmark aborted due to a fatal crash in a child thread.")
      case e : VirtualMachineError =>
        System.gc()
        alert(s"Concrete evaluation of $benchmark failed with $e")
    }
    (endResults, idnResults)
  }
  protected def runAnalysis(program: SchemeExp, benchmark: Benchmark): Analysis =
    try {
      // analyze the program using a ModF analysis
      val anl = analysis(program)
      val timeout = analysisTimeout(benchmark)
      anl.analyze(timeout)
      assume(anl.finished(), "Analysis timed out")
      anl
    } catch {
      case e: VirtualMachineError =>
        System.gc()
        cancel(s"Analysis of $benchmark encountered an error: $e")
    }
  protected def checkSubsumption(analysis: Analysis)(v: Value, abs: analysis.Value): Boolean = {
    val lat = analysis.lattice
    v match {
      case Value.Undefined(_)   => true
      case Value.Unbound(_)     => true
      case Value.Void           => lat.subsumes(abs, lat.void)
      case Value.Clo(lam, _, _) => lat.getClosures(abs).exists(_._1._1.idn == lam.idn)
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
      case v                    => throw new Exception(s"Unknown concrete value type: $v.")
    }
  }

  protected def compareResult(a: Analysis, values: Set[Value]): Unit = {
    val aRes = a.finalResult
    values.foreach { value => 
      if (!checkSubsumption(a)(value, aRes)) {
        val failureMsg =
s"""Program result is unsound:
  - concrete value: $value
  - abstract value: $aRes
"""
        fail(failureMsg)
      } 
    }
  }

  protected def compareIdentities(a: Analysis, concIdn: Map[Identity,Set[Value]]): Unit = {
    val absID: Map[Identity, a.Value] = a.store.groupBy(_._1.idn).view
                                                .mapValues(m => a.lattice.join(m.values)).toMap
                                                .withDefaultValue(a.lattice.bottom)
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

  def testTags(b: Benchmark): Seq[Tag] =
    if (isSlow(b)) {
      Seq(SoundnessTest, SlowTest)
    } else {
      Seq(SoundnessTest)
    }

  def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = CSchemeParser.parse(content)
      // run the program using a concrete interpreter
      val (cResult, cPosResults) = evalConcrete(program,benchmark)
      // analyze the program using a ModF analysis
      val anl = runAnalysis(program,benchmark)
      // check if the final result of the analysis soundly approximates the final result of concrete evaluation
      compareResult(anl, cResult)
      // check if the intermediate results at various program points are soundly approximated by the analysis
      compareIdentities(anl, cPosResults)
    }
}