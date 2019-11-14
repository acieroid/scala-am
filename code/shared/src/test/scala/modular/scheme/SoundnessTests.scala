import scala.concurrent.duration._
import java.util.concurrent.TimeoutException

import scalaam.core._
import scalaam.util._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._
import scalaam.language.scheme.SchemeInterpreter._

trait SchemeModFSoundnessTests extends SchemeBenchmarkTests {
  // analysis must support Scheme's ModF Semantics
  type Analysis = ModAnalysis[SchemeExp] with SchemeModFSemantics
  // the table of benchmark programs to execute
  def benchmarks: Set[Benchmark]
  // the analysis that is used to analyse the programs
  def name: String
  def analysis(b: Benchmark): Analysis
  // the timeout for the analysis of a single benchmark program (default: 1min.)
  def timeout(b: Benchmark) = Timeout.start(Duration(1, MINUTES))
  // the actual testing code
  private def evalConcrete(benchmark: Benchmark, t: Timeout.T): (Option[Value], Map[Position,Set[Value]]) = {
    val program = SchemeUndefiner.undefine(List(loadFile(benchmark)))
    var posResults = Map[Position,Set[Value]]().withDefaultValue(Set())
    val interpreter = new SchemeInterpreter((p, v) => posResults += (p -> (posResults(p) + v)), false)
    try {
      val endResult = interpreter.run(program, t)
      (Some(endResult), posResults)
    } catch {
      case _ : TimeoutException =>
        alert(s"Concrete evaluation for $benchmark timed out")
        (None, posResults)
      case _ : StackOverflowError =>
        alert(s"Concrete evaluation for $benchmark ran out of stack space")
        (None, posResults)
    }
  }
  private def checkSubsumption(analysis: Analysis)(v: Set[Value], abs: analysis.Value): Boolean = {
    val lat = analysis.lattice
    v.forall {
      case Value.Undefined(_)   => true
      case Value.Unbound(_)     => true
      case Value.Clo(lam, _)    => lat.getClosures(abs).exists(_._1._1.pos == lam.pos)
      case Value.Primitive(p)   => lat.getPrimitives(abs).exists(_.name == p.name)
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
    assert(checkSubsumption(a)(Set(concRes), aRes), "the end result is not sound")
  }

  private def comparePositions(a: Analysis, concPos: Map[Position,Set[Value]]) = {
    val absPos: Map[Position, a.Value] = a.store.groupBy({_._1 match {
      case a.ComponentAddr(_, addr) => addr.pos()
      case _                        => Position.none
    }}).mapValues(_.values.foldLeft(a.lattice.bottom)((x,y) => a.lattice.join(x,y)))
    concPos.foreach { case (pos,values) =>
      assert(checkSubsumption(a)(values, absPos(pos)),
            s"intermediate result at $pos is not sound: ${absPos(pos)} does not subsume $values")
    }
  }

  def onBenchmark(benchmark: Benchmark) =
    property(s"Analysis of $benchmark using $name is sound") {
      // run the program using a concrete interpreter
      val (cResult, cPosResults) = evalConcrete(benchmark,timeout(benchmark))
      // analyze the program using a ModF analysis
      val a = analysis(benchmark)
      a.analyze(timeout(benchmark))
      // assume that the analysis finished
      // if not, cancel the test for this benchmark
      assume(a.finished, s"Analysis of $benchmark timed out")
      // check if the final result of the analysis soundly approximates the final result of concrete evaluation
      // of course, this can only be done if the
      if (cResult.isDefined) { compareResult(a, cResult.get) }
      // check if the intermediate results at various program points are soundly approximated by the analysis
      // this can be done, regardless of whether the concrete evaluation terminated succesfully or not
      comparePositions(a, cPosResults)
    }
}

trait BigStepSchemeModF extends SchemeModFSoundnessTests {
  def name = "big-step semantics"
  def analysis(b: Benchmark) = new ModAnalysis(loadFile(b))
                                  with BigStepSchemeModFSemantics
                                  with ConstantPropagationDomain
                                  with NoSensitivity
}

trait SmallStepSchemeModF extends SchemeModFSoundnessTests {
  def name = "small-step semantics"
  def analysis(b: Benchmark) = new ModAnalysis(loadFile(b))
                                  with SmallStepSchemeModFSemantics
                                  with ConstantPropagationDomain
                                  with NoSensitivity
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

/*

package scalaam.cli

import java.text.SimpleDateFormat
import java.util.concurrent.TimeoutException
import java.util.Calendar
import java.io.{BufferedWriter, FileWriter}

import au.com.bytecode.opencsv.CSVWriter
import scalaam.core._
import scalaam.language.scheme.SchemeInterpreter.Value
import scalaam.language.scheme._
import scalaam.modular.ModAnalysis
import scalaam.modular.scheme._

import scala.concurrent.duration._

object MODComparison extends App {

  type Machine = ModAnalysis[SchemeExp] with SchemeModFSemantics

  val defaultTimeout: Duration = Duration(2, MINUTES)

  def readFile(file: String): SchemeExp = {
    val f   = scala.io.Source.fromFile(file)
    val exp = SchemeParser.parse(f.getLines().mkString("\n"))
    f.close()
    exp
  }

  var writer: CSVWriter = _

  // Avoid output being buffered.
  def display(data: String): Unit = {
    print(data)
    Console.out.flush()
    writer.writeNext(data.trim)
    writer.flush()
  }

  def displayErr(data: String): Unit = {
    System.err.print(data)
    System.err.flush()
    writer.writeNext(data.trim)
    writer.flush()
  }

  val outputDir: String           = "./out/"
  val  calendar: Calendar         = Calendar.getInstance()
  val    format: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd-HH'h'mm")
  val detformat: SimpleDateFormat = new SimpleDateFormat("HH:mm:ss")

  // Creates a fileName including the given name, suffix and a timestamp.
  def ts(name: String, suffix: String): String =
    outputDir + format.format(calendar.getTime) + "_" + name + suffix

  def checkSubsumption(machine: Machine)(v: Set[Value], abs: machine.Value)
                      (implicit lat: SchemeLattice[machine.Value, machine.Addr, machine.Prim,_]): Boolean = v.forall {
    case Value.Undefined(_) => true
    case Value.Unbound(_)   => true
    case Value.Clo(_, _)    => lat.getClosures(abs).nonEmpty
    case Value.Primitive(p) =>
      machine.primitives.allPrimitives.find(_.name == p.name) match {
        case None       => false
        case Some(prim) => lat.subsumes(abs, lat.primitive(prim))
      }
    case Value.Str(s)             => lat.subsumes(abs, lat.string(s))
    case Value.Symbol(s)          => lat.subsumes(abs, lat.symbol(s))
    case Value.Integer(i)         => lat.subsumes(abs, lat.number(i))
    case Value.Real(r)            => lat.subsumes(abs, lat.real(r))
    case Value.Bool(b)            => lat.subsumes(abs, lat.bool(b))
    case Value.Character(c)       => lat.subsumes(abs, lat.char(c))
    case Value.Nil                => lat.subsumes(abs, lat.nil)
    case Value.Cons(_, _)         => lat.getPointerAddresses(abs).nonEmpty
    case Value.Vector(_)          => lat.getPointerAddresses(abs).nonEmpty
    case v                  => throw new Exception(s"Unknown concrete value type: $v")
  }

  def check(name: String, p: Position, machine: Machine)(v: Set[Value], abs: machine.Value)
           (implicit lat: SchemeLattice[machine.Value, machine.Addr, machine.Prim, _]): Unit = {
    if (!checkSubsumption(machine)(v,abs))
      display(s"$name: subsumption check failed: $v > $abs at $p.\n")
  }

  def forMachine(name: String, machine: Machine, cMap: Map[Position, Set[Value]], timeout: Duration): Unit = try {
    displayErr(s"* $name\n  - started:  ${detformat.format(calendar.getTime)}\n")
    machine.analyze(Timeout.duration(timeout))
    displayErr(s"  - finished: ${detformat.format(calendar.getTime)}\n")

    val deps  = machine.deps
    val store = machine.store

    val pMap: Map[Position, machine.Value] = store.groupBy({_._1 match {
      case machine.ComponentAddr(_, addr) => addr.pos()
      case _                              => Position.none
    }}).mapValues(_.values.foldLeft(machine.lattice.bottom)((a, b) => machine.lattice.join(a, b)))

    display(s"Number of components: ${machine.allComponents.size}.\n")
    display(s"Store keyset size ${store.keySet.size}.\n")
    display(s"Dependency keyset size: ${deps.keySet.size}.\n")

    for (elem <- cMap.keySet)
      check(name, elem, machine)(cMap(elem), pMap(elem))(machine.lattice)
  } catch {
    case _: TimeoutException => displayErr(s"$name timed out!\n")
    case e: Throwable => e.printStackTrace()
      writer.writeNext("*** Stacktrace omitted ***")
      writer.flush()
  }

  def forFile(file: String, timeout: Duration = defaultTimeout): Unit = try {
    displayErr(file + "\n")

    val program = readFile(file)
    val machines: List[(String, Machine)] = List(
      ("bigStep",   new ModAnalysis(program) with BigStepSchemeModFSemantics with FullArgumentSensitivity with ConstantPropagationDomain),
      //("smallStep", new ModAnalysis(program) with FullArgumentSensitivity with ConstantPropagationDomain with SmallStepSchemeModFSemantics),
    )

    // For every position, cMap keeps the concrete values encountered by the concrete interpreter.
    var cMap: Map[Position, Set[Value]] = Map().withDefaultValue(Set())
    val interpreter = new SchemeInterpreter((p, v) => cMap = cMap + (p -> (cMap(p) + v)), false)
    displayErr(s"* concrete\n  - started:  ${detformat.format(calendar.getTime)}\n")
    interpreter.run(SchemeUndefiner.undefine(List(program)), Timeout.duration(timeout))
    displayErr(s"  - finished: ${detformat.format(calendar.getTime)}\n")

    display(s"  -> Inferred ${cMap.keySet.size} positions to check values.\n")

    machines.foreach(m => forMachine(m._1, m._2, cMap, timeout))

    display("\n")

  } catch {
    case _: TimeoutException => displayErr("Concrete machine timed out!\n")
    case e: Throwable => e.printStackTrace()
      displayErr("***\n")
  }

  val output: String = ts("MODComparison",  ".txt")
  val out = new BufferedWriter(new FileWriter(output))
  writer = new CSVWriter(out, ',', CSVWriter.NO_QUOTE_CHARACTER)

  Benchmarks.allBenchmarks.foreach(forFile(_))
  writer.close()
}
*/
