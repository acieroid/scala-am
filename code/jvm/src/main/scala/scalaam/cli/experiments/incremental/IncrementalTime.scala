package scalaam.cli.experiments.incremental

import scalaam.core.Expression
import scalaam.language.CScheme.CSchemeParser
import scalaam.modular.incremental.IncrementalModAnalysis
import scalaam.language.change.CodeVersion._
import scalaam.language.scheme.SchemeExp
import scalaam.modular.incremental.scheme.AnalysisBuilder._
import scalaam.util.Reader
import scalaam.util.Writer._
import scalaam.util.benchmarks._

import scala.concurrent.duration._

trait IncrementalTime[E <: Expression] extends App {

  // The number of warm-up runs.
  val warmup = 5
  // The number of actually measured runs.
  val actual = 15

  // A list of programs on which the benchmark should be executed.
  def benchmarks(): List[String]

  // Type of an analysis.
  type Analysis = IncrementalModAnalysis[E]

  // Analysis construction.
  def analysis(e: E): Analysis

  // Parsing.
  def parse(string: String): E

  // The timeout to be used.
  def timeout(): Timeout.T

  // The results of the evaluation.
  var results: Table[Option[Int]] = Table.empty

  // A single program run with the analysis.
  def benchmark(file: String): Unit = {
    println(s"Testing $file")
    val program = parse(file)

    // Use the same timeout for the entire warm-up.
    var timeoutWO: Timeout.T = timeout()
    print(s"* Warm-up standard analysis: ")
    for (w <- 1 to warmup) {
      print(s"$w ")
      System.gc()
      val a = analysis(program)
      a.analyze(timeoutWO)
    }
    // Also warm-up the incremental analysis.
    timeoutWO = timeout()
    print(s"\n* Warm-up incremental analysis: ")
    for (w <- 1 to warmup) {
      print(s"$w ")
      System.gc()
      val a = analysis(program)
      a.updateAnalysis(timeoutWO)
    }
    // TODO: warm-up reanalysis (but may cause different JIT optimisations...?)

    // Actual measurements.

    var timesInitial:     List[Double] = List()
    var timesIncremental: List[Double] = List()
    var timesReanalysis:  List[Double] = List()

    var incrementalTimeout: Boolean = false
    var reanalysisTimeout:  Boolean = false

    print("\n Measuring: ")
    var to: Timeout.T = Timeout.none
    for (i <- 1 to actual) {

      print(s"$i")
      val a = analysis(program)
      System.gc()
      to = timeout()
      val tb = Timer.timeOnly({a.analyze(to)})
      if (to.reached) {
        // The base line analysis timed out. Abort.
        println("=====> TIMEOUT: base analysis <=====")
        results = results.add(file, "init", None).add(file, "incr", None).add(file, "rean", None)
        return
      }
      timesInitial = (tb.toDouble / 1000000) :: timesInitial

      print("*")
      if (!incrementalTimeout) {
        System.gc()
        to = timeout()
        val ti = Timer.timeOnly({a.updateAnalysis(to)})
        if (to.reached) {
          incrementalTimeout = true
        }
        timesIncremental = (ti.toDouble / 1000000) :: timesIncremental
      }

      print("* ")
      if (!reanalysisTimeout) {
        val a = analysis(program) // Create a new analysis and set the flag to "New".
        a.version = New
        System.gc()
        to = timeout()
        val tr = Timer.timeOnly({a.analyze(to)})
        if (to.reached) {
          reanalysisTimeout = true
        }
        timesReanalysis = (tr.toDouble / 1000000) :: timesReanalysis
      }
    }

    // Process statistics.
    val init = Statistics.all(timesInitial)
    val incr = Statistics.all(timesIncremental)
    val rean = Statistics.all(timesReanalysis)

    results = results.add(file, "init", Some(Math.round(init.mea).toInt))
      .add(file, "incr", if (incrementalTimeout) None else Some(Math.round(incr.mea).toInt))
      .add(file, "rean", if (reanalysisTimeout) None else Some(Math.round(rean.mea).toInt))

    println(s"\nInit: ${init.mea} - Incr: ${incr.mea} - Rean: ${rean.mea}\n")
  }

  def measure(): Unit = {
    benchmarks().foreach { b =>
      try {
        benchmark(b)
      } catch {
        case e: Exception => writeErrln(s"Running $b resulted in an exception: ${e.getMessage}")
        case e: VirtualMachineError => writeErrln(s"Running $b resulted in an error: ${e.getMessage}")
      }
      println()
    }
  }

  setDefaultWriter(open(s"benchOutput/results-incremental-performance-${System.currentTimeMillis()}.csv"))
  measure()
  val table: String = results.prettyString(format = {
    case None => "T"
    case Some(v) => v.toString
  })
  println(table)
  write(table)
  closeDefaultWriter()
}

object IncrementalSchemeModFPerformance extends IncrementalTime[SchemeExp] {
  override def benchmarks(): List[String] = List("test/changes/scheme/ring-rotate.scm")
  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFAnalysis(e)
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
}

object IncrementalSchemeModConcPerformance extends IncrementalTime[SchemeExp] {
  override def benchmarks(): List[String] = List(
    "test/changes/cscheme/threads/sudoku.scm",
    "test/changes/cscheme/threads/pc.scm")
  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcAnalysis(e)
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
}