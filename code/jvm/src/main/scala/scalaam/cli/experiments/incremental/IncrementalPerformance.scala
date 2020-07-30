package scalaam.cli.experiments.incremental

import scalaam.bench.scheme.IncrementalSchemeBenchmarkPrograms
import scalaam.core.Expression
import scalaam.language.CScheme.CSchemeParser
import scalaam.language.change.CodeVersion._
import scalaam.language.scheme.SchemeExp
import scalaam.modular.incremental.scheme.AnalysisBuilder._
import scalaam.util.Reader
import scalaam.util.benchmarks._

import scala.concurrent.duration._

trait IncrementalTime[E <: Expression] extends IncrementalExperiment[E] {

  // The maximal number of warm-up runs.
  val maxWarmupRuns = 5
  // The number of actually measured runs.
  val  measuredRuns = 15

  // The results of the evaluation.
  sealed trait Result
  case class Finished(mean: Long, stddev: Long) extends Result { override def toString: String = s"$mean±$stddev" }
  case object Timedout extends Result { override def toString: String = "∞" }
  case object NotRun   extends Result { override def toString: String = " " }
  case object Errored  extends Result { override def toString: String = "E" }

  var results: Table[Result] = Table.empty.withDefaultValue(NotRun)

  // A single program run with the analysis.
  def onBenchmark(file: String): Unit = {
    println(s"Testing $file")
    val program = parse(file)

    // Warm-up.

    // Use the same timeout for the entire warm-up.
    var timeoutWarmup: Timeout.T = timeout()
    var analyses: List[Analysis] = List()
    print(s"* Warm-up standard analysis (max. $maxWarmupRuns): ")
    for (w <- 1 to maxWarmupRuns) {
      print(s"$w ")
      System.gc()
      val a = analysis(program)
      a.analyze(timeoutWarmup)
      analyses = a :: analyses
    }
    print(s"\n* Warm-up incremental analysis (max. ${analyses.length}): ")
    timeoutWarmup = timeout()
    for (a <- analyses) {
      print(s"* ")
      System.gc()
      a.updateAnalysis(timeoutWarmup) // We need an analysis that has already been (partially) run.
    }

    // Actual measurements.

    var timesInitial:     List[Double] = List()
    var timesIncremental: List[Double] = List()
    var timesReanalysis:  List[Double] = List()

    var incrementalTimeout: Boolean = false
    var reanalysisTimeout:  Boolean = false

    print("\n* Measuring: ")
    var to: Timeout.T = Timeout.none
    for (i <- 1 to measuredRuns) {

      print(s"$i")
      val a = analysis(program)
      System.gc()
      to = timeout()
      val tb = Timer.timeOnly({a.analyze(to)})
      if (to.reached) {
        // The base line analysis timed out. Abort.
        println(" => Base analysis timed out.")
        results = results.add(file, "init", Timedout).add(file, "incr", NotRun).add(file, "rean", NotRun)
        return
      }
      timesInitial = (tb.toDouble / 1000000) :: timesInitial

      print(if (incrementalTimeout) "x" else "*")
      if (!incrementalTimeout) {
        System.gc()
        to = timeout()
        val ti = Timer.timeOnly({a.updateAnalysis(to)})
        if (to.reached) {
          incrementalTimeout = true
        }
        timesIncremental = (ti.toDouble / 1000000) :: timesIncremental
      }

      print(if (reanalysisTimeout) "x " else "* ")
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

    results = results
      .add(file, "init", Finished(Math.round(init.mea), Math.round(init.std)))
      .add(file, "incr", if (incrementalTimeout) Timedout else Finished(Math.round(incr.mea), Math.round(incr.std)))
      .add(file, "rean", if ( reanalysisTimeout) Timedout else Finished(Math.round(rean.mea), Math.round(rean.std)))

    println(s"\n    => Init: ${init.mea} - Incr: ${incr.mea} - Rean: ${rean.mea}")
  }

  def reportError(file: String): Unit = results = results.add(file, "init", Errored).add(file, "incr", Errored). add(file, "rean", Errored)
  def createOutput(): String = results.prettyString(columns = List("init", "rean", "incr"))
}

object IncrementalSchemeModFPerformance extends IncrementalTime[SchemeExp] {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFAnalysis(e)
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
  val output: String = s"benchOutput/results-incremental-ModF-performance.txt"
}

object IncrementalSchemeModConcPerformance extends IncrementalTime[SchemeExp] {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.concurrent
  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcAnalysis(e)
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
  val output: String = s"benchOutput/results-incremental-ModConc-performance.txt"
}

object IncrementalSchemeModXPerformance {
  def main(args: Array[String]): Unit = {
    IncrementalSchemeModFPerformance.main(args)
    IncrementalSchemeModConcPerformance.main(args)
  }
}