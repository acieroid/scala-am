package scalaam.cli.experiments.incremental

import scalaam.bench.scheme.IncrementalSchemeBenchmarkPrograms
import scalaam.core.Expression
import scalaam.language.CScheme.CSchemeParser
import scalaam.language.change.CodeVersion._
import scalaam.language.scheme.SchemeExp
import scalaam.modular.incremental.scheme.AnalysisBuilder._
import scalaam.util.Reader
import scalaam.util.Writer._
import scalaam.util.benchmarks._

import scala.concurrent.duration._

trait IncrementalTime[E <: Expression] extends IncrementalExperiment[E] {

  // The maximal number of warm-up runs.
  val maxWarmupRuns = 5
  // The number of actually measured runs.
  val  measuredRuns = 30

  // The results of the evaluation.
  sealed trait Result
  case class Finished(mean: Long, stddev: Long) extends Result { override def toString: String = s"$mean±$stddev" }
  case object Timedout extends Result { override def toString: String = "∞" }
  case object NotRun   extends Result { override def toString: String = " " }
  case object Errored  extends Result { override def toString: String = "E" }

  var results: Table[Result] = Table.empty.withDefaultValue(NotRun)

  // A single program run with the analysis.
  def onBenchmark(file: String): Unit = {
    writeln(s"Testing $file")
    val program = parse(file)

    // Warm-up.

    // Use the same timeout for the entire warm-up.
    var timeoutWarmup: Timeout.T = timeout()
    var analyses: List[Analysis] = List()
    write(s"* Warm-up standard analysis (max. $maxWarmupRuns): ")
    for (w <- 1 to maxWarmupRuns) {
      write(s"$w ")
      System.gc()
      val a = analysis(program)
      a.analyze(timeoutWarmup)
      analyses = a :: analyses
    }
    write(s"\n* Warm-up incremental analysis (max. ${analyses.length}): ")
    timeoutWarmup = timeout()
    for (a <- analyses) {
      write(s"* ")
      System.gc()
      a.updateAnalysis(timeoutWarmup) // We need an analysis that has already been (partially) run.
    }

    // Actual measurements.

    var timesInitial:     List[Double] = List()
    var timesIncremental: List[Double] = List()
    var timesReanalysis:  List[Double] = List()

    var incrementalTimeout: Boolean = false
    var reanalysisTimeout:  Boolean = false

    write("\n* Measuring: ")
    var to: Timeout.T = Timeout.none
    for (i <- 1 to measuredRuns) {

      write(s"$i")
      val a = analysis(program)
      System.gc()
      to = timeout()
      val tb = Timer.timeOnly({a.analyze(to)})
      if (to.reached) {
        // The base line analysis timed out. Abort.
        writeln(" => Base analysis timed out.")
        results = results.add(file, "init", Timedout).add(file, "incr", NotRun).add(file, "rean", NotRun)
        return
      }
      timesInitial = (tb.toDouble / 1000000) :: timesInitial

      write(if (incrementalTimeout) "x" else "*")
      if (!incrementalTimeout) {
        System.gc()
        to = timeout()
        val ti = Timer.timeOnly({a.updateAnalysis(to)})
        if (to.reached) {
          incrementalTimeout = true
        }
        timesIncremental = (ti.toDouble / 1000000) :: timesIncremental
      }

      write(if (reanalysisTimeout) "x " else "* ")
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
      .add(file, "init", Finished(scala.math.round(init.mean), scala.math.round(init.stddev)))
      .add(file, "incr", if (incrementalTimeout) Timedout else Finished(scala.math.round(incr.mean), scala.math.round(incr.stddev)))
      .add(file, "rean", if ( reanalysisTimeout) Timedout else Finished(scala.math.round(rean.mean), scala.math.round(rean.stddev)))

    writeln(s"\n    => Init: ${init.mean} - Incr: ${incr.mean} - Rean: ${rean.mean}")
  }

  def reportError(file: String): Unit = results = results.add(file, "init", Errored).add(file, "incr", Errored). add(file, "rean", Errored)
  def createOutput(): String = results.prettyString(columns = List("init", "rean", "incr"))
}

object IncrementalSchemeModFPerformance extends IncrementalTime[SchemeExp] {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFAnalysis(e)
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(10, MINUTES))
  val outputFile: String = s"ModF-performance.txt"
}

object IncrementalSchemeModConcPerformance extends IncrementalTime[SchemeExp] {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.concurrent
  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcAnalysis(e)
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(10, MINUTES))
  val outputFile: String = s"ModConc-performance.txt"
}

object IncrementalSchemeModXPerformance {
  def main(args: Array[String]): Unit = {
    IncrementalSchemeModFPerformance.main(args)
    IncrementalSchemeModConcPerformance.main(args)
  }
}