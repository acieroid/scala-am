package scalaam.cli.benchmarks

import scalaam.language.scheme._
import scalaam.language.CScheme._
import scalaam.modular.ModAnalysis
import scalaam.util.Writer._
import scalaam.util._
import scalaam.util.benchmarks._

import scala.concurrent.duration._

// TODO: rename to PerformanceEvaluation?
// TODO: move to evaluation package?
abstract class Performance extends App {

  // The number of warmup runs, for which the timing is discarded
  val warmup = 5
  // The actual number of runs made after `warmup` runs
  val actual = 15

  // The list of benchmarks used for the evaluation
  def benchmarks: List[String] = SchemeBenchmarks.standard.toList.take(1)

  type Analysis = ModAnalysis[SchemeExp]

  // The analyses that are evaluated (and their names)
  def analyses: List[(SchemeExp => Analysis, String)]

  // The timeout for the analyses
  // Unspecified because it make sense that all performance analyses choose their timeout.
  // Example: Timeout.start(Duration(2, MINUTES))
  def analysisTimeout(): Timeout.T

  // A variable that holds the results
  var results = Table.empty[Option[Int]]

  // Runs a single analysis multiple times and returns the mean timing (in nanoseconds)
  def run(file: String, analysis: SchemeExp => Analysis): Option[Double] = {
    val program = CSchemeParser.parse(Reader.loadFile(file))

    var times: List[Double] = List()
    var timeout: Boolean = false

    // Warm-up.
    val warmupTimeout = analysisTimeout()
    print(s"* Warmup (${warmup}) - ")
    for (i <- 1 to warmup) {
      print(s"$i ")
      System.gc() // It never hurts (hopefully, because it may cause GC errors...)
      analysis(program).analyze(warmupTimeout)
    }

    print(s"\n* Time (${actual}) - ")
    for (i <- 1 to actual) {
      if (!timeout) { // Only run if there has been no timeout
        print(s"$i ")
        val a = analysis(program)
        System.gc()
        val t = Timer.timeOnly({a.analyze(analysisTimeout())})
        if (a.finished()) {
          times = (t.toDouble / 1000000) :: times
        } else {
          // Analysis timed out
          timeout = true
        }
      }
    }
    print(s"\n")
    if (timeout) {
      println(s"TIMED OUT. Times that succeeded: $times")
      None
    } else {
      val m = Statistics.all(times)
      println(m.toString)
      Some(m.mea)
    }
  }

  // Runs the evaluation
  def measure(): Unit = {
    benchmarks.foreach { b =>
      analyses.foreach { case (a, name) =>
        try {
          println(s"***** $b / $name *****")
          val time = run(b, a)
          // We store the results in ms
          results = results.add(b, name, time.map(_.toInt))
        } catch {
          case e: Exception => writeln(s"Running $b resulted in an exception: ${e.getMessage}")
          case e: VirtualMachineError => writeln(s"Running $b resulted in an error: ${e.getMessage}")
        }
      }
    }
  }

  // TODO: clean up the Writer interface
  setDefaultWriter(open(s"benchOutput/results-${System.currentTimeMillis()}.csv"))
  measure()
  val table = results.prettyString(format = {
    case None => "T"
    case Some(v) => v.toString()
  })
  println(table)
  write(table)
  closeDefaultWriter()
}

object SimplePerformance extends Performance {
  def benchmarksSet = Set(
      "test/R5RS/gambit/scheme.scm",
      "test/R5RS/gambit/sboyer.scm",
      "test/R5RS/gambit/nboyer.scm"
    )
  override def benchmarks: List[String] = benchmarksSet.toList
  def analysisTimeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
  def analyses: List[(SchemeExp => Analysis, String)] = List(
    (SchemeAnalyses.contextInsensitiveAnalysis, "base"),
    (SchemeAnalyses.parallelAnalysis(_,1), "parallel (n = 1)"),
    (SchemeAnalyses.parallelAnalysis(_,2), "parallel (n = 2)"),
    (SchemeAnalyses.parallelAnalysis(_,4), "parallel (n = 4)"),
    (SchemeAnalyses.parallelAnalysis(_,6), "parallel (n = 6)"),
    (SchemeAnalyses.parallelAnalysis(_,8), "parallel (n = 8)")
  )
}