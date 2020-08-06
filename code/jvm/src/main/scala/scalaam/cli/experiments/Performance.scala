package scalaam.cli.experiments

import scalaam.language.scheme._
import scalaam.language.CScheme._
import scalaam.modular.ModAnalysis
import scalaam.util._
import scalaam.util.benchmarks._

import scala.concurrent.duration._

// TODO: rename to PerformanceEvaluation?
// TODO: move to evaluation package?
trait PerformanceBenchmarks {

  type Analysis = ModAnalysis[SchemeExp]

  // Configuring the warm-up
  def maxWarmupRuns = 3                                     // maximum number of warm-up runs 
  def maxWarmupTime = Timeout.start(Duration(1, MINUTES))   // maximum time to spend on warm-up *in total* (i.e., for all runs)

  // Configuring the analysis runs
  def analysisRuns = 10                                     // number of analysis runs
  def analysisTime = Timeout.start(Duration(2, MINUTES))    // maximum time to spend *on a single analysis run*

  // The list of benchmarks used for the evaluation
  type Benchmark = String
  def benchmarks: Iterable[Benchmark]

  // The analyses that are evaluated (and their names)
  def analyses: List[(SchemeExp => Analysis, String)]

  // A variable that holds the results
  sealed trait PerformanceResult
  case class Completed(results: Statistics.Stats) extends PerformanceResult
  case object TimedOut extends PerformanceResult
  case object NoData extends PerformanceResult

  var results = Table.empty[PerformanceResult].withDefaultValue(NoData)

  def format(res: PerformanceResult): String = res match {
    case Completed(results) => scala.math.round(results.mean).toString
    case TimedOut           => "TIMEOUT"
    case NoData             => "_"
  }

  // Runs a single analysis multiple times and returns the mean timing (in milliseconds)
  def measureAnalysis(file: String, analysis: SchemeExp => Analysis): PerformanceResult = {
    // Parse the program
    val program = CSchemeParser.parse(Reader.loadFile(file))
    // Warm-up
    print(s"* WARM-UP ($maxWarmupRuns) - ")
    val warmupTimeout = maxWarmupTime
    for (i <- 1 to maxWarmupRuns) {
      print(s"$i ")
      System.gc() // It never hurts (hopefully, because it may cause GC errors...)
      analysis(program).analyze(warmupTimeout)
    }
    print("\n")
    // Actual timing
    print(s"* RUNS ($analysisRuns) - ")
    var times: List[Double] = List()
    for (i <- 1 to analysisRuns) {
      print(s"$i ")
      val a = analysis(program)
      System.gc()
      val t = Timer.timeOnly { a.analyze(analysisTime) }
      if (a.finished()) {
        times = (t.toDouble / 1000000) :: times
      } else {
        return TimedOut  // immediately return
      }
    }
    print("\n")
    // Compute, print and return the results
    val result = Statistics.all(times)
    println(result)
    Completed(result)
  }

  // Runs the evaluation
  def measureBenchmark(benchmark: Benchmark, timeoutFast: Boolean = true, failFast: Boolean = true): Unit =
    analyses.foreach { case (analysis, name) =>
      try {
        println(s"***** Running $name on $benchmark *****")
        val result = measureAnalysis(benchmark, analysis)
        results = results.add(benchmark, name, result)
        result match {
          case TimedOut if timeoutFast => return
          case _ => () 
        }
      } catch {
        case e: Exception =>
          println(s"Encountered an exception: ${e.getMessage}")
          if (failFast) return
        case e: VirtualMachineError => 
          System.gc()
          println(s"Running $benchmark resulted in an error: ${e.getMessage}")
          if (failFast) return
      }
    }

  def measureBenchmarks(timeoutFast: Boolean = true, failFast: Boolean = true) = 
    benchmarks.foreach(b => measureBenchmark(b, timeoutFast, failFast))

  def printResults() = 
    println(results.prettyString(format = format))
  def exportCSV(path: String) = {
    val hdl = Writer.openTimeStamped(path)
    val csv = results.toCSVString(format = format)
    Writer.write(hdl, csv)
    Writer.close(hdl) 
  } 

  def run(path: String = "benchOutput/performance/output.csv", timeoutFast: Boolean = true, failFast: Boolean = true) = {
    measureBenchmarks(timeoutFast, failFast)
    printResults()
    exportCSV(path)
  }
}

object ParallelModFPerformance extends PerformanceBenchmarks {
  def benchmarks = Set(
                      //"test/R5RS/mceval.scm"
                      "test/R5RS/gambit/nboyer.scm", 
                      //"test/R5RS/gambit/sboyer.scm", 
                      //"test/R5RS/gambit/scheme.scm",
                      //"test/R5RS/gambit/peval.scm",
                      //"test/R5RS/icp/icp_1c_ontleed.scm",
                      //"test/R5RS/icp/icp_1c_multiple-dwelling.scm",
                      //"test/R5RS/icp/icp_1c_prime-sum-pair.scm",
                      //"test/R5RS/icp/icp_3_leval.scm",
                      //"test/R5RS/icp/icp_7_eceval.scm",
                      //"test/R5RS/icp/icp_8_compiler.scm"
                    )
  def analyses: List[(SchemeExp => Analysis, String)] = List(
    (SchemeAnalyses.kCFAAnalysis(_, 0), "base ModF"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 1, 0), "parallel (n = 1)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 2, 0), "parallel (n = 2)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 4, 0), "parallel (n = 4)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 8, 0), "parallel (n = 8)"),
    //(SchemeAnalyses.parallelKCFAAnalysis(_, 16, 0), "parallel (n = 16)"),
    //(SchemeAnalyses.parallelKCFAAnalysis(_, 32, 0), "parallel (n = 32)"),
    //(SchemeAnalyses.parallelKCFAAnalysis(_, 64, 0), "parallel (n = 64)")
  )
  def main(args: Array[String]) = run()
}

object ParallelModConcPerformance extends PerformanceBenchmarks {
  def benchmarks = Set(
                      "test/concurrentScheme/threads/crypt.scm",
                      "test/concurrentScheme/threads/actors.scm",
                      "test/concurrentScheme/threads/matmul.scm",
                      "test/concurrentScheme/threads/minimax.scm",
                      "test/concurrentScheme/threads/msort.scm",
                      "test/concurrentScheme/threads/randomness2.scm",
                      "test/concurrentScheme/threads/sieve.scm",
                      "test/concurrentScheme/threads/stm.scm",
                      "test/concurrentScheme/threads/sudoku.scm",
                      "test/concurrentScheme/threads/tsp.scm",
                    )
  def analyses: List[(SchemeExp => Analysis, String)] = List(
    //(SchemeAnalyses.modConcAnalysis, "base ModConc"),
    (SchemeAnalyses.parallelModConc(_,1,1,5), "parallel (n = 1)"),
    (SchemeAnalyses.parallelModConc(_,2,2,5), "parallel (n = 2)"),
    (SchemeAnalyses.parallelModConc(_,4,4,5), "parallel (n = 4)"),
    (SchemeAnalyses.parallelModConc(_,8,8,5), "parallel (n = 8)"),
    //(SchemeAnalyses.parallelModConc(_,6), "parallel (n = 6)"),
    //(SchemeAnalyses.parallelModConc(_, 8), "parallel (n = 8)")
  )  
  def main(args: Array[String]) = run()
}