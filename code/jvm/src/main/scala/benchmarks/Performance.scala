package scalaam.cli.benchmarks

import scalaam.io._
import scalaam.io.Writer._
import scalaam.language.scheme._
import scalaam.modular.ModAnalysis
import scalaam.modular.scheme.CompoundSensitivities.SeparateLowHighSensitivity._
import scalaam.modular.scheme.CompoundSensitivities.SeparateLowHighSensitivity.Sensitivity._
import scalaam.modular.scheme._
import scalaam.util._
import scalaam.core._
import scala.concurrent.duration._

// TODO: rename to PerformanceEvaluation?
// TODO: move to evaluation package?
abstract class Performance extends App {

  // The number of warmup runs, for which the timing is discarded
  val warmup = 3
  // The actual number of runs made after `warmup` runs
  val actual = 15

  // The list of benchmarks used for the evaluation
  val benchmarks: List[String] = SchemeBenchmarks.standard.toList.take(1)

  type Analysis = ModAnalysis[SchemeExp]

  // The analyses that are evaluated (and their names)
  def analyses: List[(SchemeExp => Analysis, String)]

  // The timeout for the analyses
  // Unspecified because it make sense that all performance analyses choose their timeout.
  // Example: Timeout.start(Duration(2, MINUTES))
  def analysisTimeout(): Timeout.T

  // A variable that holds the results
  var results: Map[String, Map[String, Int]] = Map().withDefaultValue(Map())

  setDefaultWriter(open("benchOutput/results.txt"))

  // Runs a single analysis multiple times and returns the mean timing (in nanoseconds)
  def run(file: String, analysis: SchemeExp => Analysis): Double = {
    val program = SchemeParser.parse(Reader.loadFile(file))

    var times: List[Double] = List()

    // Warm-up.
    print(s"* Warmup (${warmup}) - ")
    for (i <- 1 to warmup) {
      print(s"$i ")
      System.gc() // It never hurts (hopefully, because it may cause GC errors...)
      analysis(program).analyze()
    }

    System.gc()

    print(s"\n* Time (${actual}) - ")
    for (i <- 1 to actual) {
      print(s"$i ")
      val a = analysis(program)
      System.gc()
      val t = Timer.timeOnly({a.analyze(analysisTimeout())})
      times = t.toDouble :: times
    }

    val m = Statistics.all(times)
    println(s"\n      Mean time: ${m.mea / 1000000}ms")
    println(s"      Min  time: ${m.min / 1000000}ms")
    println(s"      Max  time: ${m.max / 1000000}ms")
    println(s"      Med  time: ${m.med / 1000000}ms")
    println(s"         Stddev: ${m.std / 1000000}ms")

    m.mea
  }

  // Runs the evaluation
  def measure(): Unit = {
    benchmarks.foreach { b =>
      analyses.foreach { case (a, name) =>
        try {
          println(s"***** $b / $name *****")
          val time: Double = run(b, a)
          // We store the results in ms
          results = results + (b -> (results(b) + (name -> (time / 1000000).toInt)))
        } catch {
          case e: Exception => writeln(s"Running $b resulted in an exception: ${e.getMessage}")
          case e: VirtualMachineError => writeln(s"Running $b resulted in an error: ${e.getMessage}")
        }
      }
    }
  }

  measure()
  val table = LatexOutput.table[Int](results, "Benchmark", benchmarks, analyses.map(_._2), "T")
  write(table)
  closeDefaultWriter()
}
