package scalaam.cli.experiments.incremental

import scalaam.core.Expression
import scalaam.modular.GlobalStore
import scalaam.modular.incremental.IncrementalModAnalysis
import scalaam.util.Writer._
import scalaam.util.benchmarks.Timeout

trait IncrementalExperiment[E <: Expression] {
  // A list of programs on which the benchmark should be executed.
  def benchmarks(): Set[String]

  // Type of an analysis.
  type Analysis = IncrementalModAnalysis[E] with GlobalStore[E]

  // Analysis construction.
  def analysis(e: E): Analysis

  // Parsing.
  def parse(string: String): E

  // The timeout to be used. The timeout also indicates half of the time maximally spent on warm-up.
  def timeout(): Timeout.T

  // What is to be done for each benchmark program.
  def onBenchmark(file: String): Unit

  // Modifies the result to report an error during the experiments.
  def reportError(file: String): Unit

  // Where to write the results.
  val outputDir:  String = "benchOutput"
  val outputFile: String

  // Creates a string representation of the final output.
  def createOutput(): String

  def measure(): Unit = {
    benchmarks().foreach { file =>
      try {
        onBenchmark(file)
      } catch {
        case e: Exception => writeErrln(s"Running $file resulted in an exception: ${e.getMessage}\n")
          reportError(file)
        case e: VirtualMachineError => writeErrln(s"Running $file resulted in an error: ${e.getMessage}\n")
          reportError(file)
      }
      writeln()
    }
  }

  def main(args: Array[String]): Unit = {
    setDefaultWriter(openTimeStamped(outputDir, outputFile))
    enableReporting()
    measure()
    val out: String = createOutput()
    writeln(out)
    closeDefaultWriter()
    disableReporting()
  }
}
