package scalaam.cli.experiments.incremental

import scalaam.bench.scheme.IncrementalSchemeBenchmarkPrograms
import scalaam.core._
import scalaam.language.CScheme.CSchemeParser
import scalaam.language.change.CodeVersion._
import scalaam.language.scheme._
import scalaam.modular.incremental.scheme.AnalysisBuilder._
import scalaam.modular.scheme._
import scalaam.util._
import scalaam.util.Writer._
import scalaam.util.benchmarks._

import scala.concurrent.duration._

trait IncrementalProperties[E <: Expression] extends IncrementalExperiment[E] {

  final val in: String = " (init)"
  final val up: String = " (incr)"
  final val re: String = " (rean)"

  final val al = List(in, up, re)

  final val co: String = "#Components"
  final val an: String = "#Analyses"
  final val ad: String = "|Store|"
  final val dp: String = "#Dependencies"

  final val pr = List(co, an, ad, dp)

  var results: Table[String] = Table.empty.withDefaultValue(" ")

  def onBenchmark(file: String): Unit = {
    print(s"Testing $file ")
    val program = parse(file)

    // Initial analysis: analyse + update.
    val a1 = analysis(program)

    // Base case: analysis of new program version.
    val a2 = analysis(program)
    a2.version = New

    var timeOut: Timeout.T = Timeout.none

    // Run the initial analysis.
    write(s"init ")
    timeOut = timeout()
    a1.analyze(timeOut)
    if (timeOut.reached) { // We do not use the test `a1.finished`, as even though the WL can be empty, an intra-component analysis may also have been aborted.
      writeln("timed out.")
      results = results
        .add(file, co + in, "∞")
        .add(file, an + in, "∞")
        .add(file, ad + in, "∞")
        .add(file, dp + in, "∞")
      return
    }
    val vis = a1.visited.size
    val ssi = a1.store.keySet.size
    val dep = a1.deps.values.map(_.size).sum
    results = results
      .add(file, co + in, vis.toString)
      .add(file, an + in, "?")
      .add(file, ad + in, ssi.toString)
      .add(file, dp + in, dep.toString)

    // Update the initial analysis.
    write(s"-> incr ")
    timeOut = timeout()
    a1.updateAnalysis(timeOut)
    if (timeOut.reached) {
      writeln("timed out.")
      results = results
        .add(file, co + up, "∞")
        .add(file, an + up, "∞")
        .add(file, ad + up, "∞")
        .add(file, dp + up, "∞")
      return
    }
    results = results
      .add(file, co + up, s"${a1.visited.size} (+${a1.visited.size - vis})")
      .add(file, an + up, "?")
      .add(file, ad + up, s"${a1.store.keySet.size} (+${a1.store.keySet.size - ssi})")
      .add(file, dp + up, s"${a1.deps.values.map(_.size).sum} (+${a1.deps.values.map(_.size).sum - dep})")

    // Run a full reanalysis
    write(s"-> rean ")
    timeOut = timeout()
    a2.analyze(timeOut)
    if (timeOut.reached) {
      write("timed out.")
      results = results
        .add(file, co + re, "∞")
        .add(file, an + re, "∞")
        .add(file, ad + re, "∞")
        .add(file, dp + re, "∞")
      return
    }
    results = results
      .add(file, co + re, a2.visited.size.toString)
      .add(file, an + re, "?")
      .add(file, ad + re, a2.store.keySet.size.toString)
      .add(file, dp + re, a2.deps.values.map(_.size).sum.toString)
  }

  def interestingAddress[A <: Address](a: A): Boolean
  def reportError(file: String): Unit = dp.foreach(d => al.foreach(a => results = results.add(file, d + a, "E")))
  def createOutput(): String = results.prettyString(columns = List(ad + in, ad + up, ad + re, co + in, co + up, co + re, dp + in, dp + up, dp + re, an + in, an + up, an + re))
}

trait IncrementalSchemeProperties extends IncrementalProperties[SchemeExp] {
  override def interestingAddress[A <: Address](a: A): Boolean = a match {
    case PrmAddr(_) => false
    case _ => true
  }
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
}

object IncrementalSchemeModFProperties extends IncrementalSchemeProperties {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFAnalysis(e)
  val outputFile: String = s"results-incremental-ModF-properties.txt"

}

object IncrementalSchemeModConcProperties extends IncrementalSchemeProperties {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.concurrent
  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcAnalysis(e)
  val outputFile: String = s"results-incremental-ModConc-properties.txt"
}

object IncrementalSchemeModXProperties {
  def main(args: Array[String]): Unit = {
    IncrementalSchemeModFProperties.main(args)
    IncrementalSchemeModConcProperties.main(args)
  }
}