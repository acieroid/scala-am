package scalaam.cli.experiments.incremental

import scalaam.bench.scheme.IncrementalSchemeBenchmarkPrograms
import scalaam.cli.experiments.precision.PrecisionBenchmarks
import scalaam.core.Expression
import scalaam.language.CScheme.CSchemeParser
import scalaam.language.change.CodeVersion._
import scalaam.language.scheme.SchemeExp
import scalaam.modular.incremental.scheme.AnalysisBuilder._
import scalaam.util.Reader
import scalaam.util.Writer._
import scalaam.util.benchmarks.{Table, Timeout}

import scala.concurrent.duration.{Duration, MINUTES}

trait IncrementalPrecision[E <: Expression] extends IncrementalExperiment[E] {

  final val eq: String = "Equal"        // Precision of incremental update equals the one of a full reanalysis.
  final val mp: String = "More precise" // Precision of incremental update is better than the one of a full reanalysis.
  final val lp: String = "Less precise" // Precision of incremental update is lower than the one of a full reanalysis.

  var results: Table[String] = Table.empty

  def onBenchmark(file: String): Unit = {
    println(s"Testing $file")
    val program = parse(file)

    // Initial analysis: analyse + update.
    val a1 = analysis(program)

    // Base case: analysis of new program version.
    val a2 = analysis(program)
    a2.version = New

    var timeOut: Timeout.T = Timeout.none

    // Run the initial analysis.
    write(s"Running analysis on initial program ")
    timeOut = timeout()
    a1.analyze(timeOut)
    if (timeOut.reached) { // We do not use the test `a1.finished`, as even though the WL can be empty, an intra-component analysis may also have been aborted.
      writeln("timed out.")
      results = results.add(file, eq, "∞").add(file, mp, "∞").add(file, lp, "∞")
      return
    }
    writeln("finished.")

    // Update the initial analysis.
    write(s"Updating the analysis ")
    timeOut = timeout()
    a1.updateAnalysis(timeOut)
    if (timeOut.reached) {
      writeln("timed out.")
      results = results.add(file, eq, "∞").add(file, mp, "∞").add(file, lp, "∞")
      return
    }
    writeln("finished.")

    // Run a full reanalysis
    write(s"Reanalysing the program in full ")
    timeOut = timeout()
    a2.analyze(timeOut)
    if (timeOut.reached) {
      writeln("timed out.")
      results = results.add(file, eq, "∞").add(file, mp, "∞").add(file, lp, "∞")
      return
    }
    writeln("finished.")

    // Both analyses normally share the same lattice, allocation schemes,... which makes it unnecessary to convert values etc.
    val iStore = a1.store.withDefaultValue(a1.lattice.bottom)
    val rStore = a2.store.withDefaultValue(a2.lattice.bottom)
  //  println(iStore)
   // println(rStore)

    val allAddr = iStore.keySet ++ rStore.keySet
    var e = 0L
    var l = 0L
    var m = 0L
    allAddr.foreach({ a =>
      val incr = iStore(a)
      val rean = rStore(a)
      if (incr == rean) {
        e += 1 // Both results are the same => equally precise.
      } else if (a1.lattice.subsumes(incr, rean.asInstanceOf[a1.Value])) {
        l += 1 // The incremental value subsumes the value of the full reanalysis => less precise.
      } else {
        println(incr + " " + rean)
        m += 1 // The incremental value is subsumed by the value of the full reanalysis => more precise.
      }
    })
    results = results.add(file, eq, e.toString).add(file, mp, m.toString).add(file, lp, l.toString)
  }

  def reportError(file: String): Unit = results = results.add(file, eq, "E").add(file, mp, "E").add(file, lp, "E")
  val output: String = s"benchOutput/results-incremental-precision-${System.currentTimeMillis()}.txt"
  def createOutput(): String = results.prettyString(columns = List(eq, mp, lp))
}

object IncrementalSchemeModFPrecision extends IncrementalPrecision[SchemeExp] {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFAnalysis(e)
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
}

object IncrementalSchemeModConcPrecision extends IncrementalPrecision[SchemeExp] {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.concurrent
  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcAnalysis(e)
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
}

object IncrementalSchemeModXPrecision extends App {
  IncrementalSchemeModFPrecision.main(Array())
  IncrementalSchemeModConcPrecision.main(Array())
}