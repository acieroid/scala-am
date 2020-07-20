package scalaam.cli.experiments.incremental

import scalaam.bench.scheme.IncrementalSchemeBenchmarkPrograms
import scalaam.core._
import scalaam.language.CScheme.CSchemeParser
import scalaam.language.change.CodeVersion._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives._
import scalaam.modular.incremental.scheme.AnalysisBuilder._
import scalaam.modular.scheme._
import scalaam.util.{Formatter, Reader}
import scalaam.util.Writer._
import scalaam.util.benchmarks._

import scala.concurrent.duration.{Duration, MINUTES}

trait IncrementalPrecision[E <: Expression] extends IncrementalExperiment[E] {

  final val eq: String = "Equal"        // Precision of incremental update equals the one of a full reanalysis.
  final val mp: String = "More precise" // Precision of incremental update is better than the one of a full reanalysis.
  final val lp: String = "Less precise" // Precision of incremental update is lower than the one of a full reanalysis.

  var results: Table[String] = Table.empty

  case class StubPrimitive(name: String) extends SchemePrimitive[Analysis#Value, Analysis#Addr] {
    def call(fpos: SchemeExp, args: List[(SchemeExp, Analysis#Value)],
             store: Store[Analysis#Addr,Analysis#Value],
             scheme: SchemeInterpreterBridge[Analysis#Addr]) =
      throw new Exception("Stub primitive: call not supported.")
  }

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
    writeln(s"finished, components: ${a1.visited.size}.")

    // Update the initial analysis.
    write(s"Updating the analysis ")
    timeOut = timeout()
    a1.updateAnalysis(timeOut)
    if (timeOut.reached) {
      writeln("timed out.")
      results = results.add(file, eq, "∞").add(file, mp, "∞").add(file, lp, "∞")
      return
    }
    writeln(s"finished, components: ${a1.visited.size}.")

    // Run a full reanalysis
    write(s"Reanalysing the program in full ")
    timeOut = timeout()
    a2.analyze(timeOut)
    if (timeOut.reached) {
      writeln("timed out.")
      results = results.add(file, eq, "∞").add(file, mp, "∞").add(file, lp, "∞")
      return
    }
    write(s"finished, components ${a2.visited.size}.")

    // Both analyses normally share the same lattice, allocation schemes,... which makes it unnecessary to convert values etc.
    val iStore = a1.store.withDefaultValue(a1.lattice.bottom)
    val rStore = a2.store.withDefaultValue(a2.lattice.bottom)

    val allAddr = iStore.keySet.filter(interestingAddress) ++ rStore.keySet.filter(interestingAddress)
    var e: Long = 0L
    var l: Long = 0L
    var m: Long = 0L
    val t: Long = allAddr.size
    allAddr.foreach({ a =>
      val incr = iStore(a)
      val rean = rStore(a)
      if (incr == rean)
        e += 1 // Both results are the same => equally precise.
      else if (a1.lattice.subsumes(incr, rean.asInstanceOf[a1.Value]))
        l += 1 // The incremental value subsumes the value of the full reanalysis => less precise.
      else
        m += 1 // The incremental value is subsumed by the value of the full reanalysis => more precise.
    })
    results = results.add(file, eq, Formatter.withPercent(e, t)).add(file, mp, Formatter.withPercent(m, t)).add(file, lp, Formatter.withPercent(l, t))
  }

  def interestingAddress[A <: Address](a: A): Boolean
  def reportError(file: String): Unit = results = results.add(file, eq, "E").add(file, mp, "E").add(file, lp, "E")
  val output: String = s"benchOutput/results-incremental-precision-${System.currentTimeMillis()}.txt"
  def createOutput(): String = results.prettyString(columns = List(eq, lp, mp))
}

trait IncrementalSchemePrecision extends IncrementalPrecision[SchemeExp] {
  override def interestingAddress[A <: Address](a: A): Boolean = a match {
    case PrmAddr(_) => false
    case _ => true
  }
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
}

object IncrementalSchemeModFPrecision extends IncrementalSchemePrecision {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFAnalysis(e)

}

object IncrementalSchemeModConcPrecision extends IncrementalSchemePrecision {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.concurrent
  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcAnalysis(e)
}

object IncrementalSchemeModXPrecision extends App {
  IncrementalSchemeModFPrecision.main(Array())
  IncrementalSchemeModConcPrecision.main(Array())
}