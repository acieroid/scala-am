package scalaam.cli.benchmarks.precision

import scalaam.cli.benchmarks._
import scalaam.util._
import scalaam.language.scheme._
import scala.concurrent.duration._
import scalaam.lattice._
import scalaam.cli.benchmarks.SchemeBenchmarks

abstract class AnalysisComparison[
    Num: IntLattice,
    Rea: RealLattice,
    Bln: BoolLattice,
    Chr: CharLattice,
    Str: StringLattice, 
    Smb: SymbolLattice
] extends PrecisionBenchmarks {

    // the precision comparison is parameterized by:
    // - the base analysis (= lowest precision) to compare to
    // - the other analyses to compare to the base analysis
    def baseAnalysis(prg: SchemeExp): Analysis
    def otherAnalyses(prg: SchemeExp): List[Analysis]

    // and can, optionally, be configured in its timeouts (default: 2min.)
    def analysisTimeout() = Timeout.start(Duration(2, MINUTES)) //timeout for (non-base) analyses
    def concreteTimeout() = Timeout.none                        //timeout for concrete interpreter

    // keep the results of the benchmarks
    var results: Map[Benchmark, Map[String,Option[Int]]] = Map.empty

    /**
      * For a given benchmark, compare each analysis with the base analysis
      * That is, count for each analysis how many values were refined w.r.t. the base analysis
      * All results are saved in the `result` table of this object
      * 
      * @param path the name of / path to the benchmark program
      * @param program the Scheme expression of the benchmark program
      */
    protected def forBenchmark(path: Benchmark, program: SchemeExp) = {
        // keep the results of the benchmark here
        var benchmarkResults = Map.empty[String,Option[Int]]
        // run the base analysis first
        val base = baseAnalysis(program)
        val baseResult = runAnalysis(base, path, program).get // no timeout set for the base analysis!
        // run the other analyses on the benchmark
        val other = otherAnalyses(program)
        other.foreach { analysis =>
            val otherResult = runAnalysis(analysis, path, program, analysisTimeout())
            val refined = otherResult.map(store => compareOrdered(baseResult,store).size)
            benchmarkResults += (analysis.toString() -> refined)
        }
        // run a concrete interpreter on the benchmarks
        val concreteResult = runInterpreter(program, path, concreteTimeout(), 20)
        val refined = concreteResult.map(store => compareOrdered(baseResult,store).size)
        benchmarkResults += ("concrete" -> refined)
    }
}

object AnalysisComparison1 extends AnalysisComparison[
    ConstantPropagation.I,
    ConstantPropagation.R,
    Concrete.B,
    ConstantPropagation.C,
    ConstantPropagation.S,
    Concrete.Sym
] {
    def baseAnalysis(prg: SchemeExp): Analysis = 
        SchemeAnalyses.contextInsensitiveAnalysis(prg)
    def otherAnalyses(prg: SchemeExp) = List(
        SchemeAnalyses.contextSensitiveAnalysis(prg)
        //Analyses.adaptiveAnalysisPolicy1(prg, 10)
        //Analyses.adaptiveAnalysisPolicy2(prg, 10)
    )

    def main(args: Array[String]) =
        SchemeBenchmarks.standard.foreach(runBenchmark)
}