package scalaam.cli.benchmarks.precision

import scalaam.cli.benchmarks._
import scalaam.util._
import scalaam.language.scheme._

import scala.concurrent.duration._
import scalaam.lattice._

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
    def otherAnalyses(): List[(SchemeExp => Analysis, String)]

    // and can, optionally, be configured in its timeouts (default: 10min.)
    def analysisTimeout() = Timeout.start(Duration(10, MINUTES)) //timeout for (non-base) analyses
    def concreteTimeout() = Timeout.none                         //timeout for concrete interpreter

    def concreteRuns() = 20

    // keep the results of the benchmarks in a table
    var results = Table.empty[Option[Int]]

    /**
      * For a given benchmark, compare each analysis with the base analysis
      * That is, count for each analysis how many values were refined w.r.t. the base analysis
      * All results are saved in the `result` table of this object
      * 
      * @param path the name of / path to the benchmark program
      * @param program the Scheme expression of the benchmark program
      */
    protected def forBenchmark(path: Benchmark, program: SchemeExp) = {
        // run the base analysis first
        val baseResult = runAnalysis(baseAnalysis, "base analysis", program, path).get // no timeout set for the base analysis!
        // run the other analyses on the benchmark
        otherAnalyses.foreach { case (analysis, name) =>
            val otherResult = runAnalysis(analysis, name, program, path, analysisTimeout())
            val refined = otherResult.map(store => compareOrdered(baseResult,store).size)
            results = results.add(path,name,refined)
        }
        // run a concrete interpreter on the benchmarks
        val concreteResult = runInterpreter(program, path, concreteTimeout(), concreteRuns())
        val refined = concreteResult.map(store => compareOrdered(baseResult,store).size)
        results = results.add(path,"concrete",refined)
    }
}

object AnalysisComparison1 extends AnalysisComparison[
    ConstantPropagation.I,
    ConstantPropagation.R,
    ConstantPropagation.B,
    ConstantPropagation.C,
    ConstantPropagation.S,
    Concrete.Sym
] {
    def baseAnalysis(prg: SchemeExp): Analysis = 
        SchemeAnalyses.contextInsensitiveAnalysis(prg)
    def otherAnalyses() = List(
        (SchemeAnalyses.adaptiveAnalysisPolicy3(_, 5), "adaptive-policy-3")
        //(SchemeAnalyses.fullArgContextSensitiveAnalysis, "full-arg")
        //SchemeAnalyses.adaptiveCallerSensitivity(prg,10)
        //SchemeAnalyses.adaptiveAnalysisPolicy1(prg, 5),
        //SchemeAnalyses.adaptiveAnalysisPolicy3(prg, 10)
    )

    def main(args: Array[String]) = runBenchmarks(Set(
        "test/icp/icp_2_aeval.scm"
    ))

    def check(path: Benchmark) = {
        val txt = Reader.loadFile(path)
        val prg = SchemeParser.parse(txt)
        val con = runInterpreter(prg, path).get
        val abs = runAnalysis(SchemeAnalyses.fullArgContextSensitiveAnalysis(_),"analysis",prg,path).get
        val allKeys = con.keys ++ abs.keys
        val interestingKeys = allKeys.filter(_.isInstanceOf[RetAddr])
        interestingKeys.foreach { k =>
            val absVal = abs.getOrElse(k,"⊥")
            val concVal = con.getOrElse(k,"⊥")
            if (absVal != concVal) {
                println(s"$k -> $absVal ; $concVal ")
            }
        }
    }

    def runBenchmarks(benchmarks: Set[Benchmark]) = {
        benchmarks.foreach(runBenchmark)
        println(results.prettyString(format = _.map(_.toString()).getOrElse("TIMEOUT")))
    }
}

