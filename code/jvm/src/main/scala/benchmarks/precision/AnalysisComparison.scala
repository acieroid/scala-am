package scalaam.cli.benchmarks.precision

import scalaam.cli.benchmarks._
import scalaam.util._
import scalaam.io.Reader
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
        val baseResult = runAnalysis(base, path).get // no timeout set for the base analysis!
        // run the other analyses on the benchmark
        val other = otherAnalyses(program)
        other.foreach { analysis =>
            val otherResult = runAnalysis(analysis, path, analysisTimeout())
            val refined = otherResult.map(store => compareOrdered(baseResult,store).size)
            benchmarkResults += (analysis.toString() -> refined)
        }
        // run a concrete interpreter on the benchmarks
        val concreteResult = runInterpreter(program, path, concreteTimeout(), 20)
        val refined = concreteResult.map(store => compareOrdered(baseResult,store).size)
        benchmarkResults += ("concrete" -> refined)
        // save the results
        this.results += (path -> benchmarkResults)
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

    def main(args: Array[String]) = check("test/primtest.scm")

    def check(path: Benchmark) = {
        val txt = Reader.loadFile(path)
        val prg = SchemeParser.parseAddPrelude(txt)
        val con = runInterpreter(prg, path).get
        val abs = runAnalysis(SchemeAnalyses.contextSensitiveAnalysis(prg),path).get
        val allKeys = con.keys ++ abs.keys
        val interestingKeys = allKeys.filter(_.isInstanceOf[RetAddr])
        interestingKeys.foreach { k =>
            println(s"$k -> ${abs.getOrElse(k,"⊥")} ; ${con.getOrElse(k,"⊥")} ")
        }
    }

    def runBenchmarks() = {
        SchemeBenchmarks.standard.foreach(runBenchmark)
        this.results.foreach { case (b, r) =>
            val fullArg = r.getOrElse("full-argument-sensitivity", "T")
            val concrete = r.getOrElse("concrete", "T")
            println(s"$b -> $fullArg/$concrete")
        }
    }
}


object PrimitivesComparisonRQ3NoSensitivity extends AnalysisComparison[
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
        SchemeAnalyses.PrimitivesComparison.S_0_0(prg),
        SchemeAnalyses.PrimitivesComparison.S_CS_0(prg),
        SchemeAnalyses.PrimitivesComparison.S_CSFA_0(prg),
    )

    def main(args: Array[String]) = runBenchmarks() // check("test/primtest.scm")

    def check(path: Benchmark) = {
        val txt = Reader.loadFile(path)
        val prg = SchemeParser.parseAddPrelude(txt)
        val con = runInterpreter(prg, path).get
        val abs = runAnalysis(baseAnalysis(prg),path).get
        val allKeys = con.keys ++ abs.keys
        val interestingKeys = allKeys.filter(_.isInstanceOf[RetAddr])
        interestingKeys.foreach { k =>
            println(s"$k -> ${abs.getOrElse(k,"⊥")} ; ${con.getOrElse(k,"⊥")} ")
        }
    }

    def runBenchmarks() = {
        SchemeBenchmarks.forPrimitives.take(3).foreach(runBenchmark)
        this.results.foreach { case (b, r) =>
            val refined_0_0 = r.getOrElse("0_0", "T")
            val refined_CS_0 = r.getOrElse("CS_0", "T")
            val refined_CSFA_0 = r.getOrElse("CSFA_0", "T")
            val concrete = r.getOrElse("concrete", "T")
            println(s"$b -> 0_0:$refined_0_0, CS_0:$refined_CS_0, CSFA_0: $refined_CSFA_0 / $concrete")
        }
    }
}

object PrimitivesComparisonRQ3ContextSensitive extends AnalysisComparison[
    ConstantPropagation.I,
    ConstantPropagation.R,
    Concrete.B,
    ConstantPropagation.C,
    ConstantPropagation.S,
    Concrete.Sym
] {
    def baseAnalysis(prg: SchemeExp): Analysis =
      SchemeAnalyses.callSiteContextSensitiveAnalysis(prg)
    def otherAnalyses(prg: SchemeExp) = List(
//      SchemeAnalyses.PrimitivesComparison.S_0_0(prg),
//      SchemeAnalyses.PrimitivesComparison.S_CS_0(prg),
      SchemeAnalyses.PrimitivesComparison.S_CS_CS(prg),
//      SchemeAnalyses.PrimitivesComparison.S_CSFA_0(prg),
      SchemeAnalyses.PrimitivesComparison.S_CSFA_CS(prg),
    )

    def main(args: Array[String]) = runBenchmarks() // check("test/primtest.scm")

    def check(path: Benchmark) = {
        val txt = Reader.loadFile(path)
        val prg = SchemeParser.parseAddPrelude(txt)
        val con = runInterpreter(prg, path).get
        val abs = runAnalysis(baseAnalysis(prg),path).get
        val allKeys = con.keys ++ abs.keys
        val interestingKeys = allKeys.filter(_.isInstanceOf[RetAddr])
        interestingKeys.foreach { k =>
            println(s"$k -> ${abs.getOrElse(k,"⊥")} ; ${con.getOrElse(k,"⊥")} ")
        }
    }

    def runBenchmarks() = {
        SchemeBenchmarks.forPrimitives.take(3).foreach(runBenchmark)
        this.results.foreach { case (b, r) =>
            val refined_0_0 = r.getOrElse("0_0", "T")
            val refined_CS_0 = r.getOrElse("CS_0", "T")
            val refined_CS_CS = r.getOrElse("CS_CS", "T")
            val refined_CSFA_0 = r.getOrElse("CSFA_0", "T")
            val refined_CSFA_CS = r.getOrElse("CSFA_CS", "T")
            val concrete = r.getOrElse("concrete", "T")
            println(s"$b -> 0_0:$refined_0_0, CS_0:$refined_CS_0, CS_CS:$refined_CS_CS CSFA_0: $refined_CSFA_0, CSFA_CS: $refined_CSFA_CS / $concrete")
        }
    }
}
