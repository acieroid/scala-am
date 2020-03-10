package scalaam.cli.benchmarks

import scalaam.cli._
import scalaam.core._
import scalaam.util._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.language.scheme._
import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import scala.concurrent.duration._


trait PrecisionBenchmarks {

    type Benchmark = String
    type Analysis = ModAnalysis[SchemeExp] with SchemeModFSemantics

    case class PrecisionResult(singletons: Int, total: Int)

    // the precision comparison is parameterized by:
    // - the benchmark programs you want to use
    // - the different analyses you want to compare
    def benchmarks: List[Benchmark]
    def analyses(prg: SchemeExp): List[Analysis]

    // and can, optionally, be configured in its timeout (default: 2min.)
    def timeoutDuration = Duration(2, MINUTES)

    // running the benchmarks
    private def runBenchmarks(): List[(Benchmark,Analysis)] =
        benchmarks.flatMap { path =>
            val txt = FileUtil.loadFile(path)
            val prg = SchemeParser.parse(txt)
            analyses(prg).map { analysis =>
                println(s"... analysing $path using $analysis ...")
                analysis.analyze(Timeout.start(timeoutDuration))
                (path, analysis)
            }
        }

    // extract information (= abstract value) per program point (only if the analysis terminated)
    trait Location
    private def extract(analysis: Analysis): Option[Map[Identity, analysis.Value]] =
        if (analysis.finished()) {
            val bindingsPerId = analysis.store.groupBy({_._1 match {
                case analysis.ComponentAddr(_, addr)    => addr.idn()
                case analysis.ReturnAddr(cmp)           => analysis.view(cmp).body.idn
            }})
            val valuesPerId = bindingsPerId.view.mapValues(_.values.foldLeft(analysis.lattice.bottom)((x,y) => analysis.lattice.join(x,y))).toMap
            Some(valuesPerId)
        } else {
            None
        }

    // comparing the results
    def comparePrecision(): Map[(Benchmark,Analysis),Option[PrecisionResult]] =
        runBenchmarks().map { case (path,analysis) => 
            val result = extract(analysis) match {
                case Some(mapping) =>
                    val singletons = mapping.count(bnd => analysis.lattice.cardinality(bnd._2) == CardinalityNumber(1))
                    Some(PrecisionResult(singletons, mapping.size))
                case None => None 
            }
            ((path,analysis),result)
        }.toMap
}

object Analyses {
    def contextInsensitiveAnalysis(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                                          with BigStepSemantics
                                                                          with NoSensitivity
                                                                          with ConstantPropagationDomain {
        override def toString() = "no-sensitivity"
    }
    def contextSensitiveAnalysis(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                                        with BigStepSemantics
                                                                        with FullArgumentSensitivity
                                                                        with ConstantPropagationDomain {
        override def toString() = "full-argument-sensitivity"
    }
    def adaptiveAnalysisPolicy1(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)  with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveArgumentSensitivityPolicy1
                                                                                        with EagerAdaptiveArgumentSelection
                                                                                        with ConstantPropagationDomain {
        override def toString() = "adaptive-argument-policy1"
        val limit = k
        override def allocCtx(clo: lattice.Closure, args: List[Value]) = super.allocCtx(clo,args)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
    def adaptiveAnalysisPolicy2(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)  with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveArgumentSensitivityPolicy2
                                                                                        with EagerAdaptiveArgumentSelection
                                                                                        with ConstantPropagationDomain {
        override def toString() = "adaptive-argument-policy2"
        val limit = k
        override def allocCtx(clo: lattice.Closure, args: List[Value]) = super.allocCtx(clo,args)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
}

object PrecisionComparison1 extends PrecisionBenchmarks {
    def benchmarks = List(
        "test/gambit/nqueens.scm"
    )
    def analyses(prg: SchemeExp) = List(
        //Analyses.contextInsensitiveAnalysis(prg),
        //Analyses.contextSensitiveAnalysis(prg)
        //Analyses.adaptiveAnalysisPolicy1(prg, 10)
        Analyses.adaptiveAnalysisPolicy2(prg, 10)
    )
}

object PrecisionComparisonMain {
    def main(args: Array[String]) = PrecisionComparison1.comparePrecision().foreach {
        case ((benchmark,analysis), result) =>
            print(s"==> $benchmark using $analysis: ")
            result match {
                case Some(PrecisionComparison1.PrecisionResult(singletons,total)) => println(s"$singletons/$total")
                case _ => println("TIMEOUT")
            }
    }
}