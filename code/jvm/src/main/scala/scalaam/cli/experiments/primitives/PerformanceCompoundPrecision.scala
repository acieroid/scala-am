package scalaam.cli.experiments.primitives

import scalaam.bench.scheme.SchemeBenchmarkPrograms
import scalaam.cli.experiments._
import scalaam.language.scheme._
import scalaam.modular._
import scalaam.modular.scheme.modf.SchemeModFCompoundSensitivities.SeparateLowHighSensitivity._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.util.benchmarks.Timeout

import scala.concurrent.duration._

object PerformanceCompoundPrecision extends PerformanceBenchmarks {
  abstract class CompoundPrecisionAnalysis(p: SchemeExp) extends SimpleSchemeModFAnalysis(p) with SchemeConstantPropagationDomain with LIFOWorklistAlgorithm[SchemeExp] with StandardSchemeModFComponents
  override def analysisTime = Timeout.start(Duration(2, MINUTES))
  def benchmarks = SchemeBenchmarkPrograms.other
  def analyses: List[(SchemeExp => Analysis, String)] = List(
    (p => new CompoundPrecisionAnalysis(p) with S_0_0, "0_0"),
    (p => new CompoundPrecisionAnalysis(p) with S_CS_0, "CS_0"),
    (p => new CompoundPrecisionAnalysis(p) with S_2CS_0,"2CS_0"),
    (p => new CompoundPrecisionAnalysis(p) with S_2AcyclicCS_0, "2ACS_0"),
    (p => new CompoundPrecisionAnalysis(p) with S_10AcyclicCS_0, "10ACS_0"),
    (p => new CompoundPrecisionAnalysis(p) with S_10CS_0, "10_CS"),
    (p => new CompoundPrecisionAnalysis(p) with S_FA_0, "FA_0"),
    (p => new CompoundPrecisionAnalysis(p) with S_2FA_0, "2FA_0"),
    (p => new CompoundPrecisionAnalysis(p) with S_10FA_0, "10FA_0"),
    (p => new CompoundPrecisionAnalysis(p) with S_CSFA_0, "CSFA_0"),
  )
  def main(args: Array[String]) = run()
}
