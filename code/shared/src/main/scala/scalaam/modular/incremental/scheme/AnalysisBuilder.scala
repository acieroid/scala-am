package scalaam.modular.incremental.scheme

import scalaam.language.scheme.SchemeExp
import scalaam.modular._
import scalaam.modular.incremental.scheme.modconc._
import scalaam.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import scalaam.modular.scheme.SchemeTypeDomain
import scalaam.modular.scheme.modf._
import scalaam.modular.scheme.ssmodconc._

object AnalysisBuilder {

  class IncrementalModConcAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                      with KCFAModConc
                                                      with IncrementalSchemeModConcSmallStepSemantics
                                                      with LIFOWorklistAlgorithm[SchemeExp]
                                                      with SchemeTypeDomain {
    val k = 1
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra
  }

  class IncrementalSchemeModFAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                         with StandardSchemeModFComponents
                                                         with SchemeModFNoSensitivity
                                                         with SchemeModFSemantics
                                                         with LIFOWorklistAlgorithm[SchemeExp]
                                                         with SchemeTypeDomain
                                                         with IncrementalSchemeModFBigStepSemantics {
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra
  }
}
