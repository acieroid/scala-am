package scalaam.cli.benchmarks

import scalaam.language.scheme._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._

object SchemeAnalyses {

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
                                                                                        with ConstantPropagationDomain {
        override def toString() = "adaptive-argument-policy1"
        val limit = k
        override def allocCtx(clo: lattice.Closure, args: List[Value]) = super.allocCtx(clo,args)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
}