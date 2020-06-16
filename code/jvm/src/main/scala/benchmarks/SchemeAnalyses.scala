package scalaam.cli.benchmarks

import scalaam.language.scheme._
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import scalaam.core.Position._

object SchemeAnalyses {

    def contextInsensitiveAnalysis(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                                          with BigStepModFSemantics
                                                                          with NoSensitivity
                                                                          with ConstantPropagationDomain
                                                                          with FIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "no-sensitivity"
    }
    def callSiteContextSensitiveAnalysis(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                                                with BigStepModFSemantics
                                                                                with CallSiteSensitivity
                                                                                with ConstantPropagationDomain
                                                                                with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "call-site-sensitivity"
    }
    def fullArgContextSensitiveAnalysis(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                                                with BigStepModFSemantics
                                                                                with FullArgumentSensitivity
                                                                                with ConstantPropagationDomain
                                                                                with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "full-argument-sensitivity"
    }
    def adaptiveAnalysisPolicy1(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)  with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveArgumentSensitivityPolicy1
                                                                                        with ConstantPropagationDomain
                                                                                        with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "adaptive-argument-policy1"
        val limit = k
        override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
    def adaptiveAnalysisPolicy2(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)  with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveArgumentSensitivityPolicy2
                                                                                        with ConstantPropagationDomain
                                                                                        with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "adaptive-argument-policy2"
        val budget = k
        override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
    def adaptiveAnalysisPolicy3(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)  with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveArgumentSensitivityPolicy3
                                                                                        with ConstantPropagationDomain
                                                                                        with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "adaptive-argument-policy3"
        val limit = k
        override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
    def adaptiveCallerSensitivity(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg) with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveCallerSensitivity
                                                                                        with ConstantPropagationDomain
                                                                                        with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "adaptive-caller"
        val limit = k
        override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
    def parallelAnalysis(prg: SchemeExp, n: Int) = new ModAnalysis(prg) with StandardSchemeModFSemantics 
                                                                        with BigStepModFSemantics
                                                                        with ParallelWorklistAlgorithm[SchemeExp]
                                                                        with NoSensitivity
                                                                        with ConstantPropagationDomain {
      
      override def toString() = s"parallel (n = $n)"
      override def workers = n
      override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
      override def intraAnalysis(cmp: Component) = new BigStepModFIntra(cmp) with ParallelIntra
    }

}
