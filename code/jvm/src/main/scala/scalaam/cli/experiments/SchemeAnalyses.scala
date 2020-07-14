package scalaam.cli.experiments

import scalaam.core.Position._
import scalaam.language.scheme._
import scalaam.modular._
import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._

object SchemeAnalyses {

    def contextInsensitiveAnalysis(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                                          with SchemeModFNoSensitivity
                                                                          with SchemeConstantPropagationDomain
                                                                          with FIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "no-sensitivity"
    }
    def callSiteContextSensitiveAnalysis(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                                                with SchemeModFCallSiteSensitivity
                                                                                with SchemeConstantPropagationDomain
                                                                                with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "call-site-sensitivity"
    }
    def fullArgContextSensitiveAnalysis(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                                                with SchemeModFFullArgumentSensitivity
                                                                                with SchemeConstantPropagationDomain
                                                                                with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "full-argument-sensitivity"
    }
    def adaptiveAnalysisPolicy1(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)  with SchemeModFSemantics
                                                                                        with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveArgumentSensitivityPolicy1
                                                                                        with SchemeConstantPropagationDomain
                                                                                        with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "adaptive-argument-policy1"
        val limit = k
        override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
    def adaptiveAnalysisPolicy2(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)  with SchemeModFSemantics
                                                                                        with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveArgumentSensitivityPolicy2
                                                                                        with SchemeConstantPropagationDomain
                                                                                        with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "adaptive-argument-policy2"
        val budget = k
        override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
    def adaptiveAnalysisPolicy3(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)  with SchemeModFSemantics
                                                                                        with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveArgumentSensitivityPolicy3
                                                                                        with SchemeConstantPropagationDomain
                                                                                        with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "adaptive-argument-policy3"
        val limit = k
        override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
    def adaptiveCallerSensitivity(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg) with SchemeModFSemantics 
                                                                                         with AdaptiveSchemeModFSemantics
                                                                                         with AdaptiveCallerSensitivity
                                                                                         with SchemeConstantPropagationDomain
                                                                                         with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString() = "adaptive-caller"
        val limit = k
        override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = super.allocCtx(nam,clo,args,call,caller)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
    }
    def parallelAnalysis(prg: SchemeExp, n: Int) = new ModAnalysis(prg) with SchemeModFSemantics
                                                                        with StandardSchemeModFComponents 
                                                                        with BigStepModFSemantics
                                                                        with ParallelWorklistAlgorithm[SchemeExp]
                                                                        with SchemeModFNoSensitivity
                                                                        with SchemeConstantPropagationDomain {
      
      override def toString() = s"parallel (n = $n)"
      override def workers = n
      override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra with ParallelIntra
    }

}
