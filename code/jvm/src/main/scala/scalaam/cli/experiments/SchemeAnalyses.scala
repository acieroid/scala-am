package scalaam.cli.experiments

import scalaam.core.Position._
import scalaam.language.scheme._
import scalaam.modular._
import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.modular.scheme.modconc._

object SchemeAnalyses {

    def contextInsensitiveAnalysis(prg: SchemeExp) = new SimpleSchemeModFAnalysis(prg)
                                                        with SchemeModFNoSensitivity
                                                        with SchemeConstantPropagationDomain
                                                        with CallDepthFirstWorklistAlgorithm[SchemeExp] {
        override def toString() = "no-sensitivity"
    }
    def kCFAAnalysis(prg: SchemeExp, kcfa: Int) = new SimpleSchemeModFAnalysis(prg)
                                                    with SchemeModFKCallSiteSensitivity
                                                    with SchemeConstantPropagationDomain
                                                    with CallDepthFirstWorklistAlgorithm[SchemeExp] {
        override def toString() = s"kCFA (k = $kcfa)"
        val k = kcfa
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
    def parallelKCFAAnalysis(prg: SchemeExp, n: Int, kcfa: Int) = new ModAnalysis(prg)  with SchemeModFSemantics
                                                                                        with StandardSchemeModFComponents 
                                                                                        with BigStepModFSemantics
                                                                                        with ParallelWorklistAlgorithm[SchemeExp]
                                                                                        with SchemeModFKCallSiteSensitivity
                                                                                        with SchemeConstantPropagationDomain {
      
      override def toString() = s"parallel k-CFA (n = $n ; k = $kcfa)"
      val k = kcfa
      override def workers = n
      override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra with ParallelIntra
    }
    def modConcAnalysis(prg: SchemeExp) = new SimpleSchemeModConcAnalysis(prg)
                                            with SchemeModConcStandardSensitivity
                                            with SchemeConstantPropagationDomain
                                            with LIFOWorklistAlgorithm[SchemeExp] {
        override def toString = s"base modconc"
        override def modFAnalysis(intra: SchemeModConcIntra) = new InnerModFAnalysis(intra)
                                                                with SchemeModFKCallSiteSensitivity
                                                                with RandomWorklistAlgorithm[SchemeExp] {
            val k = 5
        }
    }
    def parallelModConc(prg: SchemeExp, n: Int, m: Int, kcfa: Int) = new SimpleSchemeModConcAnalysis(prg)
                                                                        with SchemeModConcStandardSensitivity
                                                                        with SchemeConstantPropagationDomain
                                                                        with ParallelWorklistAlgorithm[SchemeExp] { outer =>
        override def workers = n
        override def toString = s"parallel modconc (n = $workers)"  
        override def intraAnalysis(cmp: Component) = new SchemeModConcIntra(cmp) with ParallelIntra
        override def modFAnalysis(intra: SchemeModConcIntra) = new InnerModFAnalysis(intra)
                                                                with SchemeModFKCallSiteSensitivity
                                                                with ParallelWorklistAlgorithm[SchemeExp] {
            val k = kcfa
            override def workers = m
            override def intraAnalysis(cmp: SchemeModFComponent) = new InnerModFIntra(cmp) with ParallelIntra
        }
    }
}
