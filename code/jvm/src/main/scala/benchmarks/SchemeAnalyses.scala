package scalaam.cli.benchmarks

import scalaam.language.scheme._
import scalaam.language.scheme.primitives.SchemeLatticePrimitives
import scalaam.modular._
import scalaam.modular.scheme._
import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import scalaam.core.Position._

object SchemeAnalyses {

    def contextInsensitiveAnalysis(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                                          with BigStepSemantics
                                                                          with NoSensitivity
                                                                          with ConstantPropagationDomain {
        override def toString() = "no-sensitivity"
        val primitives = new SchemeLatticePrimitives[Value, Addr]
    }
    def contextSensitiveAnalysis(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                                        with BigStepSemantics
                                                                        with FullArgumentSensitivity
                                                                        with ConstantPropagationDomain {
        override def toString() = "full-argument-sensitivity"
        val primitives = new SchemeLatticePrimitives[Value, Addr]
    }
    def adaptiveAnalysisPolicy1(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)  with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveArgumentSensitivityPolicy1
                                                                                        with ConstantPropagationDomain {
        override def toString() = "adaptive-argument-policy1"
        val limit = k
        override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Option[ComponentContext]) = super.allocCtx(nam,clo,args,call,caller)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
        val primitives = new SchemeLatticePrimitives[Value, Addr]
    }
    def adaptiveAnalysisPolicy2(prg: SchemeExp, k: Int) = new AdaptiveModAnalysis(prg)  with AdaptiveSchemeModFSemantics
                                                                                        with AdaptiveArgumentSensitivityPolicy1
                                                                                        with ConstantPropagationDomain {
        override def toString() = "adaptive-argument-policy2"
        val limit = k
        override def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Option[ComponentContext]) = super.allocCtx(nam,clo,args,call,caller)
        override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
        val primitives = new SchemeLatticePrimitives[Value, Addr]
    }

  object PrimitivesComparison {
    def S_0_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                     with BigStepSemantics
                                                     with CompoundSensitivities.S_0_0
                                                     with ConstantPropagationDomain {
        override def toString() = "0_0"
        val primitives = new SchemeLatticePrimitives[Value, Addr]
    }
    def S_CS_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                      with BigStepSemantics
                                                      with CompoundSensitivities.S_CS_0
                                                      with ConstantPropagationDomain {
        override def toString() = "CS_0"
        val primitives = new SchemeLatticePrimitives[Value, Addr]
    }
    def S_CSFA_0(prg: SchemeExp) = new ModAnalysis(prg) with StandardSchemeModFSemantics
                                                        with BigStepSemantics
                                                        with CompoundSensitivities.S_CSFA_0
                                                        with ConstantPropagationDomain {
        override def toString() = "CSFA_0"
        val primitives = new SchemeLatticePrimitives[Value, Addr]
    }
  }
}
