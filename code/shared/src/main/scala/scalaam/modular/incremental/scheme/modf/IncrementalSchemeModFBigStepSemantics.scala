package scalaam.modular.incremental.scheme.modf

import scalaam.language.change.CodeChange
import scalaam.language.scheme.SchemeExp
import scalaam.modular.ModAnalysis
import scalaam.modular.incremental.scheme.IncrementalSchemeSemantics
import scalaam.modular.scheme.modf._
import scalaam.language.change.CodeVersion._

trait IncrementalSchemeModFBigStepSemantics extends BigStepModFSemantics with IncrementalSchemeSemantics {
  trait IncrementalSchemeModFBigStepIntra extends BigStepModFIntra {
    override protected def eval(exp: SchemeExp, env: Env): Value = exp match {
      case CodeChange(old, nw, _) => registerAffected(component); if (version == Old) eval(old, env) else eval(nw, env)
      case _                      => super.eval(exp, env)
    }
  }
}

abstract class IncrementalSimpleSchemeModFAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                                      with StandardSchemeModFComponents
                                                                      with SchemeModFSemantics
                                                                      with IncrementalSchemeModFBigStepSemantics {
  override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra
}
