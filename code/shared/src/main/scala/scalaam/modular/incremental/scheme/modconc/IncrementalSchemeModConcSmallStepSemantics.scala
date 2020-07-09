package scalaam.modular.incremental.scheme.modconc

import scalaam.language.change.CodeChange
import scalaam.language.scheme.SchemeExp
import scalaam.modular.ModAnalysis
import scalaam.modular.incremental.scheme.IncrementalSchemeSemantics
import scalaam.modular.scheme.ssmodconc._
import scalaam.language.change.CodeVersion._

trait IncrementalSchemeModConcSmallStepSemantics extends SmallStepModConcSemantics with IncrementalSchemeSemantics {
  trait IncrementalSmallStepIntra extends SmallStepIntra {
    override protected def evaluate(exp: Exp, env: Env, stack: Stack): Set[State] = exp match {
      case CodeChange(old, nw, _) => registerAffected(component); if (version == Old) Set(Eval(old, env, stack)) else Set(Eval(nw, env, stack))
      case _                      => super.evaluate(exp, env, stack)
    }
  }
}

abstract class IncrementalSimpleSchemeModConcAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                                         with kCFAModConc
                                                                         with IncrementalSchemeModConcSmallStepSemantics {
  override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with kCFAIntra
}