package scalaam.modular.incremental.scheme.modconc

import scalaam.modular.incremental.scheme.IncrementalSchemeSemantics
import scalaam.modular.scheme.ssmodconc._
import scalaam.language.change.CodeVersion._
import scalaam.language.scheme.SchemeCodeChange

trait IncrementalSchemeModConcSmallStepSemantics extends SmallStepModConcSemantics with IncrementalSchemeSemantics {
  trait IncrementalSmallStepIntra extends SmallStepIntra {
    override protected def evaluate(exp: Exp, env: Env, stack: Stack): Set[State] = exp match {
      case SchemeCodeChange(e, _, _) if version == Old =>
        registerComponent(e, component)
        Set(Eval(e, env, stack))
      case SchemeCodeChange(_, e, _) if version == New =>
        registerComponent(e, component)
        Set(Eval(e, env, stack))
      case _                                           =>
        registerComponent(exp, component)
        super.evaluate(exp, env, stack)
    }
  }
}