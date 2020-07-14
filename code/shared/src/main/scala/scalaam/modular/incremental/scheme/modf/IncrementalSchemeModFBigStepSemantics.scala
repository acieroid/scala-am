package scalaam.modular.incremental.scheme.modf

import scalaam.language.scheme.{SchemeCodeChange, SchemeExp}
import scalaam.modular.incremental.scheme.IncrementalSchemeSemantics
import scalaam.modular.scheme.modf._
import scalaam.language.change.CodeVersion._

trait IncrementalSchemeModFBigStepSemantics extends BigStepModFSemantics with IncrementalSchemeSemantics {
  trait IncrementalSchemeModFBigStepIntra extends BigStepModFIntra {
    override protected def eval(exp: SchemeExp, env: Env): Value = exp match {
      case SchemeCodeChange(e, _, _) if version == Old =>
        registerAffected(component)
        registerComponent(e, component)
        eval(e, env)
      case SchemeCodeChange(_, e, _) if version == New =>
        registerAffected(component)
        registerComponent(e, component)
        eval(e, env)
      case _                                     =>
        registerComponent(exp, component)
        super.eval(exp, env)
    }
  }
}