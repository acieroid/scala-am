package scalaam.modular.scheme.modconc

import scalaam.language.scheme._
import scalaam.modular.scheme.modf._

trait SchemeModConcNoSensitivity extends SchemeModConcSemantics {
    type ComponentContext = Unit
    def allocCtx(exp: SchemeExp, env: Env, modFCmp: SchemeModFComponent, caller: Component) = ()
}

trait SchemeModConcStandardSensitivity extends SchemeModConcSemantics {
    type ComponentContext = SchemeModFComponent
    def allocCtx(exp: SchemeExp, env: Env, modFCmp: SchemeModFComponent, caller: Component) = modFCmp
}
