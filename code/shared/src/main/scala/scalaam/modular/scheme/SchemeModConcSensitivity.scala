package scalaam.modular.scheme

import scalaam.language.scheme._
import scalaam.modular.scheme.semantics._

trait SchemeModConcNoSensitivity extends SchemeModConcSemantics {
    type ComponentContext = Unit
    def allocCtx(exp: SchemeExp, env: Env, caller: Component) = ()
}
