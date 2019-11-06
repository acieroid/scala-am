package scalaam.modular.scheme

import scalaam.core._
import scalaam.language.scheme._

/* Simplest (and most imprecise): no context-sensitivity */
trait NoSensitivity extends SchemeModFSemantics {
  type Context = Unit
  def allocCtx(lambda: SchemeLambda, env: Environment[Addr], args: List[Value]): Unit = ()
}

/* Full argument sensitivity for ModF */
trait FullArgumentSensitivity extends SchemeModFSemantics {
  type Context = List[Value]
  def allocCtx(lambda: SchemeLambda, env: Environment[Addr], args: List[Value]): List[Value] = args
}
