package scalaam.modular.scheme

import scalaam.core._
import scalaam.language.scheme._
import scalaam.modular.{GlobalStore, ModAnalysis}

/* Simplest (and most imprecise): no context-sensitivity */
trait NoSensitivity {
  this: ModAnalysis[SchemeExp] with GlobalStore[SchemeExp] =>
  type Context = Unit
  def allocCtx(lambda: SchemeLambda, env: Environment[Addr], args: List[Value]) = ()
}

/* Full argument sensitivity for ModF */
trait FullArgumentSensitivity {
  this: ModAnalysis[SchemeExp] with GlobalStore[SchemeExp] =>
  type Context = List[Value]
  def allocCtx(lambda: SchemeLambda, env: Environment[Addr], args: List[Value]): List[Value] = args
}
