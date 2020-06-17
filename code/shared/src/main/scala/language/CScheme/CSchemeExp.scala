package scalaam.language.CScheme

import scalaam.core.{Expression, Identity, Label}
import scalaam.language.scheme.SchemeExp

trait CSchemeExp extends SchemeExp

case object FRK extends Label // Fork
case object JOI extends Label // Join

/** Fork a thread with an expression to evaluate. */
case class CSchemeFork(body: SchemeExp, idn: Identity) extends CSchemeExp {
  def fv: Set[String] = body.fv
  def label: Label = FRK
  def subexpressions: List[Expression] = List(body)
  override val height: Int = body.height + 1
}

/** Join a thread, given an expression that should evaluate to a TID. */
case class CSchemeJoin(tExp: SchemeExp, idn: Identity) extends CSchemeExp {
  def fv: Set[String] = tExp.fv
  def label: Label = JOI
  def subexpressions: List[Expression] = List(tExp)
  override val height: Int = tExp.height + 1
}