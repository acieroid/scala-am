package scalaam.language.change

import scalaam.core._

case object CHA extends Label // Code change

trait ChangeExp[Expr <: Expression] extends Expression {
  val old: Expr
  val  nw: Expr
  val idn: Identity

  def fv: Set[String] = old.fv ++ nw.fv
  def label: Label = CHA
  def subexpressions: List[Expression] = List(old, nw)
  override val height: Int = Math.max(old.height, nw.height) + 1
}

object CodeVersion extends Enumeration {
  type Version = Value
  val Old, New = Value
}