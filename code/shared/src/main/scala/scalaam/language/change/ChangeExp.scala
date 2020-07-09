package scalaam.language.change

import scalaam.core._
import scalaam.language.CScheme.CSchemeExp
import scalaam.language.scheme.SchemeExp

case object CHA extends Label // Change

trait ChangeExp extends CSchemeExp

case class CodeChange(old: SchemeExp, nw: SchemeExp, idn: Identity) extends ChangeExp {
  def fv: Set[String] = old.fv ++ nw.fv
  def label: Label = CHA
  def subexpressions: List[Expression] = List(old, nw)
  override val height: Int = Math.max(old.height, nw.height) + 1
}

object CodeVersion extends Enumeration {
  type Version = Value
  val Old, New = Value
}