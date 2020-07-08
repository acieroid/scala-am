package scalaam.language.change

import scalaam.core._
import scalaam.language.CScheme.CSchemeExp
import scalaam.language.scheme.SchemeExp

case object CHA extends Label // Change

trait ChangeExp extends CSchemeExp

case class CodeChange(old: SchemeExp, nw: SchemeExp, idn: Identity) extends ChangeExp {
  def fv: Set[String] = throw new Exception("Free variable detection not allowed for code change expressions.")
  def label: Label = CHA
  def subexpressions: List[Expression] = List(old, nw)
  override val height: Int = Integer.MIN_VALUE
}
