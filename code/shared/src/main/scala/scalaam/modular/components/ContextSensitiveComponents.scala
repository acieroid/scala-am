package scalaam.modular.components

import scalaam.core.Expression
import scalaam.modular.ModAnalysis

trait ContextSensitiveComponents[Expr <: Expression] extends ModAnalysis[Expr] {

  /** Components that consist out of some content + some optional context */
  type ComponentContent
  type ComponentContext
  def content(cmp: Component): ComponentContent
  def context(cmp: Component): Option[ComponentContext]
}
