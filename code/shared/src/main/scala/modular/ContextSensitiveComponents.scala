package scalaam.modular

import scalaam.core._

trait ContextSensitiveComponents[Expr <: Expression] extends ModAnalysis[Expr] {

  /** Components that consist out of some content + some optional context */
  type ComponentContent
  type ComponentContext
  def content(cmp: Component): ComponentContent
  def context(cmp: Component): Option[ComponentContext]

  // component ordering
  implicit def contentOrdering: Ordering[ComponentContent]
  implicit def contextOrdering: Ordering[Option[ComponentContext]] = new Ordering.OptionOrdering[ComponentContext] {
    def optionOrdering = Ordering.by[ComponentContext,String](_.toString) //TODO: toString of context may be the same for different contexts
  }
  implicit def cmpOrdering: Ordering[Component] =
    Ordering[(ComponentContent,Option[ComponentContext])].on(cmp => (content(cmp), context(cmp)))
}
