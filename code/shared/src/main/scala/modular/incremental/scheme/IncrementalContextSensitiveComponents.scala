package modular.incremental.scheme

import scalaam.core.Expression
import scalaam.modular.components.ContextSensitiveComponents
import scalaam.modular.incremental.IncrementalModAnalysis

trait IncrementalContextSensitiveComponents [Expr <: Expression] extends IncrementalModAnalysis[Expr] with ContextSensitiveComponents[Expr] {

  def updateComponentContext(ctx: ComponentContext): ComponentContext
}
