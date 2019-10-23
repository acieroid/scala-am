package scalaam.modular

import core.Annotations.mutable
import scalaam.core._
import scalaam.util._

trait ReturnResult[Expr <: Expression] extends ModAnalysis[Expr] {

  // parameterized by a type that represents the result of an intra-analysis
  type Result
  val emptyResult: Result

  // keep track of the last result per component
  @mutable var results = Map[IntraComponent,Result]().withDefaultValue(emptyResult)
  private def updateComponent(component: IntraComponent, result: Result): Boolean = results.get(component) match {
    case Some(oldResult) if oldResult == result =>
      return false
    case _ =>
      results = results + (component -> result)
      return true
  }

  // Dependency that is triggered when the result of the intra-component 'component' has changed
  case class CallReturnDependency(component: IntraComponent) extends Dependency

  // intra-analysis can now also update and read the result of a component
  trait ReturnResultIntra extends super.IntraAnalysis {

    // updating the result of a component (default: of the current component)
    protected def updateResult(result: Result, component: IntraComponent = component) =
      if (updateComponent(component,result)) // Trigger a dependency.
        triggerDependency(CallReturnDependency(component))

    // reading the result of a component
    protected def readResult(component: IntraComponent): Result = {
      registerDependency(CallReturnDependency(component)) // Register a dependency.
      results(component)
    }

    // convenience method: calling other components and immediately reading their result
    protected def call(component: IntraComponent): Result = {
      spawn(component)
      readResult(component)
    }
  }
}

trait AdaptiveReturnResult[Expr <: Expression] extends AdaptiveModAnalysis[Expr] with ReturnResult[Expr] {
  // alpha definition for effects
  override def alphaDep(dep: Dependency): Dependency = dep match {
    case CallReturnDependency(component) => CallReturnDependency(alpha(component))
    case _ => super.alphaDep(dep)
  }
  // requires an alpha function for the result
  def alphaResult(result: Result): Result
  // requires a monoid to potentially merge the result of components
  // TODO: maybe just require a lattice here?
  implicit val resultMonoid: Monoid[Result]
  // when abstraction map changes, need to update the store
  override def onAlphaChange() = {
    super.onAlphaChange()
    results = alphaMap(alpha,alphaResult)(results)
  }
}
