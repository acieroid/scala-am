package scalaam.modular

import scalaam.core._
import scalaam.util._

trait ReturnResult[Expr <: Exp] extends ModAnalysis[Expr] {

  // parameterized by a type that represents the result of an intra-analysis
  type Result
  val emptyResult: Result

  // keep track of the last result per component
  var results = Map[IntraComponent,Result]().withDefaultValue(emptyResult)
  private def updateComponent(component: IntraComponent, result: Result): Boolean = results.get(component) match {
    case Some(oldResult) if oldResult == result =>
      return false
    case _ =>
      results = results + (component -> result)
      return true
  }

  // effect that is triggered when the result of the intra-component 'component' has changed
  case class ResultEffect(component: IntraComponent) extends Effect

  // intra-analysis can now also update and read the result of a component
  trait ReturnResultIntra extends super.IntraAnalysis {
    // updating the result of a component (default: of the current component)
    protected def updateResult(result: Result, component: IntraComponent = component) =
      if (updateComponent(component,result)) {
        pushEffect(ResultEffect(component))
      }
    // reading the result of a component
    protected def readResult(component: IntraComponent): Result = {
      pullEffect(ResultEffect(component))
      results(component)
    }
    // convenience method: calling other components and immediately reading their result
    protected def call(component: IntraComponent): Result = {
      spawn(component)
      readResult(component)
    }
  }
}

trait AdaptiveReturnResult[Expr <: Exp] extends AdaptiveModAnalysis[Expr] with ReturnResult[Expr] {
  // alpha definition for effects
  override def alphaEffect(effect: Effect): Effect = effect match {
    case ResultEffect(component) => ResultEffect(alpha(component))
    case _ => super.alphaEffect(effect)
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
