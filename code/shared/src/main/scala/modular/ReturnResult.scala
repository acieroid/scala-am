package scalaam.modular

import scalaam.core._
import scala.collection.mutable._

trait ReturnResult[Expr <: Exp] extends ModAnalysis[Expr] {

  // parameterized by a type that represents the result of an intra-analysis
  type Result
  val emptyResult: Result

  // keep track of the last result per component
  val results = Map[IntraComponent,Result]().withDefaultValue(emptyResult)
  private def updateComponent(component: IntraComponent, result: Result): Boolean = results.get(component) match {
    case None =>
      results(component) = result
      return true
    case Some(oldResult) if oldResult == result =>
      return false
    case Some(_) =>
      results(component) = result
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
