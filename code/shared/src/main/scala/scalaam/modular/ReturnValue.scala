package scalaam.modular

import scalaam.core._

case class ReturnAddr(val exp: Expression) extends Address { 
  def printable = true 
  def idn = exp.idn
} 

/**
 * Provides facilities for storing and retrieving return values of components (component analyses).
 * @tparam Expr The type of the expressions under analysis.
 */
trait ReturnValue[Expr <: Expression] extends GlobalStore[Expr] {

  def returnAddr(cmp: Component) = componentAddr(cmp, ReturnAddr(expr(cmp)))

  // intra-analysis can now also update and read the result of a component
  trait ReturnResultIntra extends GlobalStoreIntra {
    // updating the result of a component (default: of the current component)
    protected def writeResult(result: Value, cmp: Component = component): Unit =
      writeAddr(returnAddr(cmp), result)
    // reading the result of a component
    protected def readResult(cmp: Component): Value =
      readAddr(returnAddr(cmp))
    // convenience method: "calling" other components registers a dependency and immediately reads their prior analysis result
    protected def call(cmp: Component): Value = {
      spawn(cmp)
      readResult(cmp)
    }
  }
}
