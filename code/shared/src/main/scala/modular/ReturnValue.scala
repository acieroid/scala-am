package scalaam.modular

import scalaam.core._

case object ReturnAddr extends Address { def printable = true } 

/**
 * Provides facilities for storing and retrieving return values of components (component analyses).
 * @tparam Expr The type of the expressions under analysis.
 */
trait ReturnValue[Expr <: Expression] extends GlobalStore[Expr] {

  // intra-analysis can now also update and read the result of a component
  trait ReturnResultIntra extends GlobalStoreIntra {
    // updating the result of a component (default: of the current component)
    protected def writeResult(result: Value, cmp: Component = component): Unit =
      writeAddr(ComponentAddr(cmp,ReturnAddr),result)
    // reading the result of a component
    protected def readResult(cmp: Component): Value =
      readAddr(ComponentAddr(cmp,ReturnAddr))
    // convenience method: "calling" other components registers a dependency and immediately reads their prior analysis result
    protected def call(cmp: Component): Value = {
      spawn(cmp)
      readResult(cmp)
    }
  }
}
