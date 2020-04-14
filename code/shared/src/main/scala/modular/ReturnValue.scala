package scalaam.modular

import scalaam.core._

/**
 * Provides facilities for storing and retrieving return values of components.
 * @tparam Expr The type of the expressions under analysis.
 */
trait ReturnValue[Expr <: Expression] extends GlobalStore[Expr] {

  // add a special address, where we can store the result of a component
  case class ReturnAddr(cmp: Component) extends Addr {
    def printable = true
    override def toString = s"<ret $cmp>"
  }

  // intra-analysis can now also update and read the result of a component
  trait ReturnResultIntra extends GlobalStoreIntra {
    // updating the result of a component (default: of the current component)
    protected def writeResult(result: Value, cmp: Component = component): Unit =
      writeAddr(ReturnAddr(cmp),result)
    // reading the result of a component
    protected def readResult(cmp: Component): Value =
      readAddr(ReturnAddr(cmp))
    // convenience method: calling other components registers a dependency and immediately reads their prior analysis result
    protected def call(cmp: Component): Value = {
      spawn(cmp)
      readResult(cmp)
    }
  }
}
