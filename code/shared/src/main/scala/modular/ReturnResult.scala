package scalaam.modular

import scalaam.core._

trait ReturnResult[Expr <: Expression] extends GlobalStore[Expr] {

  // add a special address, where we can store the result of a component
  case class ReturnAddr(cmp: Component) extends Addr {
    def printable = true
  }

  // intra-analysis can now also update and read the result of a component
  trait ReturnResultIntra extends GlobalStoreIntra {
    // updating the result of a component (default: of the current component)
    protected def writeResult(result: Value, component: Component = component): Unit =
      writeAddr(ReturnAddr(component),result)
    // reading the result of a component
    protected def readResult(component: Component): Value =
      readAddr(ReturnAddr(component))
    // convenience method: calling other components and immediately reading their result
    protected def call(component: Component): Value = {
      spawn(component)
      readResult(component)
    }
  }
}

trait AdaptiveReturnResult[Expr <: Expression] extends AdaptiveGlobalStore[Expr] with ReturnResult[Expr] {
  // alpha definition for dependencies
  override def alphaAddr(addr: Addr): Addr = addr match {
    case ReturnAddr(cmp) => ReturnAddr(alpha(cmp))
    case _ => super.alphaAddr(addr)
  }
}
