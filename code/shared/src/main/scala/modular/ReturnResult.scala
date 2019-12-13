package scalaam.modular

import scalaam.core._

trait ReturnResult[Expr <: Expression] extends GlobalStore[Expr] {

  // add a special address, where we can store the result of a component
  case class ReturnAddr(cmp: CAddr) extends Addr {
    def printable = true
  }

  // intra-analysis can now also update and read the result of a component
  trait ReturnResultIntra extends GlobalStoreIntra {
    // updating the result of a component (default: of the current component)
    protected def writeResult(result: Value, cAddr: CAddr = cAddr): Unit =
      writeAddr(ReturnAddr(cAddr),result)
    // reading the result of a component
    protected def readResult(cAddr: CAddr): Value =
      readAddr(ReturnAddr(cAddr))
    // convenience method: calling other components and immediately reading their result
    protected def call(component: Component): Value = {
      readResult(spawn(component))
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
