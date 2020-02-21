package scalaam.modular.adaptive

import scalaam.core.Expression
import scalaam.modular._

trait AdaptiveReturnValue[Expr <: Expression] extends AdaptiveGlobalStore[Expr] with ReturnValue[Expr] {
  // alpha definition for dependencies
  override def updateAddr(update: Component => Component)(addr: Addr): Addr = addr match {
    case ReturnAddr(cmp) => ReturnAddr(update(cmp))
    case _ => super.updateAddr(update)(addr)
  }
}
