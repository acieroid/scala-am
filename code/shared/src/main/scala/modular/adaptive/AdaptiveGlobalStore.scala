package scalaam.modular.adaptive

import scalaam.core.Expression
import scalaam.modular.GlobalStore
import scalaam.util.MonoidImplicits._

trait AdaptiveGlobalStore[Expr <: Expression] extends AdaptiveModAnalysis[Expr] with GlobalStore[Expr] {
  // alpha definition for addresses and dependencies
  def alphaAddr(addr: Addr): Addr = addr match {
    case ComponentAddr(cmp,localAddr) => ComponentAddr(alpha(cmp),localAddr)
  }
  override def alphaDep(dep: Dependency): Dependency = dep match {
    case ReadWriteDependency(addr) => ReadWriteDependency(alphaAddr(addr))
    case _ => super.alphaDep(dep)
  }
  // requires an implementation of alpha for the abstract domain
  def alphaValue(value: Value): Value
  // when abstraction map changes, need to update the store
  override def onAlphaChange(): Unit = {
    super.onAlphaChange()
    val oldStore = store
    store = alphaMap(alphaAddr,alphaValue)(store)
    // look if we have to retrigger any dependencies due to the abstraction
    oldStore.foreach { case (oldKey, oldValue) =>
      val newKey = alphaAddr(oldKey)
      val newValue = store(newKey)
      if (oldValue != newValue) {
        triggerDependency(ReadWriteDependency(newKey))
      }
    }
  }
}
