package scalaam.modular.adaptive

import scalaam.core._
import scalaam.modular._
import scalaam.util.MonoidImplicits._

trait AdaptiveGlobalStore[Expr <: Expression] extends AdaptiveModAnalysis[Expr] with GlobalStore[Expr] {
  // update implementation for addresses and dependencies
  def updateAddr(update: Component => Component)(addr: Addr): Addr = addr match {
    case _ : GlobalAddr => addr
    case ComponentAddr(cmp, localAddr) => ComponentAddr(update(cmp), localAddr)
  }
  override def updateDep(update: Component => Component)(dep: Dependency): Dependency = dep match {
    case ReadWriteDependency(addr) => ReadWriteDependency(updateAddr(update)(addr))
    case _ => super.updateDep(update)(dep)
  }
  // requires an implementation of alpha for the abstract domain
  def updateValue(update: Component => Component)(value: Value): Value
  // when abstraction map changes, need to update the store
  override def updateAnalysisData(update: Component => Component): Unit = {
    super.updateAnalysisData(update)
    val oldStore = store
    store = updateMap(updateAddr(update),updateValue(update))(store)
    // look if we have to retrigger any dependencies due to the abstraction
    oldStore.foreach { case (oldKey, oldValue) =>
      val newKey = updateAddr(update)(oldKey)
      val newValue = store(newKey)
      if (oldValue != newValue) {
        trigger(ReadWriteDependency(newKey))
      }
    }
  }
}
