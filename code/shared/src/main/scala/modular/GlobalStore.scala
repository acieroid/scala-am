package scalaam.modular

import scalaam.core._
import scalaam.core.Annotations._
import scalaam.util.MonoidImplicits._

trait GlobalStore[Expr <: Expression] extends ModAnalysis[Expr] {

  // parameterized by a type that represents (local) addresses
  type LocalAddr <: Address
  // parameterized by a type that represents abstract values
  type Value
  implicit val lattice: Lattice[Value]

  // addresses in the global analysis are (local) addresses of the intra-analysis + the component
  trait Addr extends Address
  case class GlobalAddr(addr: LocalAddr) extends Addr {
    def printable = addr.printable
  }
  case class ComponentAddr(component: IntraComponent, addr: LocalAddr) extends Addr {
    def printable = addr.printable
  }

  // the global store of the analysis
  @mutable var store = Map[Addr,Value]().withDefaultValue(lattice.bottom)
  private def updateAddr(addr: Addr, value: Value): Boolean = store.get(addr) match {
    case None if value == lattice.bottom =>
      false
    case None =>
      store = store + (addr -> value)
      return true
    case Some(oldValue) =>
      val newValue = lattice.join(oldValue,value)
      if (newValue == oldValue) { return false }
      store = store + (addr -> newValue)
      return true
  }

  // Dependency that is triggered when an abstract value at address 'addr' is updated
  case class ReadWriteDependency(addr: Addr) extends Dependency

  trait GlobalStoreIntra extends super.IntraAnalysis {

    // allocating an address
    def allocAddr(addr: LocalAddr) = ComponentAddr(component,addr)

    // reading addresses in the global store
    protected def readAddr(addr: LocalAddr, component: IntraComponent = component): Value =
      readAddr(ComponentAddr(component,addr))

    protected def readAddr(addr: Addr): Value = {
      registerDependency(ReadWriteDependency(addr))
      store(addr)
    }

    // writing addresses of the global store
    protected def writeAddr(addr: LocalAddr, value: Value, component: IntraComponent = component): Unit =
        writeAddr(ComponentAddr(component,addr),value)

    protected def writeAddr(addr: Addr, value: Value): Unit =
        if (updateAddr(addr,value)) // If the value in the store changed, trigger the dependency.
          triggerDependency(ReadWriteDependency(addr))
    }
}

trait AdaptiveGlobalStore[Expr <: Expression] extends AdaptiveModAnalysis[Expr] with GlobalStore[Expr] {
  // alpha definition for addresses and dependencies
  def alphaAddr(addr: Addr): Addr = addr match {
    case GlobalAddr(_) => addr
    case ComponentAddr(cmp,localAddr) => ComponentAddr(alpha(cmp),localAddr)
  }
  override def alphaDep(dep: Dependency): Dependency = dep match {
    case ReadWriteDependency(addr) => ReadWriteDependency(alphaAddr(addr))
    case _ => super.alphaDep(dep)
  }
  // requires an implementation of alpha for the abstract domain
  def alphaValue(value: Value): Value
  // when abstraction map changes, need to update the store
  override def onAlphaChange() = {
    super.onAlphaChange()
    store = alphaMap(alphaAddr,alphaValue)(store)
  }
}
