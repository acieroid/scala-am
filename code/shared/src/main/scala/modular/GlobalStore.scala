package scalaam.modular

import scalaam.core._

sealed trait A[+Component] extends Address
case class GlobalAddr(addr: Address) extends A[Nothing]                                 { def printable = addr.printable }
case class ComponentAddr[Component](cmp: Component, addr: Address) extends A[Component] { def printable = addr.printable } 

/**
 * An analysis with a global store.
 * @tparam Expr The type of the expressions under analysis.
 */
trait GlobalStore[Expr <: Expression] extends ModAnalysis[Expr] { inter =>

  // parameterized by the type of address and abstract values
  type Addr <: Address
  type Value
  implicit val lattice: Lattice[Value]

  // parameterized by the representation of the store 
  var store: Map[Addr, Value]

  // parameterized by how addresses are allocated
  def sharedAddr(addr: Address): Addr
  def componentAddr(cmp: Component, addr: Address): Addr 

  // helper function to properly update a store
  private def updateAddr(store: Map[Addr,Value], addr: Addr, value: Value): (Map[Addr,Value],Boolean) = 
    store.get(addr) match {
      case None if value == lattice.bottom => (store, false)
      case None => (store + (addr -> value), true)
      case Some(oldValue) =>
        val newValue = lattice.join(oldValue, value)
        if (newValue == oldValue) {
          (store, false)
        } else {
          (store + (addr -> newValue), true)
        }
    }

  // Dependency that is triggered when an abstract value at address 'addr' is updated
  // TODO: rename to AddrDependency or something, `ReadWrite` is universal to all dependencies
  case class ReadWriteDependency(addr: Addr) extends Dependency {
    override def toString(): String = s"$addr"
  }

  trait GlobalStoreIntra extends super.IntraAnalysis { intra => 
    // keep a local copy of the store
    var store = inter.store
    // allocating an address
    protected def allocAddr(addr: Address): Addr =
      componentAddr(component, addr)
    // reading addresses in the global store
    protected def readAddr(addr: Addr): Value = {
      register(ReadWriteDependency(addr))
      store.get(addr) match {
        case None => 
          store += addr -> lattice.bottom
          return lattice.bottom
        case Some(v) => 
          return v
      }
    }
    // writing addresses of the global store
    protected def writeAddr(addr: Addr, value: Value): Boolean = {
      val (updatedStore, hasChanged) = updateAddr(intra.store, addr, value)
      if (hasChanged) {      
        intra.store = updatedStore
        intra.trigger(ReadWriteDependency(addr))
      }
      hasChanged
    }

    override def commit(dep: Dependency): Boolean = dep match {
      case ReadWriteDependency(addr) => 
        val (updatedStore, hasChanged) = updateAddr(inter.store, addr, intra.store(addr))
        if (hasChanged) {
          inter.store = updatedStore
          true
        } else {
          false
        }
      case _ => super.commit(dep)
    }
  }
}

trait DedicatedGlobalStore[Expr <: Expression] extends GlobalStore[Expr] {
  // a fresh and empty store
  override var store: Map[Addr,Value] = Map.empty
  // addresses are component addresses
  type Addr = A[Component]
  // allocating addresses
  def componentAddr(cmp: Component, addr: Address) = ComponentAddr(cmp, addr)
  def sharedAddr(addr: Address) = GlobalAddr(addr)
}