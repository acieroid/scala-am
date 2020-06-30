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

  // parameterized by read/write operations, depending on the representation of the store 
  def read(addr: Addr): Option[Value]
  def write(addr: Addr, value: Value): Boolean

  // parameterized by how addresses are allocated
  def sharedAddr(addr: Address): Addr
  def componentAddr(cmp: Component, addr: Address): Addr 

  // Dependency that is triggered when an abstract value at address 'addr' is updated
  // TODO: rename to AddrDependency or something, `ReadWrite` is universal to all dependencies
  case class ReadWriteDependency(addr: Addr) extends Dependency {
    override def toString(): String = s"$addr"
  }

  trait GlobalStoreIntra extends super.IntraAnalysis { intra => 
    // reading/writing from/to a local copy
    def readLocal(addr: Addr): Option[Value]
    def writeLocal(addr: Addr, value: Value): Boolean
    // allocating an address
    protected def allocAddr(addr: Address): Addr =
      componentAddr(component, addr)
    // reading addresses in the global store
    protected def readAddr(addr: Addr): Value = {
      register(ReadWriteDependency(addr))
      readLocal(addr) match {
        case None => 
          writeLocal(addr, lattice.bottom) // TODO: <- currently required by AdaptiveGlobalStore, but can go once fixed there
          return lattice.bottom
        case Some(v) => 
          return v
      }
    }
    // writing addresses of the global store
    protected def writeAddr(addr: Addr, value: Value): Boolean = 
      if (writeLocal(addr, value)) {
        intra.trigger(ReadWriteDependency(addr))
        true 
      } else {
        false 
      }

    override def commit(dep: Dependency): Boolean = dep match {
      case ReadWriteDependency(addr) => 
        write(addr, readLocal(addr).get) // safe to `get` here because we have an AddrDependency
      case _ => super.commit(dep)
    }
  }
}

trait DedicatedGlobalStore[Expr <: Expression] extends GlobalStore[Expr] { inter => 
  // a store is just a fresh and empty map
  var store: Map[Addr,Value] = Map.empty
  def read(addr: Addr) = store.get(addr)
  def write(addr: Addr, value: Value) = updateAddr(store, addr, value) match {
    case (updated, true)  => { store = updated ; true }
    case _                => false
  }
  // addresses are component addresses
  type Addr = A[Component]
  // allocating addresses
  def componentAddr(cmp: Component, addr: Address) = ComponentAddr(cmp, addr)
  def sharedAddr(addr: Address) = GlobalAddr(addr)

  // helper function to properly update a store
  protected def updateAddr(store: Map[Addr,Value], addr: Addr, value: Value): (Map[Addr,Value],Boolean) = 
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

    override def intraAnalysis(cmp: Component): DedicatedGlobalStoreIntra
    trait DedicatedGlobalStoreIntra extends super.GlobalStoreIntra { intra => 
      var store = inter.store
      def readLocal(addr: Addr): Option[Value] = store.get(addr)
      def writeLocal(addr: Addr, value: Value) = updateAddr(intra.store, addr, value) match {
        case (updated, true)  => { intra.store = updated ; true }
        case _                => false
      }
    }
}