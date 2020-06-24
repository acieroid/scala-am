package scalaam.modular

import scalaam.core._

/**
 * Adds a global store to the analysis. This store supports various addressing modes.
 * @tparam Expr The type of the expressions under analysis.
 */
trait GlobalStore[Expr <: Expression] extends ModAnalysis[Expr] { inter =>

  // parameterized by a type that represents (local) addresses
  type LocalAddr <: Address
  // parameterized by a type that represents abstract values
  type Value
  implicit val lattice: Lattice[Value]

  // addresses in the global analysis are (local) addresses of the intra-analysis + the component
  trait Addr extends Address
  case class ComponentAddr(cmp: Component, addr: LocalAddr) extends Addr {
    override def toString(): String = s"#<$addr $cmp>"
    def printable: Boolean = addr.printable
  }

  // the global store of the analysis
  var store: Map[Addr,Value] = Map()
  private def updateAddr(store: Map[Addr,Value], addr: Addr, value: Value): (Map[Addr,Value],Boolean) = 
    store.get(addr) match {
      case None if value == lattice.bottom => (store, false)
      case None => (store + (addr -> value), true)
      case Some(oldValue) =>
        val newValue = lattice.join(oldValue,value)
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

    // keep the store local
    var store = inter.store

    // allocating an address
    def allocAddr(addr: LocalAddr): ComponentAddr =
      ComponentAddr(component, addr)

    // reading addresses in the global store
    protected def readAddr(addr: LocalAddr, cmp: Component = component): Value =
      readAddr(ComponentAddr(cmp, addr))
    protected def readAddr(addr: Addr): Value = {
      register(ReadWriteDependency(addr))
      store.get(addr) match {
        case None => 
          store += (addr -> lattice.bottom)
          return lattice.bottom
        case Some(v) => 
          return v
      }
    }

    // Writing addresses of the global store, returns whether the store has changed or not.
    protected def writeAddr(addr: LocalAddr, value: Value, cmp: Component = component): Boolean =
      writeAddr(ComponentAddr(cmp, addr),value)
    protected def writeAddr(addr: Addr, value: Value): Boolean = {
      val (updatedStore, hasChanged) = updateAddr(intra.store,addr,value)
      if (hasChanged) {      
        intra.store = updatedStore
        intra.trigger(ReadWriteDependency(addr))
      }
      hasChanged
    }

    override def commit(dep: Dependency): Boolean = dep match {
      case ReadWriteDependency(addr) => 
        val (updatedStore, hasChanged) = updateAddr(inter.store,addr,intra.store(addr))
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