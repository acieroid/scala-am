package scalaam.modular

import scalaam.core._

// Dependency that is triggered when an abstract value at address 'addr' is updated
case class AddrDependency(addr: Address) extends Dependency {
  override def toString(): String = s"AddrDep($addr)"
}

/**
 * An analysis with a global store.
 * @tparam Expr The type of the expressions under analysis.
 */
trait GlobalStore[Expr <: Expression] extends ModAnalysis[Expr] 
                                        with AbstractDomain[Expr] { inter =>

  // TODO: should we parameterize this for more type-safety, or do we not care about that for addresses?
  type Addr = Address

  // parameterized by some store that can be accessed and modified
  var store: Map[Addr, Value]

  private def updateAddr(store: Map[Addr,Value], addr: Addr, value: Value): Option[Map[Addr,Value]] = 
    store.get(addr) match {
      case None if value == lattice.bottom => None
      case None => Some(store + (addr -> value))
      case Some(oldValue) =>
        val newValue = lattice.join(oldValue, value)
        if (newValue == oldValue) {
          None
        } else {
          Some(store + (addr -> newValue))
        }
    }

  override def intraAnalysis(cmp: Component): GlobalStoreIntra
  trait GlobalStoreIntra extends IntraAnalysis { intra => 
    // local copy of the global store
    var store = inter.store
    // reading addresses in the global store
    def readAddr(addr: Addr): Value = {
      register(AddrDependency(addr))
      intra.store.get(addr) match {
        case None => 
          intra.store = intra.store + (addr -> lattice.bottom) // TODO: <- currently required by AdaptiveGlobalStore, but can go once fixed there
          return lattice.bottom
        case Some(v) => 
          return v
      }
    }
    // writing addresses of the global store
    def writeAddr(addr: Addr, value: Value): Boolean = 
      updateAddr(intra.store, addr, value)
        .map(updated => {
          intra.store = updated
          trigger(AddrDependency(addr))
        })
        .isDefined

    override def commit(dep: Dependency): Boolean = dep match {
      case AddrDependency(addr) => 
        updateAddr(inter.store, addr, intra.store(addr))
          .map(updated => inter.store = updated)
          .isDefined
      case _ => super.commit(dep)
    }
    // An adapter for the "old" store interface
    // TODO[maybe]: while this should be sound, it might be more precise to not immediately write every value update to the global store ...
    case object StoreAdapter extends Store[Addr,Value] {
      def lookup(a: Addr): Option[Value] = Some(readAddr(a))
      def extend(a: Addr, v: Value): Store[Addr, Value] = { writeAddr(a,v) ; this }
    }
  }
}