package modular.incremental

import scalaam.core._
import scalaam.modular._
import scalaam.modular.incremental.IncrementalModAnalysis
import scalaam.util.Annotations.assume

trait IncrementalGlobalStore[Expr <: Expression] extends IncrementalModAnalysis[Expr] with DedicatedGlobalStore[Expr] {

  /** Updates an address itself in the store. Removes the old address and stores the corresponding value at the new address. */
  @assume("The address nw is not bound in the store.")
  def updateAddress(old: Addr, nw: Addr): Unit = {
    assert(!store.keySet.contains(nw)) // We assume the store cannot contain an entry for the new address.
    store.get(old).foreach( value => store = store - old + (nw -> value))
  }

  override def updateIdentities(newIdentities: Map[Identity, Identity]): Unit = {
    super.updateIdentities(newIdentities)

    // Update the identities in all component addresses of the store.
    // The components should not be updated, but the local addresses should.
    // TODO: how to update closures?
  }

}
