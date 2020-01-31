package modular.incremental

import scalaam.core.{Expression, Identity}
import scalaam.modular.ReturnValue

trait IncrementalReturnValue[Expr <: Expression] extends IncrementalGlobalStore[Expr] with ReturnValue[Expr] {

  override def updateIdentities(newIdentities: Map[Identity, Option[Identity]]): Unit = {
    super.updateIdentities(newIdentities)

    // Update the identities in all return addresses within the store.
    ???
  }

}