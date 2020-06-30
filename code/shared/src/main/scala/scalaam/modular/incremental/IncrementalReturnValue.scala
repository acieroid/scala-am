package scalaam.modular.incremental

import scalaam.core.Expression
import scalaam.modular.ReturnValue

trait IncrementalReturnValue[Expr <: Expression] extends IncrementalGlobalStore[Expr] with ReturnValue[Expr] {

  /* Simple: since an indirection is used for components, the return addresses should not be updated since the pointers in the addresses remain valid.
  override def updateIdentities(newIdentities: Map[Identity, Option[Identity]]): Unit = {
    super.updateIdentities(newIdentities)

    // Update the identities in all return addresses within the store.
    ???
  }
   */

}