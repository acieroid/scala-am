package modular.incremental

import scalaam.core._
import scalaam.modular.GlobalStore
import scalaam.modular.incremental.IncrementalModAnalysis

trait IncrementalGlobalStore[Expr <: Expression] extends IncrementalModAnalysis[Expr] with GlobalStore[Expr] {

  override def updateIdentities(newIdentities: Map[Identity, Option[Identity]]): Unit = {
    super.updateIdentities(newIdentities)

    // Update the identities in all component addresses of the store.
    ???
  }

}
