package scalaam.modular.scheme.modconc

import scalaam.core._
import scalaam.modular.scheme._
import scalaam.modular.scheme.modf._
import scalaam.language.scheme._

trait StandardSchemeModConcAllocator extends SchemeModConcSemantics {
  type AllocationContext = SchemeModFComponent
  def allocVar(id: Identifier, modfCmp: SchemeModFComponent, cmp: Component) = VarAddr(id,modfCmp)
  def allocPtr(exp: SchemeExp, modfCmp: SchemeModFComponent, cmp: Component) = PtrAddr(exp,modfCmp)  
}
