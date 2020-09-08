package scalaam.modular.scheme.modf

import scalaam.core._
import scalaam.modular.scheme._
import scalaam.language.scheme._

trait StandardSchemeModFAllocator extends BaseSchemeModFSemantics {
  type AllocationContext = Component
  def allocVar(id: Identifier, cmp: Component) = VarAddr(id,cmp)
  def allocPtr(exp: SchemeExp, cmp: Component) = PtrAddr(exp,cmp)  
}

