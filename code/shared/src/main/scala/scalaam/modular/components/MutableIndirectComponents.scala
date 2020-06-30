package scalaam.modular.components

import scalaam.core.Expression
import scalaam.modular.components.IndirectComponents
import scalaam.modular.components.IndirectComponents.ComponentPointer

/**
 * Extends component indirection by allowing updates to component pointers.
 * This should allow components to be updated more easily without
 * breaking analysis information such as inter-component dependencies.
 */
trait MutableIndirectComponents[Expr <: Expression] extends IndirectComponents[Expr] {

  /** Allows to update the 'actual component' corresponding to a given pointer.
   * If no binding exists for the pointer, this method will just register it.
   */
  def updateCPtr(cmp: ComponentData, ptr: ComponentPointer): Unit = {
    cMap.get(ptr.addr).foreach(oldC => cMapR = cMapR - oldC) // If the component was already registered, remove it from cMapR so that it can be gc'ed.
    register(cmp, ptr.addr)
  }

  /** Allows to replace the data of a component with new data. */
  def updateCPtr(oldCmp: ComponentData, newCmp: ComponentData): Unit = updateCPtr(newCmp, ref(oldCmp))
}
