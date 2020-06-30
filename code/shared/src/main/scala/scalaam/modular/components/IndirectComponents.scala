package scalaam.modular.components

import scalaam.modular.components.IndirectComponents.ComponentPointer
import scalaam.core._
import scalaam.modular.ModAnalysis
import scalaam.util.Annotations._

object IndirectComponents {
  // A component pointer just is an integer.
  case class ComponentPointer(addr: Int) extends AnyVal {
    override def toString: String = s"#$addr"
  }
}

/** Provides the ability to reference components 'by pointer'. */
trait IndirectComponents[Expr <: Expression] extends ModAnalysis[Expr] {

  /** Secretly, every component is a pointer to an 'actual component', but that information is not leaked to the outside. */
  type Component = ComponentPointer
  type Address = Int

  /** The 'actual component (data)' can be anything that is considered useful. */
  type ComponentData

  // Keep a mapping from component pointer addresses to actual component data.
  @mutable private var count: Address = _ // Next free address.
  @mutable protected var cMap : Map[Address, ComponentData] = _
  @mutable protected var cMapR: Map[ComponentData, Address] = _

  // Needed due to the initialisation order of Scala.
  protected def init(): Unit = {
    count = 0
    cMap  = Map()
    cMapR = Map()
  }

  /** Returns the next unused address. */
  private def alloc(): Address = {
    val addr = count
    count += 1
    addr
  }

  /** Registers a component at a given address. */
  protected def register(cmp: ComponentData, addr: Address): Unit = {
    cMap  = cMap  + (addr -> cmp)
    cMapR = cMapR + (cmp -> addr)
  }

  /** Creates a component (pointer) from an 'actual component'. */
  private def newComponent(cmp: ComponentData): Address = {
    val addr = alloc()
    register(cmp, addr)
    addr
  }

  /** Returns the pointer corresponding to an (actual) component. */
  def ref(cmp: ComponentData): Component = ComponentPointer(cMapR.getOrElse(cmp, newComponent(cmp)))

  /** Retrieves the component data corresponding to a given component pointer. */
  def deref(ptr: ComponentPointer): ComponentData = cMap(ptr.addr)

  /** Allows to treat a component pointer as a component. */
  implicit def view(cmp: Component): ComponentData = deref(cmp)
}


