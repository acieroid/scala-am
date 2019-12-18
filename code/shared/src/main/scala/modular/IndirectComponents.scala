package scalaam.modular

import scalaam.core._
import scalaam.util.Annotations._

/**
 *  Provides the ability to reference components 'by pointer'.
 *  This should allow components to be updates more easily without
 *  breaking analysis information such as inter-component dependencies.
 **/
trait IndirectComponents[Expr <: Expression] extends ModAnalysis[Expr] {

  // Secretly, every component is a pointer to an 'actual component', but that information is not leaked to the outside.
  type Component <: ComponentPointer
  trait ComponentPointer {
    val addr: Int
    def deref(): ComponentData = cMap(addr)
    override def toString: String = s"#$addr: ${deref().toString}"
    implicit def toData: ComponentData = deref()
  }
  def makePointer(addr: Int): Component // needs to be overwritten in subclass!
  // The 'actual component (data)' can be anything that is considered useful.
  type ComponentData

  // Keep a mapping from component pointer addresses to actual component data.
  @mutable private var count: Int = _
  @mutable private var cMap : Map[Int, ComponentData] = _
  @mutable private var cMapR: Map[ComponentData, Int] = _

  // Needed due to the initialisation order of Scala.
  protected def init(): Unit = {
    count = 0
    cMap  = Map()
    cMapR = Map()
  }

  /** Returns the next unused address. */
  private def alloc(): Int = {
    val addr = count
    count += 1
    addr
  }
  /** Registers a component at a given address. */
  private def register(cmp: ComponentData, addr: Int): Unit = {
    cMap  = cMap  + (addr -> cmp)
    cMapR = cMapR + (cmp -> addr)
  }
  /** Creates a component (pointer) from an 'actual component' */
  private def newComponent(cmp: ComponentData): Int = {
    val addr = alloc()
    register(cmp, addr)
    addr
  }
  /** Returns the pointer corresponding to an (actual) component. */
  def ref(cmp: ComponentData): Component = makePointer(cMapR.getOrElse(cmp, newComponent(cmp)))

  /** Allows to update the 'actual component' corresponding to a given pointer. */
  protected def update(cmp: ComponentData, ptr: ComponentPointer): Unit = register(cmp, ptr.addr)
}
