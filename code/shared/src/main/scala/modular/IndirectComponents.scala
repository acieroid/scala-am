package scalaam.modular

import scalaam.core._
import scalaam.util.Annotations._

trait IndirectComponents[Expr <: Expression] extends ModAnalysis[Expr] {

  // components are pointers ...
  type Component <: ComponentPointer
  trait ComponentPointer {
    val addr: Int
    def deref(): ComponentData = cMap(addr)
    override def toString: String = s"#$addr: ${deref().toString}"
  }
  def makePointer(addr: Int): Component // needs to be overwritten in subclass!
  // ... that point to the actual component data
  type ComponentData

  // keep a mapping from component pointer addresses to component data
  @mutable private var count: Int = _
  @mutable private var cMap : Map[Int, ComponentData] = _
  @mutable private var cMapR: Map[ComponentData, Int] = _

  protected def init() = {
    count = 0
    cMap = Map()
    cMapR = Map()
  }

  private def alloc(): Int = {
    val addr = count
    count += 1
    addr
  }
  private def register(cmp: ComponentData, addr: Int): Unit = {
    cMap  = cMap  + (addr -> cmp)
    cMapR = cMapR + (cmp -> addr)
  }
  private def newComponent(cmp: ComponentData): Int = {
    val addr = alloc()
    register(cmp, addr)
    addr
  }
  def ref(cmp: ComponentData): Component =
    makePointer(cMapR.getOrElse(cmp, newComponent(cmp)))
  protected def update(cmp: ComponentData, ptr: ComponentPointer): Unit =
    register(cmp, ptr.addr)
}
