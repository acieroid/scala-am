/*
package scalaam.modular

import scalaam.util.Annotations.mutable

trait Indirection {
  type Component
  type CAddr

  def alloc(cmp: Component): CAddr

  def getComponent(cAddr: CAddr): Component
  def getAddress(cmp: Component): CAddr
}

trait NoIndirection extends Indirection {
  type CAddr = Component
  def alloc(cmp: Component): CAddr = cmp
  def getComponent(cAddr: CAddr): Component = cAddr
  def getAddress(cmp: Component): CAddr = cmp
}

trait IntegerAddresses extends Indirection {
  type CAddr = Int

  @mutable var count = 0 // Could also use keyset size of cMap.

  @mutable private var cMap: Map[CAddr, Component] = Map()
  @mutable private var cMapR: Map[Component, CAddr] = Map()

  private def register(cmp: Component, cAddr: CAddr): Unit = {
    cMap = cMap + (cAddr -> cmp)
    cMapR = cMapR + (cmp -> cAddr)
  }

  def alloc(cmp: Component): CAddr = {
    val addr = count
    count += 1
    register(cmp, addr)
    addr
  }
  def getComponent(cAddr: CAddr): Component = cMap(cAddr)
  def getAddress(cmp: Component): CAddr = cMapR(cmp)
}
*/