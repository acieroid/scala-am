/*package scalaam.scalaam.modular.components

import scalaam.scalaam.modular.components.Indirection.Pointer
import scalaam.scalaam.util.Annotations.mutable

trait Indirection {

  type Ptr
  type Data

  /** Performs initialisation. Should be called before the first indirection is stored. */
  def init(): Unit

  /** Gets the reference to specific data. Creates and stores a new reference internally if needed. */
  def ref(data: Data): Ptr

  /** Retrieves the data corresponding to a certain pointer. */
  def deref(pointer: Ptr): Data

  /** Allows treating a pointer as data. */
  implicit def view(pointer: Ptr): Data = deref(pointer)
}

trait MutableIndirection extends Indirection {

  /** Updates the data corresponding to a given pointer. */
  def update(pointer: Ptr, data: Data): Unit

  /** Replaces old by nw, by updating the pointer corresponding to old. */
  def update(old: Data, nw: Data): Unit
}

object Indirection {
  case class Pointer(addr: Int) extends AnyVal {
    override def toString: String = s"#$addr"
  }
}

trait MapIndirection extends Indirection {

  type Ptr = Pointer
  type Addr = Int

  // Keep a mapping from component pointer addresses to actual component data.
  @mutable private var count: Addr = _ // Next free address.
  @mutable protected var map : Map[Addr, Data] = _
  @mutable protected var mapR: Map[Data, Addr] = _

  // Needed due to the initialisation order of Scala.
  def init(): Unit = {
    count = 0
    map  = Map()
    mapR = Map()
  }

  /** Returns the next unused address. */
  private def alloc(): Addr = {
    val addr = count
    count += 1
    addr
  }

  /** Registers data at a given address. */
  protected def register(cmp: Data, addr: Addr): Unit = {
    map  = map  + (addr -> cmp)
    mapR = mapR + (cmp -> addr)
  }

  private def store(cmp: Data): Addr = {
    val addr = alloc()
    register(cmp, addr)
    addr
  }

  def ref(cmp: Data): Pointer = Pointer(mapR.getOrElse(cmp, store(cmp)))
  def deref(pointer: Pointer): Data = map(pointer.addr)
}

trait MutableMapIndirection extends MapIndirection with MutableIndirection {

  def updateCPtr(data: Data, ptr: Pointer): Unit = {
    map.get(ptr.addr).foreach(oldC => mapR = mapR - oldC) // If the component was already registered, remove it from cMapR so that it can be gc'ed.
    register(data, ptr.addr)
  }

  /** Allows to replace the data of a component with new data. */
  def updateCPtr(old: Data, nw: Data): Unit = updateCPtr(old, ref(nw))
}*/