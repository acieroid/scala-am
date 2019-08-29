package scalaam.core

/** An address */
trait Address extends SmartHash {

  /** Should the address be included when printing an environment or store?
    * This allows to reduce the size of the printed environment/store.
    * Address that are not printable may for example include addresses of primitive functions.
    */
  def printable: Boolean
}

/** An allocator is used to allocate addresses of type A. It relies on
  * timestamps of type T, and contexts of type C */
trait Allocator[A <: Address, T, C] {
  implicit val timestamp: Timestamp[T, C]

  /** Allocate a variable given an identifier */
  def variable(name: Identifier, t: T): A

  /** Allocate a pointer given some information of type E (usually an expression) */
  def pointer[E <: Exp](e: E, t: T): A

  /** Allocate a primitive */
  def primitive(name: String): A
}

/** The most simple and useful addressing scheme: representing the address of a
  * variable by its name. This completely ignores the timestamp. */
object NameAddress {
  trait A extends Address

  /** The address of a variable */
  case class Variable(name: Identifier) extends A {
    def printable         = true
    override def toString = s"@${name.name}-${name.pos}"
  }

  /** The address for a pointer */
  case class Pointer[E <: Exp](e: E) extends A {
    def printable = false
  }

  /** The address of a primitive */
  case class Primitive(name: String) extends A {
    def printable = false
  }

  /** The NameAddress allocator */
  case class Alloc[T, C]()(implicit val timestamp: Timestamp[T, C]) extends Allocator[A, T, C] {
    def variable(name: Identifier, t: T): A = Variable(name)
    def pointer[E <: Exp](e: E, t: T): A    = Pointer(e)
    def primitive(name: String): A          = Primitive(name)
  }
}

/** Similar to NameAddress, but also includes the timestamp in the address.
  * As a result, the address has the same sensitivity as the timestamps
  */
case class TimestampAddress[T, C]()(implicit val time: Timestamp[T, C]) {
  /* A timestamp address just bundles a name address with a timestamp */
  case class A(nameAddr: NameAddress.A, t: T) extends Address {
    def printable         = nameAddr.printable
    override def toString = nameAddr.toString
  }
  val nameAlloc = NameAddress.Alloc[T, C]
  object Alloc extends Allocator[A, T, C] {
    implicit val timestamp: Timestamp[T, C] = time
    def variable(name: Identifier, t: T): A = A(nameAlloc.variable(name, t), t)
    def pointer[E <: Exp](e: E, t: T): A    = A(nameAlloc.pointer[E](e, t), t)
    def primitive(name: String): A          = A(nameAlloc.primitive(name), timestamp.initial(""))
  }
}
