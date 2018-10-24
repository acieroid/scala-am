package scalaam.core

/** An address */
trait Address {

  /** Should the address be included when printing an environment or store?
      This allows to reduce the size of the printed environment/store.
      Address that are not printable may for example include addresses of primitive functions.
    */
  def printable: Boolean
}

trait Allocator[A <: Address, T, C] {
  implicit val timestamp: Timestamp[T, C]
  def variable(name: Identifier, t: T): A
  def pointer[E](e: E, t: T): A
  def primitive(name: String): A
}

object NameAddress {
  trait A extends Address

  case class Variable(name: Identifier) extends A {
    def printable         = true
    override def toString = s"@${name.name}"
  }
  case class Pointer[E](e: E) extends A {
    def printable = false
  }
  case class Primitive(name: String) extends A {
    def printable = false
  }

  case class Alloc[T, C]()(implicit val timestamp: Timestamp[T, C]) extends Allocator[A, T, C] {
    def variable(name: Identifier, t: T): A = Variable(name)
    def pointer[E](e: E, t: T): A = Pointer(e)
    def primitive(name: String) = Primitive(name)
  }
}
