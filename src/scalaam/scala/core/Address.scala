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
}

object NameAddress {
  case class A(name: Identifier) extends Address {
    def printable = true
    override def toString = s"@${name.name}"
  }
  case class Alloc[T, C]()(implicit val timestamp: Timestamp[T, C]) extends Allocator[A, T, C] {
    def variable(name: Identifier, t: T) = A(name)
  }
}

