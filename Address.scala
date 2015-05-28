trait Address[A] {
  def subsumes(x: A, y: A) = x.equals(y)
}

trait AddressInjection[A] {
  def halt: A
  def primitive(name: String): A
  def variable(name: String): A
  def kont[Exp <: Expression](exp: Exp): A
}

trait ClassicalAddress {
  def subsumes(that: ClassicalAddress) = this.equals(that)
}
case class VariableAddress(name: String) extends ClassicalAddress
case class PrimitiveAddress(name: String) extends ClassicalAddress
abstract class KontAddress extends ClassicalAddress
case class NormalKontAddress[Exp <: Expression](exp: Exp) extends KontAddress
case class HaltKontAddress() extends KontAddress

object Address {
  implicit object ClassicalAddressAddress extends Address[ClassicalAddress] {
    override def subsumes(x: ClassicalAddress, y: ClassicalAddress) = x.subsumes(y)
  }
  implicit object ClassicalAddressInjection extends AddressInjection[ClassicalAddress] {
    def halt = HaltKontAddress()
    def primitive(name: String) = PrimitiveAddress(name)
    def variable(name: String) = VariableAddress(name)
    def kont[Exp <: Expression](exp: Exp) = NormalKontAddress(exp)
  }
}
