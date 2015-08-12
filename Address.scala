trait Address[A] {
  def subsumes(x: A, y: A) = x.equals(y)
}

trait AddressInjection[A] {
  def halt: A
  def primitive(name: String): A
  def variable(name: String): A
  def kont[Exp : Expression](exp: Exp): A
}

trait ClassicalAddress {
  def subsumes(that: ClassicalAddress) = this.equals(that)
}

object ClassicalAddress {
  case class VariableAddress(name: String) extends ClassicalAddress
  case class PrimitiveAddress(name: String) extends ClassicalAddress
  abstract class KontAddress extends ClassicalAddress
  case class NormalKontAddress[Exp : Expression](exp: Exp) extends KontAddress
  case class HaltKontAddress() extends KontAddress

  implicit object ClassicalAddressAddress extends Address[ClassicalAddress] {
    override def subsumes(x: ClassicalAddress, y: ClassicalAddress) = x.subsumes(y)
  }
  implicit object ClassicalAddressInjection extends AddressInjection[ClassicalAddress] {
    def halt = HaltKontAddress()
    def primitive(name: String) = PrimitiveAddress(name)
    def variable(name: String) = VariableAddress(name)
    def kont[Exp : Expression](exp: Exp) = NormalKontAddress(exp)
  }
}

trait ConcreteAddress {
  def subsumes(that: ConcreteAddress) = this.equals(that)
}

object ConcreteAddress {
  case class IntAddress(name: String, id: Int) extends ConcreteAddress
  implicit object ConcreteAddressAddress extends Address[ConcreteAddress] {
    override def subsumes(x: ConcreteAddress, y: ConcreteAddress) = x.subsumes(y)
  }
  var id = 0
  implicit object ConcreteAddressInjection extends AddressInjection[ConcreteAddress] {
    def halt = IntAddress("halt", 0)
    def primitive(name: String) = { id += 1; IntAddress(name, id) }
    def variable(name: String) = { id += 1; IntAddress(name, id) }
    def kont[Exp : Expression](exp: Exp) = { id += 1; IntAddress(s"kont-$exp", id) }
  }
}
