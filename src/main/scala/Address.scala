trait Address[A] {
  def subsumes(x: A, y: A): Boolean = x.equals(y)
  def isPrimitive(x: A): Boolean
}

trait AddressInjection[A] {
  def name: String
  def primitive(name: String): A
  def variable(name: String): A
  def cell[Exp : Expression](exp: Exp): A
}

trait ClassicalAddress {
  def subsumes(that: ClassicalAddress) = this.equals(that)
}

object ClassicalAddress {
  case class VariableAddress(name: String) extends ClassicalAddress
  case class PrimitiveAddress(name: String) extends ClassicalAddress
  case class CellAddress[Exp : Expression](exp: Exp) extends ClassicalAddress

  implicit object ClassicalAddressAddress extends Address[ClassicalAddress] {
    override def subsumes(x: ClassicalAddress, y: ClassicalAddress) = x.subsumes(y)
    def isPrimitive(x: ClassicalAddress) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
  }
  implicit object ClassicalAddressInjection extends AddressInjection[ClassicalAddress] {
    def name = "Classical"
    def primitive(name: String) = PrimitiveAddress(name)
    def variable(name: String) = VariableAddress(name)
    def cell[Exp : Expression](exp: Exp) = CellAddress(exp)
  }
}

trait ConcreteAddress {
  def subsumes(that: ConcreteAddress) = this.equals(that)
}

object ConcreteAddress {
  case class PrimitiveAddress(name: String) extends ConcreteAddress
  case class IntAddress(name: String, id: Int) extends ConcreteAddress
  implicit object ConcreteAddressAddress extends Address[ConcreteAddress] {
    override def subsumes(x: ConcreteAddress, y: ConcreteAddress) = x.subsumes(y)
    def isPrimitive(x: ConcreteAddress) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
  }
  var id = 0
  implicit object ConcreteAddressInjection extends AddressInjection[ConcreteAddress] {
    def name = "Concrete"
    def primitive(name: String) = { PrimitiveAddress(name) }
    def variable(name: String) = { id += 1; IntAddress(name, id) }
    def cell[Exp : Expression](exp: Exp) = { id += 1; IntAddress(s"cell-$exp", id) }
  }
}
