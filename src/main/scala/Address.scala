trait Address[A] {
  def name: String
  def subsumes(x: A, y: A): Boolean = x.equals(y)
  def isPrimitive(x: A): Boolean
  def primitive(name: String): A
  def variable(name: String): A
  def cell[Exp : Expression](exp: Exp): A
}

trait ClassicalAddress

object ClassicalAddress {
  case class VariableAddress(name: String) extends ClassicalAddress
  case class PrimitiveAddress(name: String) extends ClassicalAddress
  case class CellAddress[Exp : Expression](exp: Exp) extends ClassicalAddress

  implicit object ClassicalAddressAddress extends Address[ClassicalAddress] {
    def name = "Classical"
    def isPrimitive(x: ClassicalAddress) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def primitive(name: String) = PrimitiveAddress(name)
    def variable(name: String) = VariableAddress(name)
    def cell[Exp : Expression](exp: Exp) = CellAddress(exp)
  }
}

trait ConcreteAddress

object ConcreteAddress {
  var id = 0
  case class PrimitiveAddress(name: String) extends ConcreteAddress
  case class IntAddress(name: String, id: Int) extends ConcreteAddress
  implicit object ConcreteAddressAddress extends Address[ConcreteAddress] {
    def name = "Concrete"
    def isPrimitive(x: ConcreteAddress) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def primitive(name: String) = { PrimitiveAddress(name) }
    def variable(name: String) = { id += 1; IntAddress(name, id) }
    def cell[Exp : Expression](exp: Exp) = { id += 1; IntAddress(s"cell-$exp", id) }
  }
}
