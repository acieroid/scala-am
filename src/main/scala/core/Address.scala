trait Address[A] {
  def name: String
  def isPrimitive(x: A): Boolean
  def primitive(name: String): A
  def variable[Time : Timestamp, Abs : JoinLattice](id: Identifier, value: Abs, t: Time): A
  def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time): A
  def botAddress: A = primitive("__bottom__")
}

object Address {
  def apply[A : Address]: Address[A] = implicitly
}

trait AddressWrapper {
  type A
  val isAddress: Address[A]
}

object ClassicalAddress extends AddressWrapper {
  trait A
  case class VariableAddress[Time : Timestamp](id: Identifier, t: Time) extends A {
    override def toString = s"@$id"
  }
  case class PrimitiveAddress(name: String) extends A {
    override def toString = s"@$name"
  }
  case class CellAddress[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) extends A {
    override def toString = s"@$exp"
  }

  implicit val isAddress = new Address[A] {
    def name = "Classical"
    def isPrimitive(x: A) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def primitive(name: String) = PrimitiveAddress(name)
    def variable[Time : Timestamp, Abs : JoinLattice](id: Identifier, value: Abs, t: Time) = VariableAddress(id, t)
    def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = CellAddress(exp, t)
  }
}

object ValueSensitiveAddress extends AddressWrapper {
  trait A
  case class VariableAddress[Time : Timestamp, Abs : JoinLattice](id: Identifier, value: Abs, t: Time) extends A {
    override def toString = s"@($id,$value)"
  }
  case class PrimitiveAddress(name: String) extends A {
    override def toString = s"@$name"
  }
  case class CellAddress[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) extends A {
    override def toString = s"@$exp"
  }

  implicit val isAddress = new Address[A] {
    def name = "ValueSensitive"
    def isPrimitive(x: A) = x match {
      case PrimitiveAddress(_) => true
      case _ => false
    }
    def primitive(name: String) = PrimitiveAddress(name)
    def variable[Time : Timestamp, Abs : JoinLattice](id: Identifier, value: Abs, t: Time) = {
      /* To ensure finiteness, value should be a primitive value that doesn't contain addresses (i.e., no cons cell etc.) */
      VariableAddress(id, if (JoinLattice[Abs].isPrimitiveValue(value)) value else JoinLattice[Abs].bottom, t)
    }
    def cell[Exp : Expression, Time : Timestamp](exp: Exp, t: Time) = CellAddress(exp, t)
  }
}
