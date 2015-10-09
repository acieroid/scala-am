import AbstractValue._

trait Output[Abs] {
  def finalValues: Set[Abs]
  def containsFinalValue(v: Abs): Boolean
  def toDotFile(path: String): Unit
}

trait AbstractMachine[Exp, Abs, Addr] {
  implicit def abs : AbstractValue[Abs]
  implicit def absi : AbstractInjection[Abs]
  implicit def addr : Address[Addr]
  implicit def addri : AddressInjection[Addr]
  implicit def exp : Expression[Exp]

  def name: String
  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr], graph: Boolean): Output[Abs]
}
