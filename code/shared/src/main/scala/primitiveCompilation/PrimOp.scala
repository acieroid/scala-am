package scalaam.primitiveCompilation

// TODO: Do we need the arity here?
trait PrimOp {
  val arity: Int
  val name: String
  override def toString = name.toString
}

case object NumEq   extends PrimOp { val arity = 2; val name = "="; override def toString: String = "numEq" }
case object NumPlus extends PrimOp { val arity = 2; val name = "+"; override def toString: String = "numPlus" }

case object And     extends PrimOp { val arity = 2; val name = "and"}
case object Or      extends PrimOp { val arity = 2; val name = "or"}

case object Null    extends PrimOp { val arity = 1; val name = "null?"; override def toString: String = "isNull"}
case object Car     extends PrimOp { val arity = 1; val name = "car"}
case object Cdr     extends PrimOp { val arity = 1; val name = "cdr"}

object PrimitiveOperations {
  val    ops: List[PrimOp] = List(NumEq, NumPlus, And, Or, Null, Car, Cdr)
  val opNams: List[String] = ops.map(_.name)
}