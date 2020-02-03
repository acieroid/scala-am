package scalaam.primitiveCompilation

trait PrimOp { val arity: Int; val name: String }

case object NumEq   extends PrimOp { val arity = 2; val name = "=" }
case object NumPlus extends PrimOp { val arity = 2; val name = "+" }

case object And     extends PrimOp { val arity = 2; val name = "and"}
case object Or      extends PrimOp { val arity = 2; val name = "or"}

case object Null    extends PrimOp { val arity = 1; val name = "null?"}
case object Car     extends PrimOp { val arity = 1; val name = "car"}
case object Cdr     extends PrimOp { val arity = 1; val name = "cdr"}

object PrimitiveOperations {
  val ops = List(NumEq, NumPlus, And, Or, Null, Car, Cdr)
  val opNams = ops.map(_.name)
}