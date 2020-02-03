package primitiveCompilation

trait PrimOp { val arity: Int, val name: String }

case object NumEq   extends PrimOp { val arity = 2; val name = "=" }
case object NumPlus extends PrimOp { val arity = 2; val name = "+" }

case object And     extends PrimOp { val arity = 2; val name = "and"}
case object Or      extends PrimOp { val arity = 2; val name = "or"}

object PrimitiveOperations {
  val ops = List(NumEq, NumPlus)
}