package primitiveCompilation

trait PrimOp { val arity: Int }

case object NumEq   extends PrimOp { val arity = 2 }
case object NumPlus extends PrimOp { val arity = 2 }
