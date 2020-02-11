package scalaam.primitiveCompilation

// TODO: Do we need the arity here?
trait PrimOp {
  val arity: Int
  val name: String
  override def toString = name.toString
}

case object NumEq   extends PrimOp { val arity = 2; val name = "="; override def toString: String = "numEq" }
case object NumPlus extends PrimOp { val arity = 2; val name = "+"; override def toString: String = "numPlus" }

case object And     extends PrimOp { val arity = 2; val name = "and"; override def toString: String = "And"}
case object Or      extends PrimOp { val arity = 2; val name = "or"; override def toString: String = "Or"}

case object Null    extends PrimOp { val arity = 1; val name = "null?"; override def toString: String = "Nullp"}
case object Cons    extends PrimOp { val arity = 2; val name = "cons"; override def toString: String = "Cons"}
case object Car     extends PrimOp { val arity = 1; val name = "car"; override def toString: String = "Car"}
case object Cdr     extends PrimOp { val arity = 1; val name = "cdr"; override def toString: String = "Cdr"}
case object Vector  extends PrimOp { val arity = -1; val name = "vector"; override def toString: String = "Vector"} // TODO Fix arity
case object VectRef extends PrimOp { val arity = 2; val name = "vector-ref"; override def toString: String = "VectorRef"}
case object VectSet extends PrimOp { val arity = 3; val name = "vector-set!"; override def toString: String = "VectorSet"}

object PrimitiveOperations {
  val    ops: List[PrimOp] = List(NumEq, NumPlus, And, Or, Null, Cons, Car, Cdr, Vector, VectRef, VectSet)
  val opNams: List[String] = ops.map(_.name)
  val storeOps: List[PrimOp] = List(Null, Cons, Car, Cdr, Vector, VectRef, VectSet)
  val stoNams: List[String] = storeOps.map(_.name)
  val allocOps: List[PrimOp] = List(Cons, Vector)
  val alcNams: List[String] = allocOps.map(_.name)
}