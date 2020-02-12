package scalaam.primitiveCompilation

// TODO: Do we need the arity here?
trait PrimOp {
  val arity: Int
  val name: String
  override def toString = name.toString
}

case object Intp    extends PrimOp { val arity = 1; val name = "integer?"; override def toString: String = "Integerp" }
case object Realp   extends PrimOp { val arity = 1; val name = "real?"; override def toString: String = "Realp" }

case object NumEq   extends PrimOp { val arity = -1; val name = "="; override def toString: String = "NumEq" }
case object NumLt   extends PrimOp { val arity = 2; val name = "<"; override def toString: String = "LessThan" }
case object NumGt   extends PrimOp { val arity = 2; val name = ">"; override def toString: String = "GreaterThan" }

case object NumPlus extends PrimOp { val arity = -1; val name = "+"; override def toString: String = "Plus" }
case object NumMult extends PrimOp { val arity = -1; val name = "*"; override def toString: String = "Times" }
case object NumMin  extends PrimOp { val arity = -1; val name = "-"; override def toString: String = "Minus" }
case object NumDiv  extends PrimOp { val arity = -1; val name = "/"; override def toString: String = "Div" }
case object NumCos  extends PrimOp { val arity = 1; val name = "cos"; override def toString: String = "Cos" }
case object NumSin  extends PrimOp { val arity = 1; val name = "sin"; override def toString: String = "Sin" }
case object NumTan  extends PrimOp { val arity = 1; val name = "tan"; override def toString: String = "Tan" }
case object NumAcos extends PrimOp { val arity = 1; val name = "acos"; override def toString: String = "ACos" }
case object NumAsin extends PrimOp { val arity = 1; val name = "asin"; override def toString: String = "ASin" }
case object NumAtan extends PrimOp { val arity = 1; val name = "atan"; override def toString: String = "ATan" }

case object Boolp   extends PrimOp { val arity = 1; val name = "boolean?"; override def toString: String = "Booleanp" }
case object And     extends PrimOp { val arity = 2; val name = "and"; override def toString: String = "And" }
case object Or      extends PrimOp { val arity = 2; val name = "or"; override def toString: String = "Or" }

case object Charp   extends PrimOp { val arity = 1; val name = "char?"; override def toString: String = "Charp" }

case object Stringp extends PrimOp { val arity = 1; val name = "string?"; override def toString: String = "Stringp" }
case object StrLen  extends PrimOp { val arity = 1; val name = "string-length"; override def toString: String = "StringLength" }
case object StrApp  extends PrimOp { val arity = 1; val name = "string-append"; override def toString: String = "StringAppend" }
case object StrRef  extends PrimOp { val arity = 1; val name = "string-ref"; override def toString: String = "StringRef" }
case object StrEq   extends PrimOp { val arity = 1; val name = "string=?"; override def toString: String = "StringEq" }
case object StrLt   extends PrimOp { val arity = 1; val name = "string<?"; override def toString: String = "StringLt" }

case object Null    extends PrimOp { val arity = 1; val name = "null?"; override def toString: String = "Nullp" }
case object Lst     extends PrimOp { val arity = -1; val name = "list"; override def toString: String = "List" }
case object Cons    extends PrimOp { val arity = 2; val name = "cons"; override def toString: String = "Cons" }
case object Car     extends PrimOp { val arity = 1; val name = "car"; override def toString: String = "Car" }
case object Cdr     extends PrimOp { val arity = 1; val name = "cdr"; override def toString: String = "Cdr" }

case object Vector  extends PrimOp { val arity = -1; val name = "vector"; override def toString: String = "Vector" } // TODO Fix arity?
case object VectRef extends PrimOp { val arity = 2; val name = "vector-ref"; override def toString: String = "VectorRef" }
case object VectSet extends PrimOp { val arity = 3; val name = "vector-set!"; override def toString: String = "VectorSet" }

case object Eq      extends PrimOp { val arity = 2; val name = "eq?"; override def toString: String = "Eq"}

object PrimitiveOperations {
  val ops: List[PrimOp] =
    List(Intp, Realp,
         NumEq, NumLt, NumGt,
         NumPlus, NumMult, NumMin, NumDiv, NumCos, NumSin, NumTan, NumAcos, NumAsin, NumAtan,
         Boolp, And, Or,
         Charp,
         Stringp, StrLen, StrApp, StrRef, StrEq, StrLt,
         Null, Lst, Cons, Car, Cdr,
         Vector, VectRef, VectSet,
         Eq)
  val opNams: List[String] = ops.map(_.name)
  val storeOps: List[PrimOp] = List(Cons, Car, Cdr, Vector, VectRef, VectSet)
  val stoNams: List[String] = storeOps.map(_.name)
  val allocOps: List[PrimOp] = List(Cons, Vector)
  val alcNams: List[String] = allocOps.map(_.name)
}