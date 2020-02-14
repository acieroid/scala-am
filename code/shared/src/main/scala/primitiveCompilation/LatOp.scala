package scalaam.primitiveCompilation

// TODO: Do we need the arity here? If so, is an integer the right way to go?
trait LatOp {
  val arity: Int
  val name: String
  override def toString: String = name.toString.capitalize
}

// TODO: Now also primitives are in this list. They should be removed and a separate naming convention should be used for them.
// TODO: Thereafter, the compiler should output Name.call(...) for these primitives, and the .call can be removed from the LatOps (since they are in scope anyway).
// TODO: This is difficult however since a primCall takes an expression as primitive, and not necessarily a name (Where does this actually happen in practise except in Agda?).

case object Boolp   extends LatOp { val arity = 1; val name = "boolean?"; override def toString: String = "Booleanp" }
case object Charp   extends LatOp { val arity = 1; val name = "char?"; override def toString: String = "Charp" }
case object Intp    extends LatOp { val arity = 1; val name = "integer?"; override def toString: String = "Integerp" }
case object Pairp   extends LatOp { val arity = 1; val name = "pair?"; override def toString: String = "Pairp" }
case object Realp   extends LatOp { val arity = 1; val name = "real?"; override def toString: String = "Realp" }
case object Stringp extends LatOp { val arity = 1; val name = "string?"; override def toString: String = "Stringp" }

case object NumEq   extends LatOp { val arity = -1; val name = "="; override def toString: String = "NumEq" }
case object NumGt   extends LatOp { val arity = 2; val name = ">"; override def toString: String = "GreaterThan" }
case object NumLt   extends LatOp { val arity = 2; val name = "<"; override def toString: String = "LessThan" }

case object Acos    extends LatOp { val arity = 1; val name = "acos"; override def toString: String = "ACos" }
case object Asin    extends LatOp { val arity = 1; val name = "asin"; override def toString: String = "ASin" }
case object Atan    extends LatOp { val arity = 1; val name = "atan"; override def toString: String = "ATan" }
case object Ceiling extends LatOp { val arity = 1; val name = "ceiling" }
case object Cosinus extends LatOp { val arity = 1; val name = "cos" }
case object Div     extends LatOp { val arity = -1; val name = "/"; override def toString: String = "Div" }
case object Even    extends LatOp { val arity = 1; val name = "even?"; override def toString: String = "Evenp" }
case object Floor   extends LatOp { val arity = 1; val name = "floor" }
case object Gcd     extends LatOp { val arity = 2; val name = "gcd" }
case object Lcm     extends LatOp { val arity = 2; val name = "lcm" }
case object Log     extends LatOp { val arity = 1; val name = "log" }
case object Max     extends LatOp { val arity = 2; val name = "max" }
case object Min     extends LatOp { val arity = 2; val name = "min" }
case object Minus   extends LatOp { val arity = -1; val name = "-"; override def toString: String = "Minus" }
case object Modulo  extends LatOp { val arity = 2; val name = "modulo" }
case object Mult    extends LatOp { val arity = -1; val name = "*"; override def toString: String = "Times" }
case object Odd     extends LatOp { val arity = 1; val name = "odd?"; override def toString: String = "Oddp" }
case object Plus    extends LatOp { val arity = -1; val name = "+"; override def toString: String = "Plus" }
case object Sinus   extends LatOp { val arity = 1; val name = "sin" }
case object Tangent extends LatOp { val arity = 1; val name = "tan" }

case object And     extends LatOp { val arity = 2; val name = "and" }
case object Or      extends LatOp { val arity = 2; val name = "or" }

case object StrApp  extends LatOp { val arity = 2; val name = "string-append"; override def toString: String = "StringAppend" }
case object StrEq   extends LatOp { val arity = 2; val name = "string=?"; override def toString: String = "StringEq" }
case object StrLen  extends LatOp { val arity = 1; val name = "string-length"; override def toString: String = "StringLength" }
case object StrLt   extends LatOp { val arity = 2; val name = "string<?"; override def toString: String = "StringLt" }
case object StrRef  extends LatOp { val arity = 2; val name = "string-ref"; override def toString: String = "StringRef" }

case object Append  extends LatOp { val arity = 1; val name = "append" }
case object Cons    extends LatOp { val arity = 2; val name = "cons" }
case object Lst     extends LatOp { val arity = -1; val name = "list" }
case object Null    extends LatOp { val arity = 1; val name = "null?"; override def toString: String = "Nullp" }
case object LstLen  extends LatOp { val arity = 1; val name = "length" }
case object LstTail extends LatOp { val arity = 1; val name = "list-tail"; override def toString: String = "LstTail" }

case object Caaaar  extends LatOp { val arity = 1; val name = "caaaar" }
case object Caaadr  extends LatOp { val arity = 1; val name = "caaadr" }
case object Caaar   extends LatOp { val arity = 1; val name = "caaar" }
case object Caadar  extends LatOp { val arity = 1; val name = "caadar" }
case object Caaddr  extends LatOp { val arity = 1; val name = "caaddr" }
case object Caadr   extends LatOp { val arity = 1; val name = "caadr" }
case object Caar    extends LatOp { val arity = 1; val name = "caar" }
case object Cadaar  extends LatOp { val arity = 1; val name = "cadaar" }
case object Cadadr  extends LatOp { val arity = 1; val name = "cadadr" }
case object Cadar   extends LatOp { val arity = 1; val name = "cadar" }
case object Caddar  extends LatOp { val arity = 1; val name = "caddar" }
case object Cadddr  extends LatOp { val arity = 1; val name = "cadddr" }
case object Caddr   extends LatOp { val arity = 1; val name = "caddr" }
case object Cadr    extends LatOp { val arity = 1; val name = "cadr" }
case object Car     extends LatOp { val arity = 1; val name = "car" }
case object Cdaaar  extends LatOp { val arity = 1; val name = "cdaaar" }
case object Cdaadr  extends LatOp { val arity = 1; val name = "cdaadr" }
case object Cdaar   extends LatOp { val arity = 1; val name = "cdaar" }
case object Cdadar  extends LatOp { val arity = 1; val name = "cdadar" }
case object Cdaddr  extends LatOp { val arity = 1; val name = "cdaddr" }
case object Cdadr   extends LatOp { val arity = 1; val name = "cdadr" }
case object Cdar    extends LatOp { val arity = 1; val name = "cdar" }
case object Cddaar  extends LatOp { val arity = 1; val name = "cddaar" }
case object Cddadr  extends LatOp { val arity = 1; val name = "cddadr" }
case object Cddar   extends LatOp { val arity = 1; val name = "cddar" }
case object Cdddar  extends LatOp { val arity = 1; val name = "cdddar" }
case object Cddddr  extends LatOp { val arity = 1; val name = "cddddr" }
case object Cdddr   extends LatOp { val arity = 1; val name = "cdddr" }
case object Cddr    extends LatOp { val arity = 1; val name = "cddr" }
case object Cdr     extends LatOp { val arity = 1; val name = "cdr" }

case object Assoc   extends LatOp { val arity = 2; val name = "assoc" }
case object Assq    extends LatOp { val arity = 2; val name = "assq" }
case object Assv    extends LatOp { val arity = 2; val name = "assv" }

case object Member  extends LatOp { val arity = 2; val name = "member" }
case object Memq    extends LatOp { val arity = 2; val name = "memq" }
case object Memv    extends LatOp { val arity = 2; val name = "memv" }

case object Vector  extends LatOp { val arity = -1; val name = "vector" }
case object VectRef extends LatOp { val arity = 2; val name = "vector-ref"; override def toString: String = "VectorRef" }
case object VectSet extends LatOp { val arity = 3; val name = "vector-set!"; override def toString: String = "VectorSet" }

case object Eq      extends LatOp { val arity = 2; val name = "eq?" }
case object Equal   extends LatOp { val arity = 2; val name = "equal?" }
case object Eqv     extends LatOp { val arity = 2; val name = "eqv?"; override def toString: String = "Eqv" }

object LatticeOperations {
  val ops: List[LatOp] =
    List(Intp, Realp, Pairp,
         NumEq, NumLt, NumGt,
         Plus, Mult, Minus, Div, Cosinus, Sinus, Tangent, Acos, Asin, Atan, Gcd, Lcm, Floor, Ceiling, Log, Max, Min, Modulo,
         Boolp, And, Or,
         Charp,
         Stringp, StrLen, StrApp, StrRef, StrEq, StrLt,
         Null, Lst, Cons, Car, Cdr, Append, LstLen, LstTail,
         Caaaar, Caaadr, Caaar, Caadar, Caaddr, Caar, Cadaar, Cadadr, Caddar, Cadr, Car, Caadr, Cadar, Cadddr, Caddr,
         Cdaaar, Cdaadr, Cdaar, Cdadar, Cdaddr, Cdar, Cddaar, Cddadr, Cdddar, Cddr, Cdr, Cdadr, Cddar, Cddddr, Cdddr,
         Assoc, Assq, Assv,
         Member, Memq, Memv,
         Vector, VectRef, VectSet,
         Eq, Eqv, Equal)
  val opNams: List[String] = ops.map(_.name)
  val storeOps: List[LatOp] = List(
    Pairp, Lst, Cons, Car, Cdr, Append, LstLen, LstTail,
    Caaaar, Caaadr, Caaar, Caadar, Caaddr, Caar, Cadaar, Cadadr, Caddar, Cadr, Car, Caadr, Cadar, Cadddr, Caddr,
    Cdaaar, Cdaadr, Cdaar, Cdadar, Cdaddr, Cdar, Cddaar, Cddadr, Cdddar, Cddr, Cdr, Cdadr, Cddar, Cddddr, Cdddr,
    Assoc, Assq, Assv,
    Member, Memq, Memv,
    Vector, VectRef, VectSet, Equal)
  val stoNams: List[String] = storeOps.map(_.name)
  val allocOps: List[LatOp] = List(Cons, Vector)
  val alcNams: List[String] = allocOps.map(_.name)
}