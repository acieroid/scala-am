package scalaam.language.scheme

import scalaam.core._
import scalaam.lattice._
import scalaam.util._
import SchemeOps._
import UnaryOperator._
import BinaryOperator._
import scalaam.language.scheme.primitives.SchemePrimitive

class TypeSchemeLattice[A <: Address, Env] {
  type P = SchemePrimitive[L, A]

  case class L(str: Boolean = false, bool: Boolean = false, num: Boolean = false, char: Boolean = false, sym: Boolean = false, nil: Boolean = false, prims: Set[P] = Set.empty, clos: Set[((SchemeLambdaExp, Env), Option[String])] = Set.empty, consCells: Set[(A, A)] = Set.empty) extends SmartHash {
    def isBottom = !str && !bool && !num && !char && !sym && !nil && prims.isEmpty && clos.isEmpty && consCells.isEmpty
  }
  object Inject {
    val bottom: L = L()
    val str: L = L(str = true)
    val bool: L = L(bool = true)
    val num: L = L(num = true)
    val char: L = L(char = true)
    val sym: L = L(sym = true)
    val nil: L = L(nil = true)
    def prim(p: P): L = L(prims = Set(p))
    def clo(clo: schemeLattice.Closure, name: Option[String]): L = L(clos = Set((clo, name)))
    def cons(car: A, cdr: A): L = L(consCells = Set((car, cdr)))
  }

  def check(b: Boolean, v: L)(name: String, args: List[L]): MayFail[L, Error] =
    if (b) { MayFail.success(v) } else { MayFail.failure(OperatorNotApplicable(name, args)) }

  val schemeLattice: SchemeLattice[L, A, P, Env] = new SchemeLattice[L, A, P, Env] {
    def show(x: L): String = s"$x"
    def isTrue(x: L): Boolean = true // only "false" is not true, but we only have Bool represented
    def isFalse(x: L): Boolean = x.bool
    def unaryOp(op: UnaryOperator)(x: L) =
      if (x.isBottom) { MayFail.success(x) } else { op match {
      case IsNull | IsCons | IsPointer | IsChar | IsSymbol | IsInteger
         | IsString | IsReal | IsBoolean | IsVector | Not =>
          // Any -> Bool
          MayFail.success(Inject.bool)
      case Ceiling | Floor | Round | Log | Random | Sin | Cos
         | ACos | Tan | ATan | Sqrt | ExactToInexact | InexactToExact =>
          // Num -> Num
          check(x.num, Inject.num)(op.toString, List(x))
      case VectorLength =>
          // Vector -> Num
          ???
      case StringLength =>
          // String -> Num
          check(x.str, Inject.str)(op.toString, List(x))
      case NumberToString =>
          // Number -> String
          check(x.num, Inject.str)(op.toString, List(x))
      case SymbolToString =>
          // Symbol -> String
          check(x.sym, Inject.str)(op.toString, List(x))
      case StringToSymbol =>
          // String -> Symbol
          check(x.str, Inject.sym)(op.toString, List(x))
      case CharacterToInteger =>
          // Char -> Num
          check(x.char, Inject.num)(op.toString, List(x))
    }}
    def binaryOp(op: BinaryOperator)(x: L, y: L): MayFail[L, Error] =
      if (x.isBottom || y.isBottom) { MayFail.success(Inject.bottom) } else { op match {
        case Plus | Minus | Times | Quotient | Div | Expt | Modulo | Remainder =>
          // Num -> Num -> Num
          check(x.num && y.num, Inject.num)(op.toString, List(x, y))
        case Lt | NumEq =>
          // Num -> Num -> Bool
          check(x.num && y.num, Inject.num)(op.toString, List(x, y))
        case Eq =>
          // Any -> Any -> Bool
          MayFail.success(Inject.bool)
        case StringAppend =>
          // Str -> Str -> Str
          check(x.str && y.str, Inject.str)(op.toString, List(x, y))
        case StringRef =>
          // Str -> Num -> Char
          check(x.str && y.num, Inject.char)(op.toString, List(x, y))
        case StringLt =>
          // Str -> Str -> Bool
          check(x.str && y.str, Inject.bool)(op.toString, List(x, y))
      }}
    def join(x: L, y: => L): L =
      L(str = x.str || y.str,
        bool = x.bool || y.bool,
        num = x.num || y.num,
        char = x.char || y.char,
        sym = x.sym || y.sym,
        nil = x.nil || y.nil,
        prims = x.prims.union(y.prims),
        clos = x.clos.union(y.clos),
        consCells = x.consCells.union(y.consCells))
    def subsumes(x: L, y: => L): Boolean =
      ((if (x.str) y.str else true) &&
        (if (x.bool) y.bool else true) &&
        (if (x.num) y.num else true) &&
        (if (x.char) y.char else true) &&
        (if (x.sym) y.sym else true) &&
        (if (x.nil) y.nil else true) &&
        y.prims.subsetOf(x.prims) &&
        y.clos.subsetOf(y.clos) &&
        y.consCells.subsetOf(y.consCells))
    def top: L = ???
    def vectorRef(v: L, idx: L): MayFail[L, Error] = ???
    def vectorSet(v: L, idx: L, newval: L): MayFail[L, Error] = ???
    def getClosures(x: L): Set[(Closure, Option[String])] = x.clos
    def getConsCells(x: L): Set[(A, A)] = x.consCells
    def getPrimitives(x: L): Set[P] = x.prims
    def getPointerAddresses(x: L): Set[A] = ???

    def bottom: L = Inject.bottom
    def number(x: scala.Int): L = Inject.num
    def numTop: L = Inject.num
    def real(x: Double): L = Inject.num
    def string(x: String): L = Inject.str
    def bool(x: Boolean): L = Inject.bool
    def char(x: scala.Char): L = Inject.char
    def primitive(x: P): L                    = Inject.prim(x)
    def closure(x: schemeLattice.Closure, name: Option[String]): L  = Inject.clo(x, name)
    def symbol(x: String): L                  = Inject.sym
    def nil: L                                = Inject.nil
    def cons(car: A, cdr: A): L               = Inject.cons(car, cdr)
    def pointer(a: A): L                      = ???
    def eql[B : BoolLattice](x: L, y: L) = BoolLattice[B].top /* could be refined in some cases */
    def vector(size: L, init: L): MayFail[L, Error] = ???
    def split(v: L): Set[L] = ???
    def cardinality(x: L) = ???
  }
  object L {
    implicit val lattice: SchemeLattice[L, A, P, Env] = schemeLattice
  }
}
