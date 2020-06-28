package scalaam.language.scheme

import scalaam.core._
import scalaam.lattice._
import scalaam.util._
import SchemeOps._
import scalaam.language.CScheme.TID
import scalaam.language.scheme.primitives._

class TypeSchemeLattice[A <: Address] {
  type P = SchemePrimitive[L, A]

  case class L(str: Boolean = false, bool: Boolean = false, num: Boolean = false, char: Boolean = false, sym: Boolean = false, nil: Boolean = false, prims: Set[P] = Set.empty, clos: Set[((SchemeLambdaExp, schemeLattice.Env), Option[String])] = Set.empty, ptrs: Set[A] = Set.empty, consCells: (L,L) = (L(),L())) extends SmartHash {
    def isBottom: Boolean = !str && !bool && !num && !char && !sym && !nil && prims.isEmpty && clos.isEmpty && consCells._1.isBottom && consCells._2.isBottom
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
    def pointer(a: A): L = L(ptrs = Set(a))
    def clo(clo: schemeLattice.Closure, name: Option[String]): L = L(clos = Set((clo, name)))
    def cons(car: L, cdr: L): L = L(consCells = (car,cdr))
  }

  def check(b: Boolean, v: L)(name: String, args: List[L]): MayFail[L, Error] =
    if (b) { MayFail.success(v) } else { MayFail.failure(OperatorNotApplicable(name, args)) }

  val schemeLattice: SchemeLattice[L, A, P] = new SchemeLattice[L, A, P] {
    def show(x: L): String = s"$x"
    def isTrue(x: L): Boolean = true // only "false" is not true, but we only have Bool represented
    def isFalse(x: L): Boolean = x.bool
    def unaryOp(op: UnaryOperator)(x: L) = {
      import UnaryOperator._
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
    }}}
    def binaryOp(op: BinaryOperator)(x: L, y: L): MayFail[L, Error] = {
      import BinaryOperator._
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
      }}}
    def join(x: L, y: => L): L =
      L(str = x.str || y.str,
        bool = x.bool || y.bool,
        num = x.num || y.num,
        char = x.char || y.char,
        sym = x.sym || y.sym,
        nil = x.nil || y.nil,
        prims = x.prims.union(y.prims),
        clos = x.clos.union(y.clos),
        ptrs = x.ptrs.union(y.ptrs),
        consCells = (join(x.consCells._1,y.consCells._1),
                     join(x.consCells._2,y.consCells._2)))
    def subsumes(x: L, y: => L): Boolean =
      ((if (x.str) y.str else true) &&
        (if (x.bool) y.bool else true) &&
        (if (x.num) y.num else true) &&
        (if (x.char) y.char else true) &&
        (if (x.sym) y.sym else true) &&
        (if (x.nil) y.nil else true) &&
        y.prims.subsetOf(x.prims) &&
        y.clos.subsetOf(y.clos) &&
        subsumes(x.consCells._1, y.consCells._1) &&
        subsumes(x.consCells._1, y.consCells._2))
    def top: L = ???
    def vectorRef(v: L, idx: L): MayFail[L, Error] = ???
    def vectorSet(v: L, idx: L, newval: L): MayFail[L, Error] = ???
    def getClosures(x: L): Set[(Closure, Option[String])] = x.clos
    def car(x: L): MayFail[L,Error] = MayFail.success(x.consCells._1)
    def cdr(x: L): MayFail[L,Error] = MayFail.success(x.consCells._2)
    def getPrimitives(x: L): Set[P] = x.prims
    def getPointerAddresses(x: L): Set[A] = Set()
    def getThreads(x: L): Set[TID] = throw new Exception("Not supported.")

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
    def cons(car: L, cdr: L): L               = Inject.cons(car, cdr)
    def pointer(a: A): L                      = Inject.pointer(a)
    def eql[B : BoolLattice](x: L, y: L) = BoolLattice[B].top /* could be refined in some cases */
    def vector(size: L, init: L): MayFail[L, Error] = ???
    def thread(tid: TID): L                   = ???
    def split(v: L): Set[L] = ???
  }
  object L {
    implicit val lattice: SchemeLattice[L, A, P] = schemeLattice
  }

  object Primitives extends SchemeLatticePrimitives[L, A] with PrimitiveBuildingBlocks[L,A] {
    override def allPrimitives = super.allPrimitives ++ List(
      `abs`,
      // `assoc`, // TODO
      // `assq`, // TODO
      // `assv`, // TODO
      `display`,
      `equal?`,
      `eqv?`,
      `even?`,
      `gcd`,
      `lcm`,
      `length`,
      // `list-ref`, // TODO
      // `list->vector`, // TODO? or not
      // `list-tail`, // TODO
      `list?`,
      // `member`, // TODO
      // `memq`, // TODO
      // `memv`, // TODO
      `negative?`,
      `newline`,
      `not`,
      `odd?`,
      `positive?`,
      `zero?`,
      `<=`,
      `>`,
      `>=`,
      `caar`, `cadr`, `cdar`, `cddr`,
      `caddr`, `cdddr`, `caadr`, `cdadr`,
      `cadddr`,
      // TODO: other cxr
      // `vector->list // TODO
      // We decided not to implement some primitives as they can't be properly supported in the framework: reverse, map, for-each, apply
    )
    class SimplePrim(val name: String, ret: L) extends SchemePrimitive[L, A] {
      def call(fexp: SchemeExp, args: List[(SchemeExp, L)], store: Store[A, L], alloc: SchemeAllocator[A]): MayFail[(L, Store[A, L]), Error] =
        MayFail.success((ret, store))
    }
    object `abs` extends SimplePrim("abs", Inject.num)
    object `display` extends SimplePrim("display", Inject.str) // undefined behavior in R5RS
    object `equal?` extends SimplePrim("equal?", Inject.bool)
    object `eqv?` extends SimplePrim("eqv?", Inject.bool)
    object `even?` extends SimplePrim("even?", Inject.bool)
    object `gcd` extends SimplePrim("gcd", Inject.num)
    object `lcm` extends SimplePrim("lcm", Inject.num)
    object `length` extends SimplePrim("length", Inject.num)
    object `list?` extends SimplePrim("list?", Inject.bool)
    object `negative?` extends SimplePrim("negative?", Inject.bool)
    object `newline` extends SimplePrim("newline", Inject.bool)
    object `not` extends SimplePrim("not", Inject.bool)
    object `odd?` extends SimplePrim("odd?", Inject.bool)
    object `positive?` extends SimplePrim("positive?", Inject.bool)
    object `zero?` extends SimplePrim("zero?", Inject.bool)
    object `<=` extends SimplePrim("<=", Inject.bool)
    object `>` extends SimplePrim(">", Inject.bool)
    object `>=` extends SimplePrim(">=", Inject.bool)
    object `caar` extends Store1Operation("caar", { (x, store) =>
      dereferencePointer(x, store) { c1 => 
        L.lattice.car(c1) >>= { car =>
          dereferencePointer(car, store) { c2 =>
            L.lattice.car(c2)
          }
        }
      }.map((_, store))
    })
    object `cadr` extends Store1Operation("cadr", { (x, store) =>
      dereferencePointer(x, store) { c1 => 
        L.lattice.cdr(c1) >>= { cdr =>
          dereferencePointer(cdr, store) { c2 =>
            L.lattice.car(c2)
          }
        }
      }.map((_, store))
    })
    object `cdar` extends Store1Operation("cdar", { (x, store) =>
      dereferencePointer(x, store) { c1 => 
        L.lattice.car(c1) >>= { car =>
          dereferencePointer(car, store) { c2 =>
            L.lattice.cdr(c2)
          }
        }
      }.map((_, store))
    })
    object `cddr` extends Store1Operation("cddr", { (x, store) =>
      dereferencePointer(x, store) { c1 => 
        L.lattice.cdr(c1) >>= { cdr =>
          dereferencePointer(cdr, store) { c2 =>
            L.lattice.cdr(c2)
          }
        }
      }.map((_, store))
    })
    object `caddr` extends Store1Operation("caddr", { (x, store) =>
      dereferencePointer(x, store) { c1 => 
        L.lattice.cdr(c1) >>= { cdr =>
          dereferencePointer(cdr, store) { c2 =>
            L.lattice.cdr(c2) >>= { cddr =>
              dereferencePointer(cddr, store) { c3 => 
                L.lattice.car(c3)
              }
            }
          }
        }
      }.map((_, store))
    })
    object `caadr` extends Store1Operation("caadr", { (x, store) =>
      dereferencePointer(x, store) { c1 => 
        L.lattice.cdr(c1) >>= { cdr =>
          dereferencePointer(cdr, store) { c2 =>
            L.lattice.car(c2) >>= { cadr =>
              dereferencePointer(cadr, store) { c3 => 
                L.lattice.car(c3)
              }
            }
          }
        }
      }.map((_, store))
    })
    object `cdadr` extends Store1Operation("cdadr", { (x, store) =>
      dereferencePointer(x, store) { c1 => 
        L.lattice.cdr(c1) >>= { cdr =>
          dereferencePointer(cdr, store) { c2 =>
            L.lattice.car(c2) >>= { cadr =>
              dereferencePointer(cadr, store) { c3 => 
                L.lattice.cdr(c3)
              }
            }
          }
        }
      }.map((_, store))
    })
    object `cdddr` extends Store1Operation("cdddr", { (x, store) =>
      dereferencePointer(x, store) { c1 => 
        L.lattice.cdr(c1) >>= { cdr =>
          dereferencePointer(cdr, store) { c2 =>
            L.lattice.cdr(c2) >>= { cddr =>
              dereferencePointer(cddr, store) { c3 => 
                L.lattice.cdr(c3)
              }
            }
          }
        }
      }.map((_, store))
    })
    object `cadddr` extends Store1Operation("cadddr", { (x, store) =>
      dereferencePointer(x, store) { c1 => 
        L.lattice.cdr(c1) >>= { cdr =>
          dereferencePointer(cdr, store) { c2 =>
            L.lattice.cdr(c2) >>= { cddr =>
              dereferencePointer(cddr, store) { c3 => 
                L.lattice.cdr(c3) >>= { cdddr => 
                  dereferencePointer(cdddr, store) { c4 =>
                    L.lattice.cdr(c4)
                  }
                }
              }
            }
          }
        }
      }.map((_, store))
    })
  }
}
