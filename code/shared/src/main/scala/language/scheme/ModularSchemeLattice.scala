package scalaam.language.scheme

import scalaam.core._
import scalaam.lattice._
import scalaam.util._
import SchemeOps._
import UnaryOperator._
import BinaryOperator._
import scalaam.language.scheme.primitives.SchemePrimitive

/** Defines a Scheme lattice based on other lattices.
  * Example usage:
  *    val address = NameAddress
  *    val lattice = new ModularSchemeLattice[SchemeExp, address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym]
  * Now `lattice.L` is a SchemeLattice, of which the implicit for the typeclass is available in the current scope.
  */
/** TODO[medium]: use Show and ShowStore here */
class ModularSchemeLattice[
    A <: Address,
    Env,
    S: StringLattice,
    B: BoolLattice,
    I: IntLattice,
    R: RealLattice,
    C: CharLattice,
    Sym: SymbolLattice
] {

  type P = SchemePrimitive[L,A]

  implicit def mfAddrMonoid[X]: Monoid[MayFail[Set[X],Error]] = MonoidImplicits.mayFail[Set[X]](MonoidImplicits.setMonoid[X])

  /** We first implement all possible operations on single values, that can be
    * only joined when compatible. This therefore is not a lattice but will be
    * used to build the set lattice */
  sealed trait Value extends SmartHash
  case class Str(s: S) extends Value {
    override def toString: String = StringLattice[S].show(s)
  }
  case class Bool(b: B) extends Value {
    override def toString: String = BoolLattice[B].show(b)
  }
  case class Int(i: I) extends Value {
    override def toString: String = IntLattice[I].show(i)
  }
  case class Real(r: R) extends Value {
    override def toString: String = RealLattice[R].show(r)
  }
  case class Char(c: C) extends Value {
    override def toString: String = CharLattice[C].show(c)
  }
  case class Symbol(s: Sym) extends Value {
    override def toString: String = SymbolLattice[Sym].show(s)
  }
  case class Prim(prim: P) extends Value {
    override def toString = s"#<primitive ${prim.name}>"
  }
  case class Clo(lambda: SchemeLambdaExp, env: Env, name: Option[String]) extends Value {
    def printName: String = name match {
      case None => s"anonymous@${lambda.idn}"
      case Some(name) => name
    }
    override def toString: String = s"#<closure $printName ($env)>"
  }
  case class Cons(car: L, cdr: L) extends Value {
    override def toString: String = s"($car . $cdr)"
  }
  case object Nil extends Value {
    override def toString: String = "()"
  }
  case class Pointer(a: A) extends Value {
    override def toString: String = s"#<pointer $a>"
  }
  case class Vec(size: I, elements: Map[I, L]) extends Value {
    override def toString: String = {
      val els = elements.toList
        .map({
          case (k, v) => s"$k: $v"
        })
        .mkString(", ")
      s"Vec(size: $size, elems: {$els})"
    }
  }
  /** The injected true value */
  val True = Bool(BoolLattice[B].inject(true))
  /** The injected false value */
  val False = Bool(BoolLattice[B].inject(false))

  object Value {

    /** Check if two values are compatible to be joined */
    def compatible(x: Value, y: => Value): Boolean = (x, y) match {
      case (_: Str,     _: Str)     => true
      case (_: Bool,    _: Bool)    => true
      case (_: Int,     _: Int)     => true
      case (_: Real,    _: Real)    => true
      case (_: Char,    _: Char)    => true
      case (_: Symbol,  _: Symbol)  => true
      case (_: Cons,    _: Cons)    => true
      case (_: Vec,     _: Vec)     => true
      case _                        => false
    }

    /** Tries to join. Returns a Right element if it can, otherwise a Left. */
    def join(x: Value, y: => Value): Option[Value] =
      if (x == y) {
        Some(x)
      } else {
        (x, y) match {
          case (Str(s1), Str(s2))   => Some(Str(StringLattice[S].join(s1, s2)))
          case (Bool(b1), Bool(b2)) => Some(Bool(BoolLattice[B].join(b1, b2)))
          case (Int(i1), Int(i2))   => Some(Int(IntLattice[I].join(i1, i2)))
          case (Real(f1), Real(f2)) => Some(Real(RealLattice[R].join(f1, f2)))
          case (Char(c1), Char(c2)) => Some(Char(CharLattice[C].join(c1, c2)))
          case (Symbol(s1), Symbol(s2)) => Some(Symbol(SymbolLattice[Sym].join(s1,s2)))
          case (Cons(a1,d1), Cons(a2,d2)) => Some(Cons(schemeLattice.join(a1,a2), schemeLattice.join(d1,d2)))
          case (Vec(size1, els1), Vec(size2, els2)) =>
            // First, joins the size
            val vSizeInitJoined = Vec(IntLattice[I].join(size1, size2), Map.empty)
            // Then, joins elements by adding (with vector-set!) all elements of els1 and then els2 inside the new vector
            val vWithEls1Joined = els1.foldLeft(vSizeInitJoined)({ case (acc, (k, v)) => vectorSet(acc, Int(k), v).getOrElse(schemeLattice.bottom) match {
              case Elements(vs) => vs.head.asInstanceOf[Vec] // Should really be improved, this is ugly
            }})
            val vWithEls2Joined = els2.foldLeft(vWithEls1Joined)({ case (acc, (k, v)) => vectorSet(acc, Int(k), v).getOrElse(schemeLattice.bottom) match {
              case Elements(vs) => vs.head.asInstanceOf[Vec] // Should really be improved, this is ugly
            }})
            Some(vWithEls2Joined)
          case _ => None
        }
      }
    def subsumes(x: Value, y: => Value): Boolean =
      if (x == y) {
        true
      } else {
        (x, y) match {
          case (Str(s1), Str(s2))   => StringLattice[S].subsumes(s1, s2)
          case (Bool(b1), Bool(b2)) => BoolLattice[B].subsumes(b1, b2)
          case (Int(i1), Int(i2))   => IntLattice[I].subsumes(i1, i2)
          case (Real(f1), Real(f2)) => RealLattice[R].subsumes(f1, f2)
          case (Char(c1), Char(c2)) => CharLattice[C].subsumes(c1, c2)
          case (Symbol(s1), Symbol(s2)) => SymbolLattice[Sym].subsumes(s1,s2)
          case (Cons(a1,d1), Cons(a2,d2)) => schemeLattice.subsumes(a1,a2) && schemeLattice.subsumes(d1,d2)
          case (Vec(siz1,els1), Vec(siz2,els2)) =>
            IntLattice[I].subsumes(siz1, siz2) &&
            els2.forall { case (idx2, vlu2) =>
              els1.exists { case (idx1, vlu1) => 
                IntLattice[I].subsumes(idx1, idx2) && schemeLattice.subsumes(vlu1, vlu2)
              }
            }
          case _                    => false
        }
      }

    def isTrue(x: Value): Boolean = x match {
      case Bool(b) => BoolLattice[B].isTrue(b)
      case _       => true
    }
    def isFalse(x: Value): Boolean = x match {
      case Bool(b) => BoolLattice[B].isFalse(b)
      case _       => false
    }
    def unaryOp(op: UnaryOperator)(x: Value): MayFail[Value, Error] = op match {
      case IsNull => MayFail.success(x match {
        case Nil => True
        case _   => False
      })
      case IsCons => MayFail.success(x match {
        case _: Cons => True
        case _       => False
      })
      case IsPointer => MayFail.success(x match {
        case _: Pointer => True
        case _          => False
      })
      case IsChar => MayFail.success(x match {
        case _: Char => True
        case _       => False
      })
      case IsSymbol => MayFail.success(x match {
        case _: Symbol => True
        case _         => False
      })
      case IsString => MayFail.success(x match {
        case _: Str => True
        case _      => False
      })
      case IsInteger => MayFail.success(x match {
        case _: Int => True
        case _      => False
      })
      case IsReal => MayFail.success(x match {
        case _: Real => True
        case _       => False
      })
      case IsBoolean => MayFail.success(x match {
        case _: Bool => True
        case _       => False
      })
      case IsVector => MayFail.success(x match {
        case _: Vec => True
        case _      => False
      })
      case Not => MayFail.success(x match {
        case Bool(b) => Bool(BoolLattice[B].not(b))
        case _       => False /* any value is true */
      })
      case Ceiling => x match {
        case Int(n)  => MayFail.success(Int(n))
        case Real(n) => MayFail.success(Real(RealLattice[R].ceiling(n)))
        case _       => MayFail.failure(OperatorNotApplicable("ceiling", List(x)))
      }
      case Floor => x match {
        case Int(n)  => MayFail.success(Int(n))
        case Real(n) => MayFail.success(Real(RealLattice[R].floor(n)))
        case _       => MayFail.failure(OperatorNotApplicable("floor", List(x)))
      }
      case Round => x match {
        case Int(n)  => MayFail.success(Int(n))
        case Real(n) => MayFail.success(Real(RealLattice[R].round(n)))
        case _       => MayFail.failure(OperatorNotApplicable("round", List(x)))
      }
      case Log => x match {
        case Int(n)  => MayFail.success(Real(RealLattice[R].log(IntLattice[I].toReal(n))))
        case Real(n) => MayFail.success(Real(RealLattice[R].log(n)))
        case _       => MayFail.failure(OperatorNotApplicable("log", List(x)))
      }
      case Random => x match {
        case Int(n)  => MayFail.success(Int(IntLattice[I].random(n)))
        case Real(n) => MayFail.success(Real(RealLattice[R].random(n)))
        case _       => MayFail.failure(OperatorNotApplicable("random", List(x)))
      }
      case Sin => x match {
        case Int(n)  => MayFail.success(Real(RealLattice[R].sin(IntLattice[I].toReal(n))))
        case Real(n) => MayFail.success(Real(RealLattice[R].sin(n)))
        case _       => MayFail.failure(OperatorNotApplicable("sin", List(x)))
      }
      case ASin => x match {
        case Int(n)  => MayFail.success(Real(RealLattice[R].asin(IntLattice[I].toReal(n))))
        case Real(n) => MayFail.success(Real(RealLattice[R].asin(n)))
        case _       => MayFail.failure(OperatorNotApplicable("asin", List(x)))
      }
      case Cos => x match {
        case Int(n)  => MayFail.success(Real(RealLattice[R].cos(IntLattice[I].toReal(n))))
        case Real(n) => MayFail.success(Real(RealLattice[R].cos(n)))
        case _       => MayFail.failure(OperatorNotApplicable("cos", List(x)))
      }
      case ACos => x match {
        case Int(n)  => MayFail.success(Real(RealLattice[R].acos(IntLattice[I].toReal(n))))
        case Real(n) => MayFail.success(Real(RealLattice[R].acos(n)))
        case _       => MayFail.failure(OperatorNotApplicable("acos", List(x)))
      }
      case Tan => x match {
        case Int(n)  => MayFail.success(Real(RealLattice[R].tan(IntLattice[I].toReal(n))))
        case Real(n) => MayFail.success(Real(RealLattice[R].tan(n)))
        case _       => MayFail.failure(OperatorNotApplicable("tan", List(x)))
      }
      case ATan => x match {
        case Int(n)  => MayFail.success(Real(RealLattice[R].atan(IntLattice[I].toReal(n))))
        case Real(n) => MayFail.success(Real(RealLattice[R].atan(n)))
        case _       => MayFail.failure(OperatorNotApplicable("atan", List(x)))
      }
      case Sqrt => x match {
        case Int(n)  => MayFail.success(Real(RealLattice[R].sqrt(IntLattice[I].toReal(n))))
        case Real(n) => MayFail.success(Real(RealLattice[R].sqrt(n)))
        case _       => MayFail.failure(OperatorNotApplicable("sqrt", List(x)))
      }
      case VectorLength => x match {
        case Vec(size, _) => MayFail.success(Int(size))
        case _            => MayFail.failure(OperatorNotApplicable("vector-length", List(x)))
      }
      case StringLength => x match {
        case Str(s) => MayFail.success(Int(StringLattice[S].length(s)))
        case _      => MayFail.failure(OperatorNotApplicable("string-length", List(x)))
      }
      case NumberToString => x match {
        case Int(n)  => MayFail.success(Str(IntLattice[I].toString(n)))
        case Real(n) => MayFail.success(Str(RealLattice[R].toString(n)))
        case _       => MayFail.failure(OperatorNotApplicable("number->string", List(x)))
      }
      case SymbolToString => x match {
        case Symbol(s) => MayFail.success(Str(SymbolLattice[Sym].toString(s)))
        case _         => MayFail.failure(OperatorNotApplicable("symbol->string", List(x)))
      }
      case StringToSymbol => x match {
        case Str(s) => MayFail.success(Symbol(StringLattice[S].toSymbol(s)))
        case _      => MayFail.failure(OperatorNotApplicable("string->symbol", List(x)))
      }
      case ExactToInexact => x match {
        case Int(n)  => MayFail.success(Real(IntLattice[I].toReal(n)))
        case Real(n) => MayFail.success(Real(n))
        case _       => MayFail.failure(OperatorNotApplicable("exact->inexact", List(x)))
      }
      case InexactToExact => x match {
        case Int(n)  => MayFail.success(Int(n))
        case Real(n) => MayFail.success(Int(RealLattice[R].toInt[I](n))) /* should introduce fractions */
        case _       => MayFail.failure(OperatorNotApplicable("inexact->exact", List(x)))
      }
      case CharacterToInteger => x match {
        case Char(c) => MayFail.success(Int(CharLattice[C].toInt[I](c)))
        case _       => MayFail.failure(OperatorNotApplicable("char->integer", List(x)))
      }
  }
    def binaryOp(op: BinaryOperator)(x: Value, y: Value): MayFail[Value, Error] = op match {
      case Plus => (x, y) match {
        case (Int(n1), Int(n2))   => MayFail.success(Int(IntLattice[I].plus(n1, n2)))
        case (Int(n1), Real(n2))  => MayFail.success(Real(RealLattice[R].plus(IntLattice[I].toReal(n1), n2)))
        case (Real(n1), Int(n2))  => MayFail.success(Real(RealLattice[R].plus(n1, IntLattice[I].toReal(n2))))
        case (Real(n1), Real(n2)) => MayFail.success(Real(RealLattice[R].plus(n1, n2)))
        case _                    => MayFail.failure(OperatorNotApplicable("+", List(x, y)))
      }
      case Minus => (x, y) match {
        case (Int(n1), Int(n2))   => MayFail.success(Int(IntLattice[I].minus(n1, n2)))
        case (Int(n1), Real(n2))  => MayFail.success(Real(RealLattice[R].minus(IntLattice[I].toReal(n1), n2)))
        case (Real(n1), Int(n2))  => MayFail.success(Real(RealLattice[R].minus(n1, IntLattice[I].toReal(n2))))
        case (Real(n1), Real(n2)) => MayFail.success(Real(RealLattice[R].minus(n1, n2)))
        case _                    => MayFail.failure(OperatorNotApplicable("-", List(x, y)))
      }
      case Times => (x, y) match {
        case (Int(n1), Int(n2))   => MayFail.success(Int(IntLattice[I].times(n1, n2)))
        case (Int(n1), Real(n2))  => MayFail.success(Real(RealLattice[R].times(IntLattice[I].toReal(n1), n2)))
        case (Real(n1), Int(n2))  => MayFail.success(Real(RealLattice[R].times(n1, IntLattice[I].toReal(n2))))
        case (Real(n1), Real(n2)) => MayFail.success(Real(RealLattice[R].times(n1, n2)))
        case _                    => MayFail.failure(OperatorNotApplicable("*", List(x, y)))
      }
      case Quotient => (x, y) match {
        case (Int(n1), Int(n2)) => try {
          MayFail.success(Int(IntLattice[I].quotient(n1, n2)))
        } catch {
          case _: ArithmeticException =>
            MayFail.failure(OperatorNotApplicable("quotient", List(x, y)))
        }
        case _                  => MayFail.failure(OperatorNotApplicable("quotient", List(x, y)))
      }
      case Div => (x, y) match {
        case (Int(n1), Int(n2))   => MayFail.success(Real(IntLattice[I].div[R](n1, n2)))
        case (Int(n1), Real(n2))  => MayFail.success(Real(RealLattice[R].div(IntLattice[I].toReal(n1), n2)))
        case (Real(n1), Int(n2))  => MayFail.success(Real(RealLattice[R].div(n1, IntLattice[I].toReal(n2))))
        case (Real(n1), Real(n2)) => MayFail.success(Real(RealLattice[R].div(n1, n2)))
        case _                    => MayFail.failure(OperatorNotApplicable("/", List(x, y)))
      }
      case Expt => (x, y) match {
        case (Int(n1), Int(n2))   => MayFail.success(Int(IntLattice[I].expt(n1, n2)))
        case (Int(n1), Real(n2))  => MayFail.success(Real(RealLattice[R].expt(IntLattice[I].toReal(n1), n2)))
        case (Real(n1), Int(n2))  => MayFail.success(Real(RealLattice[R].expt(n1, IntLattice[I].toReal(n2))))
        case (Real(n1), Real(n2)) => MayFail.success(Real(RealLattice[R].expt(n1, n2)))
        case _                    => MayFail.failure(OperatorNotApplicable("expt", List(x, y)))
      }
      case Modulo => (x, y) match {
        case (Int(n1), Int(n2)) => MayFail.success(Int(IntLattice[I].modulo(n1, n2)))
        case _                  => MayFail.failure(OperatorNotApplicable("modulo", List(x, y)))
      }
      case Remainder => (x, y) match {
        case (Int(n1), Int(n2)) => MayFail.success(Int(IntLattice[I].remainder(n1, n2)))
        case _                  => MayFail.failure(OperatorNotApplicable("modulo", List(x, y)))
      }
      case Lt => (x, y) match {
        case (Int(n1), Int(n2))   => MayFail.success(Bool(IntLattice[I].lt(n1, n2)))
        case (Int(n1), Real(n2))  => MayFail.success(Bool(RealLattice[R].lt(IntLattice[I].toReal(n1), n2)))
        case (Real(n1), Int(n2))  => MayFail.success(Bool(RealLattice[R].lt(n1, IntLattice[I].toReal(n2))))
        case (Real(n1), Real(n2)) => MayFail.success(Bool(RealLattice[R].lt(n1, n2)))
        case _                    => MayFail.failure(OperatorNotApplicable("<", List(x, y)))
      }
      case NumEq => (x, y) match {
        case (Int(n1), Int(n2))   => MayFail.success(Bool(IntLattice[I].eql(n1, n2)))
        case (Int(n1), Real(n2))  => MayFail.success(Bool(RealLattice[R].eql(IntLattice[I].toReal(n1), n2)))
        case (Real(n1), Int(n2))  => MayFail.success(Bool(RealLattice[R].eql(n1, IntLattice[I].toReal(n2))))
        case (Real(n1), Real(n2)) => MayFail.success(Bool(RealLattice[R].eql(n1, n2)))
        case _                    => MayFail.failure(OperatorNotApplicable("number=", List(x, y)))
      }
      case Eq => MayFail.success((x, y) match {
        case (Str(s1), Str(s2))       => Bool(StringLattice[S].eql(s1, s2)) /* TODO: this isn't really physical equality for strings */
        case (Bool(b1), Bool(b2))     => Bool(BoolLattice[B].eql(b1, b2))
        case (Int(n1), Int(n2))       => Bool(IntLattice[I].eql(n1, n2))
        case (Real(n1), Real(n2))     => Bool(RealLattice[R].eql(n1, n2))
        case (Char(c1), Char(c2))     => Bool(CharLattice[C].eql(c1, c2))
        case (Symbol(s1), Symbol(s2)) => Bool(SymbolLattice[Sym].eql(s1, s2))
        case (Nil, Nil)               => True
        case (Prim(_), Prim(_))       => Bool(BoolLattice[B].inject(x == y))
        case (_: Clo, _: Clo)         => Bool(BoolLattice[B].inject(x == y))
        case (_: Cons, _: Cons)       => Bool(BoolLattice[B].inject(x == y))
        case (_: Vec, _: Vec)         => Bool(BoolLattice[B].inject(x == y))
        case (_: Pointer, _: Pointer) => Bool(BoolLattice[B].top) // We can't know for sure that equal addresses are eq (in the abstract). This implementation is not suited for use in a concrete machine!
        case _                        => False
      })
      case StringAppend => (x, y) match {
        case (Str(s1), Str(s2)) => MayFail.success(Str(StringLattice[S].append(s1, s2)))
        case _                  => MayFail.failure(OperatorNotApplicable("string-append", List(x, y)))
      }
      case StringRef => (x, y) match {
        case (Str(s), Int(n)) => MayFail.success(Char(StringLattice[S].ref(s, n)))
        case _                => MayFail.failure(OperatorNotApplicable("string-ref", List(x, y)))
      }
      case StringLt => (x, y) match {
        case (Str(s1), Str(s2)) => MayFail.success(Bool(StringLattice[S].lt(s1, s2)))
        case _                  => MayFail.failure(OperatorNotApplicable("string<?", List(x, y)))
      }
    }

    def number(x: scala.Int): Value               = Int(IntLattice[I].inject(x))
    def real(x: Double): Value                    = Real(RealLattice[R].inject(x))
    def string(x: String): Value                  = Str(StringLattice[S].inject(x))
    def bool(x: Boolean): Value                   = Bool(BoolLattice[B].inject(x))
    def char(x: scala.Char): Value                = Char(CharLattice[C].inject(x))
    def primitive(x: P): Value                    = Prim(x)
    def closure(x: schemeLattice.Closure, name: Option[String]): Value  = Clo(x._1,x._2,name)
    def symbol(x: String): Value                  = Symbol(SymbolLattice[Sym].inject(x))
    def nil: Value                                = Nil
    def cons(car: L, cdr: L): Value               = Cons(car, cdr)
    def pointer(a: A): Value                      = Pointer(a)

    def getClosures(x: Value): Set[(schemeLattice.Closure,Option[String])] = x match {
      case Clo(lam, env, name) => Set(((lam, env),name))
      case _                   => Set()
    }
    def getPrimitives(x: Value): Set[P] = x match {
      case Prim(p) => Set(p)
      case _       => Set()
    }
    def getPointerAddresses(x: Value): Set[A] = x match {
      case Pointer(a) => Set(a)
      case _          => Set()
    }
    def car(x: Value): MayFail[L, Error] = x match {
      case Cons(car, _) => MayFail.success(car)
      case _            => MayFail.failure(TypeError("expecting cons to access car", x))
    }
    def cdr(x: Value): MayFail[L, Error] = x match {
      case Cons(_, cdr) => MayFail.success(cdr)
      case _            => MayFail.failure(TypeError("expecting cons to access cdr", x))
    }
    // This implementation is not suited for use in a concrete machine!
    def vectorRef(vector: Value, index: Value): MayFail[L, Error] = (vector, index) match {
      case (Vec(size, content), Int(index)) =>
        val comp = IntLattice[I].lt(index, size)
        val t: L = if (BoolLattice[B].isTrue(comp)) {
          val vals = content.view.filterKeys(index2 => BoolLattice[B].isTrue(IntLattice[I].eql(index, index2))).values
          if (vals.isEmpty) {
            schemeLattice.bottom
          } else {
            schemeLattice.join(vals)
          }
        } else {
          schemeLattice.bottom
        }
        /* Don't perform bound checks here because we would get too many spurious flows */
        val f: L = schemeLattice.bottom
        MayFail.success(schemeLattice.join(t, f))
      case (_: Vec, _) => MayFail.failure(TypeError("expecting int to access vector", index))
      case _           => MayFail.failure(TypeError("vector-ref: expecting vector", vector))
    }

    // This implementation is not suited for use in a concrete machine!
    def vectorSet(vector: Value, index: Value, newval: L): MayFail[L, Error] =
      (vector, index) match {
        case (Vec(size, content), Int(index)) =>
            val comp = IntLattice[I].lt(index, size)
            val t: L = if (BoolLattice[B].isTrue(comp)) {
              content.find({ case (k, _) => IntLattice[I].subsumes(k, index) }) match {
                case Some((index2, _)) =>
                  // Case 1: there is an `index2` that already subsumes `index`
                  // Then we just update the value for `index2`
                  Element(
                    Vec(
                      size,
                      content + (index2 -> schemeLattice.join(content(index2), newval))
                    )
                  )
                case None =>
                  val subsumedKeys = content.keySet.filter(k => IntLattice[I].subsumes(index, k))
                  if (subsumedKeys.nonEmpty) {
                    // Case 2: this index subsumes other indices
                    // In that case, we join all values and removed the subsumed indices
                    val joinedValues = schemeLattice.join(content.view.filterKeys(subsumedKeys).toMap.values)
                    val contentWithoutSubsumedKeys = subsumedKeys.foldLeft(content)((acc, k) => acc - k)
                    Element(Vec(size, contentWithoutSubsumedKeys + (index -> schemeLattice.join(joinedValues, newval))))
                  } else {
                    // Case 3: there is nothing in `content` that we can update, so we add a new key
                    Element(Vec(size, content + (index -> newval)))
                  }
              }
            } else {
              schemeLattice.bottom
            }
            // We ignore out-of-bounds accesses, mostly because most of them will be spurious.
            // For example, vector-set! called with Int as first argument would result in
            // a possible out-of-bound access
            val f: L = schemeLattice.bottom
            MayFail.success(schemeLattice.join(t, f))
        case (_: Vec, _) => MayFail.failure(TypeError("expecting int to set vector", index))
        case _           => MayFail.failure(TypeError("vector-set!: expecting vector", vector))
      }

    def vector(size: Value, init: L): MayFail[Value, Error] = size match {
      case Int(size) => MayFail.success(if (init == IntLattice[I].bottom) {
        Vec(size, Map[I, L]())
      } else {
        // Field-sensitive vectors:
        // Vec(size, Map.from[I, L](IntLattice[I].valuesBetween(IntLattice[I].inject(0), size).map(idx => idx -> init).toList))
        // Field-insensitive vectors:
        Vec(size, Map[I, L](IntLattice[I].top -> init))
      })
      case _         => MayFail.failure(TypeError("expected int size when constructing vector", size))
    }

    def split(v: Value): Set[Value] = v match {
      case Bool(b)    => Lattice[B].split(b).map(Bool)
      case Int(i)     => Lattice[I].split(i).map(Int)
      case Char(c)    => Lattice[C].split(c).map(Char)
      case Str(s)     => Lattice[S].split(s).map(Str)
      case Real(r)    => Lattice[R].split(r).map(Real)
      case Symbol(s)  => Lattice[Sym].split(s).map(Symbol)
      case _          => Set(v)
    }
  }

  type L = Elements
  case class Elements(vs: Set[Value]) extends SmartHash {
    override def toString: String = 
      if (vs.isEmpty) {
        "⊥"
      } else if (vs.size == 1) {
        vs.head.toString
      } else {
        vs.map(_.toString).toList.sorted.mkString("{",",","}")
      }
    def foldMapL[X](f: Value => X)(implicit monoid: Monoid[X]): X =
      vs.foldLeft(monoid.zero)((acc, x) => monoid.append(acc, f(x)))
  }
  object Element {
    def apply(v: Value): L = Elements(Set(v))
  }

  import MonoidInstances.{boolOrMonoid, boolAndMonoid, setMonoid}
  implicit val lMonoid: Monoid[L] = new Monoid[L] {
    def append(x: L, y: => L): L = (x,y) match {
      // TODO: this can probably be done more efficiently?
      case (Elements(as), Elements(bs)) =>
        /* every element in the other set has to be joined in this set */
        Elements(as.foldLeft(bs)((acc, x2) =>
          if (acc.exists(x1 => Value.subsumes(x1, x2))) {
            /* the set already contains an element that subsumes x2, don't add it to the set */
            acc
          } else if (acc.exists(x1 => Value.compatible(x1, x2))) {
            /* merge x2 into another element of the set */
            acc.map(x1 => Value.join(x1, x2) match {
                case None         => x1
                case Some(joined) => joined
            })
          } else {
            /* just add x2 to the set */
            acc + x2
          }))
    }
    def zero: L = Elements(Set.empty)
  }
  implicit val lMFMonoid: Monoid[MayFail[L, Error]] = MonoidInstances.mayFail[L]

  val schemeLattice: SchemeLattice[L, A, P, Env] = new SchemeLattice[L, A, P, Env] {
    def    show(x: L):  String = x.toString /* TODO[easy]: implement better */
    def  isTrue(x: L): Boolean = x.foldMapL(Value.isTrue(_))(boolOrMonoid)
    def isFalse(x: L): Boolean = x.foldMapL(Value.isFalse(_))(boolOrMonoid)
    def unaryOp(op: UnaryOperator)(x: L): MayFail[L, Error] =
      x.foldMapL(x => Value.unaryOp(op)(x).map(x => Element(x)))
    def binaryOp(op: BinaryOperator)(x: L, y: L): MayFail[L, Error] =
      x.foldMapL(x => y.foldMapL(y => Value.binaryOp(op)(x, y).map(x => Element(x))))
    def join(x: L, y: => L): L = Monoid[L].append(x, y)
    def subsumes(x: L, y: => L): Boolean =
      y.foldMapL(
        y =>
          /* For every element in y, there exists an element of x that subsumes it */
          x.foldMapL(x => Value.subsumes(x, y))(boolOrMonoid)
      )(boolAndMonoid)
    def top: L    = throw LatticeTopUndefined

    def vectorRef(vector: L, index: L): MayFail[L, Error] =
      vector.foldMapL(vec => index.foldMapL(i => Value.vectorRef(vec, i)))
    def vectorSet(vector: L, index: L, newval: L): MayFail[L, Error] =
      vector.foldMapL(vec => index.foldMapL(i => Value.vectorSet(vec, i, newval)))

    def getClosures(x: L): Set[(Closure,Option[String])] = x.foldMapL(x => Value.getClosures(x))(setMonoid)
    def getPrimitives(x: L): Set[P] =
      x.foldMapL(x => Value.getPrimitives(x))(setMonoid)
    def getPointerAddresses(x: L): Set[A] = x.foldMapL(x => Value.getPointerAddresses(x))(setMonoid)

    def bottom: L                             = Elements(Set.empty)
    def number(x: scala.Int): L               = Element(Value.number(x))
    def numTop: L                             = Element(Int(IntLattice[I].top))
    def real(x: Double): L                    = Element(Value.real(x))
    def string(x: String): L                  = Element(Value.string(x))
    def char(x: scala.Char): L                = Element(Value.char(x))
    def bool(x: Boolean): L                   = Element(Value.bool(x))
    def primitive(x: P): L                    = Element(Value.primitive(x))
    def closure(x: Closure,
                name: Option[String]): L      = Element(Value.closure(x,name))
    def symbol(x: String): L                  = Element(Value.symbol(x))
    def cons(car: L, cdr: L): L               = Element(Value.cons(car, cdr))
    def car(x: L): MayFail[L, Error]          = x.foldMapL(v => Value.car(v))
    def cdr(x: L): MayFail[L, Error]          = x.foldMapL(v => Value.cdr(v))
    def pointer(a: A): L                      = Element(Value.pointer(a))
    def vector(size: L, init: L): MayFail[L, Error] = size.foldMapL(sz => Value.vector(sz, init).map(v => Element(v)))
    def nil: L = Element(Value.nil)
    def eql[B2: BoolLattice](x: L, y: L): B2 = ??? // TODO[medium] implement
    def split(abs: L): Set[L] = abs.foldMapL(v => Value.split(v).map(va => Element(va)))(setMonoid)
  }

  object L {
    implicit val lattice: SchemeLattice[L, A, P, Env] = schemeLattice
  }
}
