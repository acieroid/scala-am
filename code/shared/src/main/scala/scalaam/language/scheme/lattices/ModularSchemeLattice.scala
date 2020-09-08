package scalaam.language.scheme.lattices

import scalaam.core._
import scalaam.language.CScheme.TID
import SchemeOps.BinaryOperator.{CharacterEq, CharacterEqCI, CharacterLt, CharacterLtCI, Div, Eq, Expt, Lt, Minus, Modulo, NumEq, Plus, Quotient, Remainder, StringAppend, StringLt, StringRef, Times}
import SchemeOps.UnaryOperator.{ACos, ASin, ATan, Ceiling, CharacterDowncase, CharacterIsLower, CharacterIsUpper, CharacterToInteger, CharacterToString, CharacterUpcase, Cos, ExactToInexact, Floor, InexactToExact, IntegerToCharacter, IsBoolean, IsChar, IsCons, IsInteger, IsLock, IsNull, IsPointer, IsProcedure, IsReal, IsString, IsSymbol, IsThread, IsVector, Log, Not, NumberToString, Random, Round, Sin, Sqrt, StringLength, StringToNumber, StringToSymbol, SymbolToString, Tan, VectorLength}
import SchemeOps._
import scalaam.language.scheme.primitives.SchemePrimitive
import scalaam.lattice._
import scalaam.util.SmartUnion._
import scalaam.util._

/** Defines a Scheme lattice based on other lattices.
  * Example usage:
  *    val address = NameAddress
  *    val lattice = new ModularSchemeLattice[SchemeExp, address.A, Type.S, Type.B, Type.I, Type.R, Type.C, Type.Sym]
  * Now `lattice.L` is a SchemeLattice, of which the implicit for the typeclass is available in the current scope.
  */
/** TODO[medium]: use Show and ShowStore here */
class ModularSchemeLattice[
    A <: Address,
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
  sealed trait Value extends SmartHash {
    def ord: scala.Int
  }
  case class Str(s: S) extends Value {
    def ord = 0
    override def toString: String = StringLattice[S].show(s)
  }
  case class Bool(b: B) extends Value {
    def ord = 1
    override def toString: String = BoolLattice[B].show(b)
  }
  case class Int(i: I) extends Value {
    def ord = 2
    override def toString: String = IntLattice[I].show(i)
  }
  case class Real(r: R) extends Value {
    def ord = 3
    override def toString: String = RealLattice[R].show(r)
  }
  case class Char(c: C) extends Value {
    def ord = 4
    override def toString: String = CharLattice[C].show(c)
  }
  case class Symbol(s: Sym) extends Value {
    def ord = 5
    override def toString: String = SymbolLattice[Sym].show(s)
  }
  case class Prim(prim: Set[P]) extends Value {
    def ord = 6
    override def toString = prim.map(_.name).mkString("{primitives ",",","}")
  }
  // TODO: define `type Closure = (SchemeLambdaExp, Env, Option[String])` (maybe using a case class)
  case class Clo(closures: Set[(schemeLattice.Closure, Option[String])]) extends Value {
    def ord = 7
    override def toString: String =
      closures.map(namedClo => namedClo._2.getOrElse(s"λ@${namedClo._1._1.idn}"))
              .mkString("{closures ",",","}")
  }
  case object Nil extends Value {
    def ord = 8
    override def toString: String = "()"
  }
  case class Pointer(ptrs: Set[A]) extends Value {
    def ord = 9
    override def toString: String = ptrs.mkString("{pointers ",",","}")
  }
  case class Cons(car: L, cdr: L) extends Value {
    def ord = 10
    override def toString: String = s"($car . $cdr)"
  }
  case class Vec(size: I, elements: Map[I, L]) extends Value {
    def ord = 11
    override def toString: String = {
      val els = elements.toList
        .map({
          case (k, v) => s"$k: $v"
        })
        .mkString(", ")
      s"Vec(size: $size, elems: {$els})"
    }
  }
  case class Thread(threads: Set[TID]) extends Value {
    def ord = 12
    override def toString: String = s"🧵$threads"
  }
  // Could also store (a) Thread(s) here, but this seems to be simpler.
  // An empty set indicates the lock is not held, but a non-empty set may also indicate this... (due to the monotonicity of the analysis, threads will only increase in size).
  // This should correspond to the formalisation of ModConc and \lambda_\tau.
  case class Lock(threads: Set[TID]) extends Value {
    def ord = 13
    override def toString: String = s"<Lock $threads>"
  }
  case object Void extends Value {
    def ord = 14

    override def toString: String = "<void>"
  }
  /** The injected true value */
  val True = Bool(BoolLattice[B].inject(true))
  /** The injected false value */
  val False = Bool(BoolLattice[B].inject(false))

  object Value {

    /** Tries to join (throws an exception for incompatible types) */
    def join(x: Value, y: => Value): Value = (x, y) match {
      case (Nil, Nil)                 => Nil
      case (Str(s1), Str(s2))         => Str(StringLattice[S].join(s1, s2))
      case (Bool(b1), Bool(b2))       => Bool(BoolLattice[B].join(b1, b2))
      case (Int(i1), Int(i2))         => Int(IntLattice[I].join(i1, i2))
      case (Real(f1), Real(f2))       => Real(RealLattice[R].join(f1, f2))
      case (Char(c1), Char(c2))       => Char(CharLattice[C].join(c1, c2))
      case (Symbol(s1), Symbol(s2))   => Symbol(SymbolLattice[Sym].join(s1,s2))
      case (Prim(p1), Prim(p2))       => Prim(sunion(p1,p2))
      case (Clo(c1), Clo(c2))         => Clo(sunion(c1,c2))
      case (Pointer(a1), Pointer(a2)) => Pointer(sunion(a1,a2))
      case (Cons(a1,d1), Cons(a2,d2)) => Cons(schemeLattice.join(a1,a2), schemeLattice.join(d1,d2))
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
        vWithEls2Joined
      case (Thread(t1), Thread(t2))   => Thread(sunion(t1, t2))
      case (Lock(l1), Lock(l2))       => Lock(sunion(l1, l2))
      case (Void, Void)               => Void
      case _ => throw new Exception(s"Illegal join of $x and $y")
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
          case (Clo(c1), Clo(c2))         => c2.subsetOf(c1)
          case (Prim(p1), Prim(p2))       => p2.subsetOf(p1)
          case (Pointer(a1), Pointer(a2)) => a2.subsetOf(a1)
          case (Cons(a1,d1), Cons(a2,d2)) => schemeLattice.subsumes(a1,a2) && schemeLattice.subsumes(d1,d2)
          case (Vec(siz1,els1), Vec(siz2,els2)) =>
            IntLattice[I].subsumes(siz1, siz2) &&
            els2.forall { case (idx2, vlu2) =>
              els1.exists { case (idx1, vlu1) => 
                IntLattice[I].subsumes(idx1, idx2) && schemeLattice.subsumes(vlu1, vlu2)
              }
            }
          case (Thread(t1), Thread(t2))  => t2.subsetOf(t1)
          case (Lock(l1), Lock(l2))      => l2.subsetOf(l1)
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
      case IsThread => MayFail.success(x match {
        case _: Thread => True
        case _         => False
      })
      case IsLock => MayFail.success(x match {
        case _: Lock => True
        case _       => False
      })
      case IsProcedure => MayFail.success(x match {
        case _: Clo => True
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
      case StringToNumber => x match {
        // TODO: string may also be a float!
        case Str(s) => StringLattice[S].toNumber(s).map(Int)
        case _      => MayFail.failure(OperatorNotApplicable("string->number", List(x)))
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
      case CharacterToString => x match {
        case Char(c)  => MayFail.success(Str(CharLattice[C].toString(c)))
        case _        => MayFail.failure(OperatorNotApplicable("char->string", List(x)))
      }
      case CharacterDowncase => x match {
        case Char(c)  => MayFail.success(Char(CharLattice[C].downCase(c)))
        case _        => MayFail.failure(OperatorNotApplicable("char-downcase", List(x)))
      }
      case CharacterUpcase => x match {
        case Char(c)  => MayFail.success(Char(CharLattice[C].upCase(c)))
        case _        => MayFail.failure(OperatorNotApplicable("char-upcase", List(x)))
      }
      case CharacterIsLower => x match {
        case Char(c)  => MayFail.success(Bool(CharLattice[C].isLower(c)))
        case _        => MayFail.failure(OperatorNotApplicable("char-lower-case?", List(x)))
      }
      case CharacterIsUpper => x match {
        case Char(c)  => MayFail.success(Bool(CharLattice[C].isUpper(c)))
        case _        => MayFail.failure(OperatorNotApplicable("char-upper-case?", List(x)))
      }
      case IntegerToCharacter => x match {
        case Int(i)   => MayFail.success(Char(IntLattice[I].toChar(i)))
        case _        => MayFail.failure(OperatorNotApplicable("integer->char", List(x)))
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
        case _                  => MayFail.failure(OperatorNotApplicable("remainder", List(x, y)))
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
      // TODO: this should be the eql method instead of a binary op?
      case Eq => MayFail.success((x, y) match {
        case (Str(s1), Str(s2))         => Bool(StringLattice[S].eql(s1, s2)) /* TODO: this isn't really physical equality for strings */
        case (Bool(b1), Bool(b2))       => Bool(BoolLattice[B].eql(b1, b2))
        case (Int(n1), Int(n2))         => Bool(IntLattice[I].eql(n1, n2))
        case (Real(n1), Real(n2))       => Bool(RealLattice[R].eql(n1, n2))
        case (Char(c1), Char(c2))       => Bool(CharLattice[C].eql(c1, c2))
        case (Symbol(s1), Symbol(s2))   => Bool(SymbolLattice[Sym].eql(s1, s2))
        case (Nil, Nil)                 => True
        case (Prim(p1), Prim(p2))       => if (p1.intersect(p2).isEmpty) Bool(BoolLattice[B].inject(false)) else Bool(BoolLattice[B].top)
        case (Clo(c1), Clo(c2))         => if (c1.intersect(c2).isEmpty) Bool(BoolLattice[B].inject(false)) else Bool(BoolLattice[B].top)
        case (_: Cons, _: Cons)         => throw new Exception("should not happen")
        case (_: Vec, _: Vec)           => throw new Exception("should not happen")
        case (Pointer(p1), Pointer(p2)) => if (p1.intersect(p2).isEmpty) Bool(BoolLattice[B].inject(false)) else Bool(BoolLattice[B].top)
                                                                  // We can't know for sure that equal addresses are eq (in the abstract). This implementation is not suited for use in a concrete machine!
        case (Thread(t1), Thread(t2))   => if (t1.intersect(t2).isEmpty) Bool(BoolLattice[B].inject(false)) else Bool(BoolLattice[B].top)
        case _                          => False
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
      case CharacterEq => (x, y) match {
        case (Char(c1), Char(c2)) => MayFail.success(Bool(CharLattice[C].charEq(c1, c2)))
        case _                    => MayFail.failure(OperatorNotApplicable("char=?", List(x, y)))
      }
      case CharacterLt => (x, y) match {
        case (Char(c1), Char(c2)) => MayFail.success(Bool(CharLattice[C].charLt(c1, c2)))
        case _                    => MayFail.failure(OperatorNotApplicable("char<?", List(x, y)))
      }
      case CharacterEqCI => (x, y) match {
        case (Char(c1), Char(c2)) => MayFail.success(Bool(CharLattice[C].charEqCI(c1, c2)))
        case _                    => MayFail.failure(OperatorNotApplicable("char-ci=?", List(x, y)))
      }
      case CharacterLtCI => (x, y) match {
        case (Char(c1), Char(c2)) => MayFail.success(Bool(CharLattice[C].charLtCI(c1, c2)))
        case _                    => MayFail.failure(OperatorNotApplicable("char-ci<?", List(x, y)))
      }
    }

    def number(x: scala.Int): Value               = Int(IntLattice[I].inject(x))
    def real(x: Double): Value                    = Real(RealLattice[R].inject(x))
    def string(x: String): Value                  = Str(StringLattice[S].inject(x))
    def bool(x: Boolean): Value                   = Bool(BoolLattice[B].inject(x))
    def char(x: scala.Char): Value                = Char(CharLattice[C].inject(x))
    def primitive(x: P): Value                    = Prim(Set(x))
    def closure(x: schemeLattice.Closure, name: Option[String]): Value  = Clo(Set((x,name)))
    def symbol(x: String): Value                  = Symbol(SymbolLattice[Sym].inject(x))
    def nil: Value                                = Nil
    def cons(car: L, cdr: L): Value               = Cons(car, cdr)
    def pointer(a: A): Value                      = Pointer(Set(a))
    def thread(tid: TID): Value                   = Thread(Set(tid))
    def lock(threads: Set[TID]): Value            = Lock(threads)
    def void: Value                               = Void
    def getClosures(x: Value): Set[(schemeLattice.Closure,Option[String])] = x match {
      case Clo(closures) => closures
      case _             => Set.empty
    }
    def getPrimitives(x: Value): Set[P] = x match {
      case Prim(prms) => prms
      case _          => Set.empty
    }
    def getPointerAddresses(x: Value): Set[A] = x match {
      case Pointer(ptrs)  => ptrs
      case _              => Set.empty
    }
    def getThreads(x: Value): Set[TID] = x match {
      case Thread(t) => t
      case _         => Set.empty
    }
    def getLocks(x: Value): Set[TID] = x match {
      case Lock(l) => l
      case _       => Set.empty
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

    // Indicates whether a lock is held.
    //def isHeld(lock: Value): MayFail[L, Error] = lock match {
    //  case Lock(tids) => MayFail.success(Element(bool(tids.nonEmpty)))
    //  case _          => MayFail.failure(TypeError("acquire: expected a lock", lock))
    //}

    // Acquire creates a new lock to which the given TID is added.
    def acquire(lock: Value, tid: TID): MayFail[L, Error] = lock match {
      case Lock(tids) => MayFail.success(Element(Lock(tids + tid)))
      case _          => MayFail.failure(TypeError("acquire: expected a lock", lock))
    }

    def release(lock: Value, tid: TID): MayFail[L, Error] = lock match {
      case Lock(tids) if tids.contains(tid) => MayFail.success(Element(Lock(tids - tid)))
      case Lock(_) => MayFail.failure(InvalidRelease("Cannot release lock since it is not held by the requesting thread.", lock))
      case _       => MayFail.failure(TypeError("release: expected a lock", lock))
    }
  }

  type L = Elements
  case class Elements(vs: List[Value]) extends SmartHash {
    override def toString: String = 
      if (vs.isEmpty) {
        "⊥"
      } else if (vs.tail.isEmpty) {
        vs.head.toString
      } else {
        vs.map(_.toString).toList.sorted.mkString("{",",","}")
      }
    def foldMapL[X](f: Value => X)(implicit monoid: Monoid[X]): X =
      vs.foldLeft(monoid.zero)((acc, x) => monoid.append(acc, f(x)))
  }
  object Element {
    def apply(v: Value): L = Elements(List(v))
  }

  import MonoidInstances._
  implicit val lMonoid: Monoid[L] = new Monoid[L] {
    private def insert(vs: List[Value], v: Value): List[Value] = vs match {
      case scala.Nil                        => List(v)
      case v0 :: _      if v.ord < v0.ord   => v :: vs
      case v0 :: rest   if v.ord == v0.ord  => Value.join(v,v0) :: rest
      case v0 :: rest                       => v0 :: insert(rest,v)
    }
    def append(x: L, y: => L): L = (x,y) match {
      case (Elements(as), Elements(bs)) => Elements(bs.foldLeft(as)(insert))
    }
    def zero: L = Elements(scala.Nil)
  }
  implicit val lMFMonoid: Monoid[MayFail[L, Error]] = MonoidInstances.mayFail[L]

  val schemeLattice: SchemeLattice[L, A, P] = new SchemeLattice[L, A, P] {
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
    def getThreads(x: L): Set[TID] = x.foldMapL(Value.getThreads)(setMonoid)

    def acquire(lock: L, tid: TID): MayFail[L, Error] =
      lock.foldMapL(l => Value.acquire(l, tid))
    def release(lock: L, tid: TID): MayFail[L, Error] =
      lock.foldMapL(l => Value.release(l, tid))

    def bottom: L                             = Elements(List.empty)
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
    def thread(tid: TID): L                   = Element(Value.thread(tid))
    def lock(threads: Set[TID]): L            = Element(Value.lock(threads))
    def nil: L                                = Element(Value.nil)
    def void: L                               = Element(Value.void)
    def eql[B2: BoolLattice](x: L, y: L): B2 = ??? // TODO[medium] implement
  }

  object L {
    implicit val lattice: SchemeLattice[L, A, P] = schemeLattice
  }
}
