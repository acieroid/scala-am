import scalaz.{Plus => _, _}
import scalaz.Scalaz._

import UnaryOperator._
import BinaryOperator._

/* TODO: */
trait StoreShow[L] {
  def shows[Addr : Address, Abs : AbstractValue](l: L, store: Store[Addr, Abs]): String
}

/**
 * A lattice element should define its name, the bottom value, how to join two
 * elements, if an element subsumes another, as well as total order (not linked
 * to the subsumption).
 */
trait LatticeElement[L] extends Order[L] with Show[L] with Monoid[L] {
  def name: String
  def bot: L
  def join(x: L, y: L): L
  def subsumes(x: L, y: L): Boolean

  /* For Monoid[L] */
  final def zero: L = bot
  final def append(x: L, y: => L): L = join(x, y)
}

trait IsString[S] extends LatticeElement[S] {
  def inject(s: String): S
  def length[I : IsInteger](s: S): I
  def eql[B : IsBoolean](s1: S, s2: S): B
}

trait IsBoolean[B] extends LatticeElement[B] {
  def inject(b: Boolean): B
  def isTrue(b: B): Boolean
  def isFalse(b: B): Boolean
  def not(b: B): B
  def eql(b1: B, b2: B): B
}

trait IsInteger[I] extends LatticeElement[I] {
  def inject(n: Int): I
  def ceiling(n: I): I
  def toFloat[F : IsFloat](n: I): F
  def random(n: I): I
  def plus(n1: I, n2: I): I
  def minus(n1: I, n2: I): I
  def times(n1: I, n2: I): I
  def div(n1: I, n2: I): I
  def lt[B : IsBoolean](n1: I, n2: I): B
  def eql[B : IsBoolean](n1: I, n2: I): B
}

trait IsFloat[F] extends LatticeElement[F] {
  def inject(n: Float): F
  def ceiling(n: F): F
  def log(n: F): F
  def random(n: F): F
  def plus(n1: F, n2: F): F
  def minus(n1: F, n2: F): F
  def times(n1: F, n2: F): F
  def div(n1: F, n2: F): F
  def lt[B : IsBoolean](n1: F, n2: F): B
  def eql[B : IsBoolean](n1: F, n2: F): B
}

trait IsChar[C] extends LatticeElement[C] {
  def inject(c: Char): C
  def eql[B : IsBoolean](c1: C, c2: C): B
}

trait IsSymbol[Sym] extends LatticeElement[Sym] {
  def inject(sym: String): Sym
  def eql[B : IsBoolean](sym1: Sym, sym2: Sym): B
}

trait IsVector[V] extends LatticeElement[V] {
  def inject[I : IsInteger, A : AbstractValue](size: I, init: A): V
  def length[I : IsInteger](v: V): I
  def ref[I : IsInteger, A : AbstractValue](v: V, index: I): A
  def set[I : IsInteger, A : AbstractValue](v: V, index: I): V
}

class MakeLattice[S, B, I, F, C, Sym, V](implicit str: IsString[S],
  bool: IsBoolean[B], int: IsInteger[I], float: IsFloat[F], char: IsChar[C],
  sym: IsSymbol[Sym], vec: IsVector[V]) {
  trait Value
  case object Bot extends Value
  case class Str(s: S) extends Value
  case class Bool(b: B) extends Value
  case class Int(i: I) extends Value
  case class Float(f: F) extends Value
  case class Char(c: C) extends Value
  case class Vec(v: V) extends Value
  case class Symbol(s: Sym) extends Value
  case class Err(message: String) extends Value
  case class Prim[Addr : Address, Abs : AbstractValue](prim: Primitive[Addr, Abs]) extends Value
  case class Closure[Exp : Expression, Addr : Address](lambda: Exp, env: Environment[Addr]) extends Value
  case object Nil extends Value
  case class Cons[Addr : Address](car: Addr, cdr: Addr) extends Value
  case class VectorAddress[Addr : Address](a: Addr) extends Value
  case class Tid[TID : ThreadIdentifier](t: TID) extends Value
  case class LockAddress[Addr : Address](addr: Addr) extends Value
  object Locked extends Value
  object Unlocked extends Value

  val True = Bool(bool.inject(true))
  val False = Bool(bool.inject(false))

  type L = Value
  implicit val isOrder: Order[L] = ???
  implicit val isMonoid: Monoid[L] = ???
  val isAbstractValue = new AbstractValue[L] {
    def name = s"Lattice(${str.name}, ${bool.name}, ${int.name}, ${float.name}, ${char.name}, ${sym.name}, ${vec.name})"
    def isTrue(x: L): Boolean = x match {
      case Bool(b) => bool.isTrue(b)
      case Bot => false
      case _ => true
    }
    def isFalse(x: L): Boolean = x match {
      case Bool(b) => bool.isFalse(b)
      case Bot => true
      case _ => false
    }
    def isError(x: L): Boolean = x match {
      case Err(_) => true
      case _ => false
    }
    def isNotError(x: L): Boolean = x match {
      case Err(_) => false
      case _ => true
    }

    def unaryOp(op: UnaryOperator)(x: L): L = if (x == Bot) { Bot } else { op match {
      case IsNull => x match {
        case Nil => True
        case _ => False
      }
      case IsCons => x match {
        case Cons(_, _) => True
        case _ => False
      }
      case IsChar => x match {
        case Char(_) => True
        case _ => False
      }
      case IsSymbol => x match {
        case Symbol(_) => True
        case _ => False
      }
      case IsString => x match {
        case Str(_) => True
        case _ => False
      }
      case IsInteger => x match {
        case Int(_) => True
        case _ => False
      }
      case IsFloat => x match {
        case Float(_) => True
        case _ => False
      }
      case IsBoolean => x match {
        case Bool(_) => True
        case _ => False
      }
      case IsVector => x match {
        case Vec(_) => True
        case VectorAddress(_) => True
        case _ => False
      }
      case IsLock => x match {
        case LockAddress(_) => True
        case Locked => True
        case Unlocked => True
        case _ => False
      }
      case Not => x match {
        case Bool(b) => Bool(bool.not(b))
        case _ => False /* any value is true */
      }
      case Ceiling => x match {
        case Int(n) => Int(int.ceiling(n))
        case Float(n) => Float(float.ceiling(n))
        case _ => Err(s"Ceiling not applicable to $x")
      }
      case Log => x match {
        case Int(n) => Float(float.log(int.toFloat(n)))
        case Float(n) => Float(float.log(n))
        case _ => Err(s"Log not applicable to $x")
      }
      case Random => x match {
        case Int(n) => Int(int.random(n))
        case Float(n) => Float(float.random(n))
        case _ => Err(s"Random not applicable to $x")
      }
      case VectorLength => x match {
        case Vec(v) => Int(vec.length(v))
        case _ => Err(s"VectorLength not applicable to $x")
      }
      case StringLength => x match {
        case Str(s) => Int(str.length(s))
        case _ => Err(s"StringLength not applicable to $x")
      }
    }}

    def binaryOp(op: BinaryOperator)(x: L, y: L): L = op match {
      case Plus => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.plus(n1, n2))
        case (Int(n1), Float(n2)) => Float(float.plus(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Float(float.plus(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Float(float.plus(n1, n2))
        case _ => Err(s"Plus not applicable to $x and $y")
      }
      case Minus => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.minus(n1, n2))
        case (Int(n1), Float(n2)) => Float(float.minus(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Float(float.minus(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Float(float.minus(n1, n2))
        case _ => Err(s"Minus not applicable to $x and $y")
      }
      case Times => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.times(n1, n2))
        case (Int(n1), Float(n2)) => Float(float.times(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Float(float.times(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Float(float.times(n1, n2))
        case _ => Err(s"Times not applicable to $x and $y")
      }
      /* TODO: have a div for integer division (i.e., Scheme's quotient), and one for real division (/)) */
      case Div => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.div(n1, n2))
        case (Int(n1), Float(n2)) => Float(float.div(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Float(float.div(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Float(float.div(n1, n2))
        case _ => Err(s"Div not applicable to $x and $y")
      }
      case Lt => (x, y) match {
        case (Int(n1), Int(n2)) => Bool(int.lt(n1, n2))
        case (Int(n1), Float(n2)) => Bool(float.lt(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Bool(float.lt(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Bool(float.lt(n1, n2))
        case _ => Err(s"Lt not applicable to $x and $y")
      }
      case NumEq => (x, y) match {
        case (Int(n1), Int(n2)) => Bool(int.eql(n1, n2))
        case (Int(n1), Float(n2)) => Bool(float.eql(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Bool(float.eql(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Bool(float.eql(n1, n2))
        case _ => Err(s"NumEq not applicable to $x and $y")
      }
      case Eq => (x, y) match {
        /* TODO: this isn't physical equality (for strings only) */
        case (Str(s1), Str(s2)) => Bool(str.eql(s1, s2))
        case (Bool(b1), Bool(b2)) => Bool(bool.eql(b1, b2))
        case (Int(n1), Int(n2)) => Bool(int.eql(n1, n2))
        case (Float(n1), Float(n2)) => Bool(float.eql(n1, n2))
        case (Char(c1), Char(c2)) => Bool(char.eql(c1, c2))
        case (Symbol(s1), Symbol(s2)) => Bool(sym.eql(s1, s2))
        /* TODO: Could be implemented in other cases as well */
        case _ => False
      }
      case VectorRef => (x, y) match {
        case (Vec(v), Int(index)) => vec.ref[I, L](v, index)(int, this)
        case _ => Err(s"VectorRef not applicable to $x and $y")
      }
    }

    def and(x: L, y: => L): L = x match {
      case Bot => False
      case Bool(b) => (bool.isTrue(b), bool.isFalse(b)) match {
        case (true, false) => y
        case (false, _) => False
        case (true, true) => join(False, y)
      }
      case _ => y
    }

    def or(x: L, y: => L): L = x match {
      case Bot => y
      case Bool(b) => (bool.isTrue(b), bool.isFalse(b)) match {
        case (true, false) => x
        case (false, _) => y
        case (true, true) => join(x, y)
      }
      case _ => x
    }

    def join(x: L, y: L): L =
      if (x == y) {
        x
      } else {
        (x, y) match {
          case (Str(s1), Str(s2)) => Str(str.join(s1, s2))
          case (Bool(b1), Bool(b2)) => Bool(bool.join(b1, b2))
          case (Int(i1), Int(i2)) => Int(int.join(i1, i2))
          case (Float(f1), Float(f2)) => Float(float.join(f1, f2))
          case (Char(c1), Char(c2)) => Char(char.join(c1, c2))
          case (Vec(v1), Vec(v2)) => Vec(vec.join(v1, v2))
          case _ => throw new CannotJoin[L](Set(x, y))
        }
      }

    def meet(x: L, y: L): L = ???

    def subsumes(x: L, y: L): Boolean = if (x == y) { true } else {
      (x, y) match {
        case (Str(s1), Str(s2)) => str.subsumes(s1, s2)
        case (Bool(b1), Bool(b2)) => bool.subsumes(b1, b2)
        case (Int(i1), Int(i2)) => int.subsumes(i1, i2)
        case (Float(f1), Float(f2)) => float.subsumes(f1, f2)
        case (Char(c1), Char(c2)) => char.subsumes(c1, c2)
        case (Vec(v1), Vec(v2)) => vec.subsumes(v1, v2)
        case _ => false
      }
    }

    def car[Addr : Address](x: L): Set[Addr] = x match {
      case Cons(car: Addr, cdr: Addr) => Set(car)
      case _ => Set()
    }

    def cdr[Addr : Address](x: L): Set[Addr] = x match {
      case Cons(car: Addr, cdr: Addr) => Set(cdr)
      case _ => Set()
    }

    def vectorSet[Addr : Address](x: L, index: L, value: L): L = ???

    def toString[Addr : Address](x: L, store: Store[Addr, L]) = ???

    def getClosures[Exp : Expression, Addr : Address](x: L) = x match {
      case Closure(lam: Exp, env: Environment[Addr]) => Set((lam, env))
      case _ => Set()
    }
    def getPrimitives[Addr : Address, Abs : AbstractValue](x: L) = x match {
      case Prim(p: Primitive[Addr, Abs]) => Set(p)
      case _ => Set()
    }
    def getTids[TID : ThreadIdentifier](x: L) = x match {
      case Tid(t: TID) => Set(t)
      case _ => Set()
    }
    def getVectors[Addr : Address](x: L) = x match {
      case VectorAddress(a: Addr) => Set(a)
      case _ => Set()
    }
    def getLocks[Addr : Address](x: L) = x match {
      case LockAddress(a: Addr) => Set(a)
      case _ => Set()
    }

    def bottom = Bot
    def error(x: L): L = Err(x.toString)
    def inject(x: scala.Int): L = Int(int.inject(x))
    def inject(x: scala.Float): L = Float(float.inject(x))
    def inject(x: String): L = Str(str.inject(x))
    def inject(x: scala.Char): L = Char(char.inject(x))
    def inject(x: Boolean): L = Bool(bool.inject(x))
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]): L = Prim(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L = Closure(x._1, x._2)
    def injectTid[TID : ThreadIdentifier](tid: TID): L = Tid(tid)
    def injectSymbol(x: String): L = Symbol(sym.inject(x))
    def nil: L = Nil
    def cons[Addr : Address](car: Addr, cdr: Addr): L = Cons(car, cdr)
    def vector[Addr : Address](addr: Addr, size: L, init: L) = ???
    def lock[Addr : Address](addr: Addr) = LockAddress(addr)
    def lockedValue = Locked
    def unlockedValue = Unlocked
  }

  type LSet = Value \/ ISet[Value]
  val boolOrMonoid = new Monoid[Boolean] {
    def append(x: Boolean, y: => Boolean): Boolean = x || y
    def zero: Boolean = false
  }
  val boolAndMonoid = new Monoid[Boolean] {
    def append(x: Boolean, y: => Boolean): Boolean = x && y
    def zero: Boolean = true
  }
  implicit val isOrderSet: Order[LSet] = ???
  implicit val isMonoidSet: Monoid[LSet] = ???
  val isAbstractValueSet = new AbstractValue[LSet] {
    def name = s"SetLattice(${str.name}, ${bool.name}, ${int.name}, ${float.name}, ${char.name}, ${sym.name}, ${vec.name})"
    def isTrue(x: LSet): Boolean = x match {
      case -\/(x) => isAbstractValue.isTrue(x)
      case \/-(xs) => xs.foldMap(x => isAbstractValue.isTrue(x))(boolOrMonoid)
    }
    def isFalse(x: LSet): Boolean = x match {
      case -\/(x) => isAbstractValue.isFalse(x)
      case \/-(xs) => xs.foldMap(x => isAbstractValue.isFalse(x))(boolOrMonoid)
    }
    def isError(x: LSet): Boolean = x match {
      case -\/(x) => isAbstractValue.isError(x)
      case \/-(xs) => xs.foldMap(x => isAbstractValue.isError(x))(boolOrMonoid)
    }
    /* TODO
    def isNotError(x: LSet): Boolean = x match {
      case -\/(x) => isAbstractValue.isNotError(x)
      case \/-(xs) => xs.foldMap(x => isAbstractValue.isNotError(x))(boolAndMonoid)
    } */
    private def wrap(x: => Value): LSet = try { -\/(x) } catch {
      case err: CannotJoin[Value] => \/-(ISet.fromList(err.values.toList))
    }
    def unaryOp(op: UnaryOperator)(x: LSet): LSet = x match {
      case -\/(x) => -\/(isAbstractValue.unaryOp(op)(x))
      case \/-(xs) => xs.foldMap(x => wrap(isAbstractValue.unaryOp(op)(x)))
    }
    def binaryOp(op: BinaryOperator)(x: LSet, y: LSet): LSet = (x, y) match {
      case (-\/(x), -\/(y)) => -\/(isAbstractValue.binaryOp(op)(x, y))
      case (-\/(x), \/-(ys)) => ys.foldMap(y => wrap(isAbstractValue.binaryOp(op)(x, y)))
      case (\/-(xs), -\/(y)) => xs.foldMap(x => wrap(isAbstractValue.binaryOp(op)(x, y)))
      case (\/-(xs), \/-(ys)) => xs.foldMap(x => ys.foldMap(y => wrap(isAbstractValue.binaryOp(op)(x, y))))
    }
    def join(x: LSet, y: LSet): LSet = (x, y) match {
      case (-\/(x), -\/(y)) => wrap(isAbstractValue.join(x, y))
      case (-\/(x), \/-(ys)) => ???
      case (\/-(xs), -\/(y)) => ???
      case (\/-(xs), \/-(ys)) => ???
    }
    def meet(x: LSet, y: LSet): LSet = ???
    def subsumes(x: LSet, y: LSet): Boolean = (x, y) match {
      case (-\/(x), -\/(y)) => isAbstractValue.subsumes(x, y)
      case (-\/(x), \/-(ys)) => ys.foldMap(y => isAbstractValue.subsumes(x, y))(boolAndMonoid)
      case (\/-(xs), -\/(y)) => xs.foldMap(x => isAbstractValue.subsumes(x, y))(boolOrMonoid)
      case (\/-(xs), \/-(ys)) => ys.foldMap(y => xs.foldMap(x => isAbstractValue.subsumes(x, y))(boolOrMonoid))(boolAndMonoid)
    }
    def and(x: LSet, y: => LSet): LSet = ???
    def or(x: LSet, y: => LSet): LSet = ???
    def car[Addr : Address](x: LSet): Set[Addr] = x match {
      case -\/(x) => isAbstractValue.car(x)
      case \/-(xs) => xs.foldMap(x => isAbstractValue.car(x))
    }
    def cdr[Addr : Address](x: LSet): Set[Addr] = x match {
      case -\/(x) => isAbstractValue.cdr(x)
      case \/-(xs) => xs.foldMap(x => isAbstractValue.cdr(x))
    }
    def vectorSet[Addr : Address](vector: LSet, index: LSet, value: LSet): LSet = ???
    def toString[Addr : Address](x: LSet, store: Store[Addr, LSet]): String = ???
    /* TODO: instead:
     def toString[Addr : Address, Abs : AbstractValue](x: LSet, store: Store[Addr, AbstractValue]) */
    def getClosures[Exp : Expression, Addr : Address](x: LSet): Set[(Exp, Environment[Addr])] = x match {
      case -\/(x) => isAbstractValue.getClosures(x)
      case \/-(xs) => xs.foldMap(x => isAbstractValue.getClosures(x))
    }
    def getPrimitives[Addr : Address, Abs : AbstractValue](x: LSet): Set[Primitive[Addr, Abs]] = x match {
      case -\/(x) => isAbstractValue.getPrimitives(x)
      case \/-(xs) => xs.foldMap(x => isAbstractValue.getPrimitives(x))
    }
    def getTids[TID : ThreadIdentifier](x: LSet): Set[TID] = x match {
      case -\/(x) => isAbstractValue.getTids(x)
      case \/-(xs) => xs.foldMap(x => isAbstractValue.getTids(x))
    }
    def getVectors[Addr : Address](x: LSet): Set[Addr] = x match {
      case -\/(x) => isAbstractValue.getVectors(x)
      case \/-(xs) => xs.foldMap(x => isAbstractValue.getVectors(x))
    }
    def getLocks[Addr : Address](x: LSet): Set[Addr] = x match {
      case -\/(x) => isAbstractValue.getLocks(x)
      case \/-(xs) => xs.foldMap(x => isAbstractValue.getLocks(x))
    }

    def bottom: LSet = -\/(isAbstractValue.bottom)
    def error(x: LSet): LSet = -\/(isAbstractValue.error(isAbstractValue.inject(x.toString))) // TODO: could be improved
    def inject(x: scala.Int): LSet = -\/(isAbstractValue.inject(x))
    def inject(x: scala.Float): LSet = -\/(isAbstractValue.inject(x))
    def inject(x: String): LSet = -\/(isAbstractValue.inject(x))
    def inject(x: scala.Char): LSet = -\/(isAbstractValue.inject(x))
    def inject(x: Boolean): LSet = -\/(isAbstractValue.inject(x))
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]): LSet = -\/(isAbstractValue.inject(x))
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): LSet = -\/(isAbstractValue.inject(x))
    def injectTid[TID : ThreadIdentifier](tid: TID): LSet = -\/(isAbstractValue.injectTid(tid))
    def injectSymbol(x: String): LSet = -\/(isAbstractValue.injectSymbol(x))
    def cons[Addr : Address](car: Addr, cdr: Addr): LSet = -\/(isAbstractValue.cons(car, cdr))
    def vector[Addr : Address](addr: Addr, size: LSet, init: LSet): (LSet, LSet) = ???
    def lock[Addr : Address](addr: Addr): LSet = -\/(isAbstractValue.lock(addr))
    def lockedValue: LSet = -\/(isAbstractValue.lockedValue)
    def unlockedValue: LSet = -\/(isAbstractValue.unlockedValue)
    def nil: LSet = -\/(isAbstractValue.nil)
  }
}

