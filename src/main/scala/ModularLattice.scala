import scalaz.{Plus => _, _}
import scalaz.Scalaz._
import OrderDerive._

import UnaryOperator._
import BinaryOperator._

/**
 * A lattice element should define its name, the bottom value, how to join two
 * elements, if an element subsumes another, as well as total order (not linked
 * to the subsumption).
 */
trait LatticeElement[L] extends Order[L] with Monoid[L] with Show[L] {
  def name: String
  def bot: L
  def top: L
  def join(x: L, y: => L): L
  def subsumes(x: L, y: => L): Boolean
  def eql[B : IsBoolean](s1: L, s2: L): B

  /* For Monoid[L] */
  final def zero: L = bot
  final def append(x: L, y: => L): L = join(x, y)
}

trait IsString[S] extends LatticeElement[S] {
  def inject(s: String): S
  def length[I : IsInteger](s: S): I
}

trait IsBoolean[B] extends LatticeElement[B] {
  def inject(b: Boolean): B
  def isTrue(b: B): Boolean
  def isFalse(b: B): Boolean
  def not(b: B): B
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
  def modulo(n1: I, n2: I): I
  def lt[B : IsBoolean](n1: I, n2: I): B
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
}

trait IsChar[C] extends LatticeElement[C] {
  def inject(c: Char): C
}

trait IsSymbol[Sym] extends LatticeElement[Sym] {
  def inject(sym: String): Sym
}

class MakeLattice[S, B, I, F, C, Sym](implicit str: IsString[S],
  bool: IsBoolean[B], int: IsInteger[I], float: IsFloat[F], char: IsChar[C],
  sym: IsSymbol[Sym]) {
  sealed trait Value
  case object Bot extends Value {
    override def toString = "⊥"
  }
  case class Str(s: S) extends Value {
    override def toString = str.shows(s)
  }
  case class Bool(b: B) extends Value {
    override def toString = bool.shows(b)
  }
  case class Int(i: I) extends Value {
    override def toString = int.shows(i)
  }
  case class Float(f: F) extends Value {
    override def toString = float.shows(f)
  }
  case class Char(c: C) extends Value {
    override def toString = char.shows(c)
  }
  case class Symbol(s: Sym) extends Value {
    override def toString = sym.shows(s)
  }
  case class Err(message: String) extends Value
  case class Prim[Addr : Address, Abs : AbstractValue](prim: Primitive[Addr, Abs]) extends Value {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class Closure[Exp : Expression, Addr : Address](lambda: Exp, env: Environment[Addr]) extends Value {
    override def toString = "#<clo>"
  }
  case class Cons[Addr : Address](car: Addr, cdr: Addr) extends Value
  case object Nil extends Value {
    override def toString = "()"
  }
  case class Vec[Addr : Address](size: I, elements: Map[I, Addr], init: Addr) extends Value {
    override def toString = {
      val els = elements.toList.map({ case (k, v) => s"${int.shows(k)}: $v" }).mkString(", ")
      s"Vec(${int.shows(size)}, {$els}, $init)"
    }
  }
  case class VectorAddress[Addr : Address](a: Addr) extends Value
  case class Tid[TID : ThreadIdentifier](t: TID) extends Value {
    override def toString = s"#<thread $t>"
  }
  case class LockAddress[Addr : Address](addr: Addr) extends Value
  case object Locked extends Value
  case object Unlocked extends Value

  val True = Bool(bool.inject(true))
  val False = Bool(bool.inject(false))

  type L = Value

  val isAbstractValue = new AbstractValue[L] {
    def name = s"Lattice(${str.name}, ${bool.name}, ${int.name}, ${float.name}, ${char.name}, ${sym.name})"
    /* This doesn't deal with circular structures (vectors and cons cells) */
    override def shows[Addr : Address, Abs : AbstractValue](x: L, store: Store[Addr, Abs]) = x match {
      case Bot => "⊥"
      case Str(s) => str.shows(s)
      case Bool(b) => bool.shows(b)
      case Int(i) => int.shows(i)
      case Float(f) => float.shows(f)
      case Char(c) => char.shows(c)
      case Symbol(s) => sym.shows(s)
      case Err(msg) => s"Error($msg)"
      case Nil => "()"
      case Cons(car: Addr, cdr: Addr) => {
        val carstr = implicitly[AbstractValue[Abs]].shows(store.lookup(car), store)
        val cdrstr = implicitly[AbstractValue[Abs]].shows(store.lookup(cdr), store)
        s"($carstr . $cdrstr)"
      }
      case VectorAddress(a: Addr) => implicitly[AbstractValue[Abs]].shows(store.lookup(a), store)
      case Vec(size, elements: Map[I, Addr], init: Addr) => {
        val initstr = implicitly[AbstractValue[Abs]].shows(store.lookup(init), store)
        val content = elements.toList.map({ case (k, a)  => s"${int.shows(k)}: ${implicitly[AbstractValue[Abs]].shows(store.lookup(a), store)}"}).mkString(", ")
        if (content.isEmpty) {
          s"#(default: $initstr)"
        } else {
          s"#($content, default: $initstr)"
        }
      }
      case LockAddress(a: Addr) => implicitly[AbstractValue[Abs]].shows(store.lookup(a), store)
      case Unlocked => "#unlocked"
      case Locked => "#locked"
      case _ => x.toString
    }

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
    def isPrimitiveValue(x: L): Boolean = x match {
      case Bot | Str(_) | Bool(_) | Int(_) | Float(_) | Char(_) | Symbol(_) | Err(_) | Nil | Locked | Unlocked => true
      case Closure(_, _) | Prim(_) | Tid(_) | Cons(_, _) | VectorAddress(_) | Vec(_, _, _) | LockAddress(_) => false
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
        case Vec(_, _, _) => True
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
        case _ => Err(s"Ceiling not applicable")
      }
      case Log => x match {
        case Int(n) => Float(float.log(int.toFloat(n)))
        case Float(n) => Float(float.log(n))
        case _ => Err(s"Log not applicable")
      }
      case Random => x match {
        case Int(n) => Int(int.random(n))
        case Float(n) => Float(float.random(n))
        case _ => Err(s"Random not applicable")
      }
      case VectorLength => x match {
        case Vec(size, _, _) => Int(size)
        case _ => Err(s"VectorLength not applicable")
      }
      case StringLength => x match {
        case Str(s) => Int(str.length(s))
        case _ => Err(s"StringLength not applicable")
      }
    }}

    def binaryOp(op: BinaryOperator)(x: L, y: L): L = op match {
      case Plus => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.plus(n1, n2))
        case (Int(n1), Float(n2)) => Float(float.plus(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Float(float.plus(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Float(float.plus(n1, n2))
        case _ => Err(s"Plus not applicable")
      }
      case Minus => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.minus(n1, n2))
        case (Int(n1), Float(n2)) => Float(float.minus(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Float(float.minus(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Float(float.minus(n1, n2))
        case _ => Err(s"Minus not applicable")
      }
      case Times => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.times(n1, n2))
        case (Int(n1), Float(n2)) => Float(float.times(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Float(float.times(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Float(float.times(n1, n2))
        case _ => Err(s"Times not applicable")
      }
      /* TODO: have a div for integer division (i.e., Scheme's quotient), and one for real division (/)) */
      case Div => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.div(n1, n2))
        case (Int(n1), Float(n2)) => Float(float.div(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Float(float.div(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Float(float.div(n1, n2))
        case _ => Err(s"Div not applicable")
      }
      case Modulo => (x, y) match {
        case (Int(n1), Int(n2)) => Int(int.modulo(n1, n2))
        case _ => Err(s"Modulo not applicable")
      }
      case Lt => (x, y) match {
        case (Int(n1), Int(n2)) => Bool(int.lt(n1, n2))
        case (Int(n1), Float(n2)) => Bool(float.lt(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Bool(float.lt(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Bool(float.lt(n1, n2))
        case _ => Err(s"Lt not applicable")
      }
      case NumEq => (x, y) match {
        case (Int(n1), Int(n2)) => Bool(int.eql(n1, n2))
        case (Int(n1), Float(n2)) => Bool(float.eql(int.toFloat(n1), n2))
        case (Float(n1), Int(n2)) => Bool(float.eql(n1, int.toFloat(n2)))
        case (Float(n1), Float(n2)) => Bool(float.eql(n1, n2))
        case _ => Err(s"NumEq not applicable")
      }
      case Eq => (x, y) match {
        case (Str(s1), Str(s2)) => Bool(str.eql(s1, s2)) /* TODO: this isn't really physical equality for strings */
        case (Bool(b1), Bool(b2)) => Bool(bool.eql(b1, b2))
        case (Int(n1), Int(n2)) => Bool(int.eql(n1, n2))
        case (Float(n1), Float(n2)) => Bool(float.eql(n1, n2))
        case (Char(c1), Char(c2)) => Bool(char.eql(c1, c2))
        case (Symbol(s1), Symbol(s2)) => Bool(sym.eql(s1, s2))
        case (Nil, Nil) => True
        case (Prim(_), Prim(_)) => Bool(bool.inject(x == y))
        case (Closure(_, _), Closure(_, _)) => Bool(bool.inject(x == y))
        case (Cons(_, _), Cons(_, _)) => Bool(bool.inject(x == y))
        case (VectorAddress(_), VectorAddress(_)) => Bool(bool.inject(x == y))
        case (LockAddress(_), LockAddress(_)) => Bool(bool.inject(x == y))
        case _ => False
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
          case (Bot, _) => y
          case (_, Bot) => x
          case (Str(s1), Str(s2)) => Str(str.join(s1, s2))
          case (Bool(b1), Bool(b2)) => Bool(bool.join(b1, b2))
          case (Int(i1), Int(i2)) => Int(int.join(i1, i2))
          case (Float(f1), Float(f2)) => Float(float.join(f1, f2))
          case (Char(c1), Char(c2)) => Char(char.join(c1, c2))
          case _ => throw new CannotJoin[L](Set(x, y))
        }
      }

    def meet(x: L, y: L): L = ??? // TODO: remove meet, we don't use it

    def subsumes(x: L, y: L): Boolean = if (x == y) { true } else {
      (x, y) match {
        case (_, Bot) => true
        case (Str(s1), Str(s2)) => str.subsumes(s1, s2)
        case (Bool(b1), Bool(b2)) => bool.subsumes(b1, b2)
        case (Int(i1), Int(i2)) => int.subsumes(i1, i2)
        case (Float(f1), Float(f2)) => float.subsumes(f1, f2)
        case (Char(c1), Char(c2)) => char.subsumes(c1, c2)
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

    def vectorRef[Addr : Address](x: L, index: L): Set[Either[L, Addr]] = (x, index) match {
      case (Vec(size, content: Map[I, Addr], init: Addr), Int(index)) => {
        val comp = int.lt(index, size)
        val t: Set[Either[L, Addr]] = if (bool.isTrue(comp)) {
          content.get(index) match {
            case Some(a: Addr) =>
              if (bool.isTrue(int.eql(index, index)) && !bool.isFalse(int.eql(index, index))) {
                /* we know index represents a concrete integer, we can return only one address */
                Set(Right(a))
              } else {
                /* otherwise, init should be returned as well for soundness */
                Set(Right(a), Right(init))
              }
            case None => Set(Right(init))
          }
        } else { Set() }
        /* Don't perform bound checks here because we would get too many spurious flows */
        val f: Set[Either[L, Addr]] = Set()
        t ++ f
      }
      case (_: Vec[Addr], _) => Set(Left(Err(s"Vector ref with non-integer index")))
      case _ => Set(Left(Err(s"Vector ref on non-vector")))
    }

    def vectorSet[Addr : Address](vector: L, index: L, addr: Addr): (L, Set[Addr]) = (vector, index) match {
      case (Vec(size, content: Map[I, Addr], init: Addr), Int(index)) => {
        val comp = int.lt(index, size)
        val t: (L, Set[Addr]) = if (bool.isTrue(comp)) {
          content.get(index) match {
            case Some(a: Addr) => (vector, Set(a))
            case None => (Vec(size, content + (index -> addr), init), Set(addr))
          }
        } else { (Bot, Set()) }
        val f: (L, Set[Addr]) = (Bot, Set())
        (join(t._1, f._1), t._2 ++ f._2)
      }
      case (_: Vec[Addr], _) => (Err(s"Vector set with non-integer index"), Set())
      case _ => (Err(s"Vector set on non-vector"), Set())
    }

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
    def vector[Addr : Address](addr: Addr, size: L, init: Addr) = size match {
      case Int(size) => (VectorAddress(addr), Vec(size, Map[I, Addr](), init))
      case _ => (Err(s"vector creation expects an integer size"), Bot)
    }
    def lock[Addr : Address](addr: Addr) = LockAddress(addr)
    def lockedValue = Locked
    def unlockedValue = Unlocked
  }

  sealed trait LSet
  case class Element(v: Value) extends LSet {
    override def toString = v.toString
  }
  case class Elements(vs: Set[Value]) extends LSet {
    override def toString = "{" + vs.mkString(",") + "}"
  }
  val boolOrMonoid = new Monoid[Boolean] {
    def append(x: Boolean, y: => Boolean): Boolean = x || y
    def zero: Boolean = false
  }
  val boolAndMonoid = new Monoid[Boolean] {
    def append(x: Boolean, y: => Boolean): Boolean = x && y
    def zero: Boolean = true
  }
  private def wrap(x: => Value): LSet = try { Element(x) } catch {
    case err: CannotJoin[Value] => Elements(err.values)
  }
  implicit val lsetMonoid = new Monoid[LSet] {
    def append(x: LSet, y: => LSet): LSet = x match {
      case Element(Bot) => y
      case Element(a) => y match {
        case Element(Bot) => x
        case Element(b) => wrap(isAbstractValue.join(a, b))
        case _: Elements => append(Elements(Set(a)), y)
      }
      case Elements(as) => y match {
        case Element(Bot) => x
        case Element(b) => append(x, Elements(Set(b)))
        case Elements(bs) =>
          /* every element in the other set has to be joined in this set */
          Elements(as.foldLeft(bs)((acc, x2) =>
            if (acc.exists(x1 => isAbstractValue.subsumes(x1, x2))) {
              /* the set already contains an element that subsumes x2, don't add it to the set */
              acc
            } else {
              /* remove all elements subsumed by x2 and add x2 to the set */
              val subsumed = acc.filter(x1 => isAbstractValue.subsumes(x2, x1))
              (acc -- subsumed) + x2
            }))
      }
    }
    def zero: LSet = Element(Bot)
  }

  private def foldMapLSet[B](x: LSet, f: L => B)(implicit b: Monoid[B]): B = x match {
    case Element(x) => f(x)
    case Elements(xs) => xs.foldMap(x => f(x))(b)
  }
  val isAbstractValueSet = new AbstractValue[LSet] {
    def name = s"SetLattice(${str.name}, ${bool.name}, ${int.name}, ${float.name}, ${char.name}, ${sym.name})"
    override def shows[Addr : Address, Abs : AbstractValue](x: LSet, store: Store[Addr, Abs]) = x match {
      case Element(x) => isAbstractValue.shows(x, store)
      case Elements(xs) => "{" + xs.map(x => isAbstractValue.shows(x, store)).mkString(",") + "}"
    }

    def isTrue(x: LSet): Boolean = foldMapLSet(x, isAbstractValue.isTrue(_))(boolOrMonoid)
    def isFalse(x: LSet): Boolean = foldMapLSet(x, isAbstractValue.isFalse(_))(boolOrMonoid)
    def isError(x: LSet): Boolean = foldMapLSet(x, isAbstractValue.isError(_))(boolAndMonoid)
    def isNotError(x: LSet): Boolean = foldMapLSet(x, isAbstractValue.isNotError(_))(boolAndMonoid)
    def isPrimitiveValue(x: LSet): Boolean = foldMapLSet(x, isAbstractValue.isPrimitiveValue(_))(boolAndMonoid)
    def unaryOp(op: UnaryOperator)(x: LSet): LSet = foldMapLSet(x, x => wrap(isAbstractValue.unaryOp(op)(x)))
    def binaryOp(op: BinaryOperator)(x: LSet, y: LSet): LSet = foldMapLSet(x, x => foldMapLSet(y, y => wrap(isAbstractValue.binaryOp(op)(x, y))))
    def join(x: LSet, y: LSet): LSet = implicitly[Monoid[LSet]].append(x, y)
    def meet(x: LSet, y: LSet): LSet = ???
    def subsumes(x: LSet, y: LSet): Boolean = foldMapLSet(y, y =>
      /* For every element in y, there exists an element of x that subsumes it */
      foldMapLSet(x, x => isAbstractValue.subsumes(x, y))(boolOrMonoid))(boolAndMonoid)
    def and(x: LSet, y: => LSet): LSet = foldMapLSet(x, x => foldMapLSet(y, y => wrap(isAbstractValue.and(x, y))))
    def or(x: LSet, y: => LSet): LSet = foldMapLSet(x, x => foldMapLSet(y, y => wrap(isAbstractValue.or(x, y))))
    def car[Addr : Address](x: LSet): Set[Addr] = foldMapLSet(x, x => isAbstractValue.car(x))
    def cdr[Addr : Address](x: LSet): Set[Addr] = foldMapLSet(x, x => isAbstractValue.cdr(x))

    def vectorRef[Addr : Address](vector: LSet, index: LSet): Set[Either[LSet, Addr]] = foldMapLSet(vector, vector => foldMapLSet(index, index =>
      isAbstractValue.vectorRef(vector, index).map(_.left.map((v: L) => wrap(v)))))
    def vectorSet[Addr : Address](vector: LSet, index: LSet, addr: Addr): (LSet, Set[Addr]) = foldMapLSet(vector, vector => foldMapLSet(index, index =>
      isAbstractValue.vectorSet(vector, index, addr) match {
        case (v, addrs) => (wrap(v), addrs)
      }))

    def toString[Addr : Address](x: LSet, store: Store[Addr, LSet]): String = x match {
      case Element(x) => x.toString
      case Elements(xs) => "{" + xs.mkString(",") + "}"
    }
    def getClosures[Exp : Expression, Addr : Address](x: LSet): Set[(Exp, Environment[Addr])] = foldMapLSet(x, x => isAbstractValue.getClosures(x))
    def getPrimitives[Addr : Address, Abs : AbstractValue](x: LSet): Set[Primitive[Addr, Abs]] = foldMapLSet(x, x => isAbstractValue.getPrimitives(x))
    def getTids[TID : ThreadIdentifier](x: LSet): Set[TID] = foldMapLSet(x, x => isAbstractValue.getTids(x))
    def getVectors[Addr : Address](x: LSet): Set[Addr] = foldMapLSet(x, x => isAbstractValue.getVectors(x))
    def getLocks[Addr : Address](x: LSet): Set[Addr] = foldMapLSet(x, x => isAbstractValue.getLocks(x))

    def bottom: LSet = Element(isAbstractValue.bottom)
    def error(x: LSet): LSet = Element(isAbstractValue.error(isAbstractValue.inject(x.toString))) // TODO: could be improved
    def inject(x: scala.Int): LSet = Element(isAbstractValue.inject(x))
    def inject(x: scala.Float): LSet = Element(isAbstractValue.inject(x))
    def inject(x: String): LSet = Element(isAbstractValue.inject(x))
    def inject(x: scala.Char): LSet = Element(isAbstractValue.inject(x))
    def inject(x: Boolean): LSet = Element(isAbstractValue.inject(x))
    def inject[Addr : Address, Abs : AbstractValue](x: Primitive[Addr, Abs]): LSet = Element(isAbstractValue.inject(x))
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): LSet = Element(isAbstractValue.inject(x))
    def injectTid[TID : ThreadIdentifier](tid: TID): LSet = Element(isAbstractValue.injectTid(tid))
    def injectSymbol(x: String): LSet = Element(isAbstractValue.injectSymbol(x))
    def cons[Addr : Address](car: Addr, cdr: Addr): LSet = Element(isAbstractValue.cons(car, cdr))
    def vector[Addr : Address](addr: Addr, size: LSet, init: Addr): (LSet, LSet) = foldMapLSet(size, size =>
      isAbstractValue.vector(addr, size, init) match { case (a, v) => (Element(a), Element(v)) })
    def lock[Addr : Address](addr: Addr): LSet = Element(isAbstractValue.lock(addr))
    def lockedValue: LSet = Element(isAbstractValue.lockedValue)
    def unlockedValue: LSet = Element(isAbstractValue.unlockedValue)
    def nil: LSet = Element(isAbstractValue.nil)
  }
}
