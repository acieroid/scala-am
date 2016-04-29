import scalaz._
import scalaz.Scalaz._


/**
 * Exception to be raised during injection if an element of the lattice is not
 * supported (e.g., a lattice that doesn't support primitives)
 */
object UnsupportedLatticeElement extends Exception

/**
 * Exception to be raised when multiple values cannot be joined
 */
case class CannotJoin[Abs](values: Set[Abs]) extends Exception {
  override def toString = "CannotJoin(" + values.mkString(", ") + ")"
}

/** A (join semi-)lattice L should support the following operations */
trait JoinLattice[L] extends Semigroup[L] {
  /** A lattice has a bottom element */
  def bottom: L
  /** Elements of the lattice can be joined together */
  def join(x: L, y: L): L
  def append(x: L, y: => L): L = join(x, y) /* A lattice is trivially a semigroup by using join as append */
  /** Subsumption between two elements can be checked */
  def subsumes(x: L, y: L): Boolean

  /** We have some more components for convenience */
  /** A name identifying the lattice */
  def name: String
  /** It should state whether it supports abstract counting or not. (TODO: this is probably not the best place for that) */
  def counting: Boolean

  /** Some elements can be considered as errors */
  def isError(x: L): Boolean
  /** Some elements may contain addresses in there and are therefore not considered as primitive values */
  def isPrimitiveValue(x: L): Boolean
}

/**
 * We define here some domains that can will be useful to build a lattice for most languages.
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
  def append(s1: S, s2: S): S
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
  def toString[S : IsString](n: I): S
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
  def toString[S : IsString](n: F): S
}

trait IsChar[C] extends LatticeElement[C] {
  def inject(c: Char): C
}

trait IsSymbol[Sym] extends LatticeElement[Sym] {
  def inject(sym: String): Sym
}

/**
 * Some implementations of these abstract domains
 */
object ConcreteString {
  type S = ISet[String]
  implicit val isString = new IsString[S] {
    def name = "ConcreteString"
    private def showString(s: String) = "\"" + s + "\""
    override def shows(x: S): String = if (x.size == 1) { showString(x.elems.head) } else { "{" + x.toList.map(showString _).mkString(",") + "}" }
    def top: S = throw new Error("Concrete lattice has no top value")
    val bot: S = ISet.empty
    def join(x: S, y: => S) = x.union(y)
    def subsumes(x: S, y: => S) = y.isSubsetOf(x)

    def inject(x: String): S = ISet.singleton(x)
    def length[I](s: S)(implicit int: IsInteger[I]): I = s.foldMap(s => int.inject(s.size))
    def append(s1: S, s2: S): S = s1.foldMap(s1 => s2.map(s2 => s1 + s2))
    def eql[B](s1: S, s2: S)(implicit bool: IsBoolean[B]): B = s1.foldMap(s1 => s2.foldMap(s2 => bool.inject(s1 == s2)))

    def order(x: S, y: S): Ordering = implicitly[Order[ISet[String]]].order(x, y)
  }
}

object ConcreteBoolean {
  type B = ISet[Boolean]
  implicit val isBoolean: IsBoolean[B] = new IsBoolean[B] {
    def name = "ConcreteBoolean"
    private def showBool(b: Boolean) = if (b) "#t" else "#f"
    override def shows(x: B): String = if (x.size == 1) { showBool(x.elems.head) } else { "{" + x.elems.map(showBool _).mkString(",") + "}" }
    val bot: B = ISet.empty
    val top: B = ISet.fromList(List(true, false))
    def join(x: B, y: => B) = x.union(y)
    def subsumes(x: B, y: => B) = y.isSubsetOf(x)

    def inject(x: Boolean): B = ISet.singleton(x)
    def isTrue(b: B): Boolean = b.contains(true)
    def isFalse(b: B): Boolean = b.contains(false)
    def not(b: B): B = b.map(x => !x)
    def eql[B1](b1: B, b2: B)(implicit bool: IsBoolean[B1]): B1 = b1.foldMap(b1 => b2.foldMap(b2 => bool.inject(b1 == b2)))

    def order(x: B, y: B): Ordering = implicitly[Order[ISet[Boolean]]].order(x, y)
  }
}

object ConcreteInteger {
  type I = ISet[Int]
  implicit val isInteger = new IsInteger[I] {
    def name = "ConcreteInteger"
    override def shows(x: I): String = if (x.size == 1) { x.elems.head.toString } else { "{" + x.elems.mkString(",") + "}" }
    val bot: I = ISet.empty
    def top: I = throw new Error("Concrete lattice has no top value")
    def join(x: I, y: => I) = x.union(y)
    def subsumes(x: I, y: => I) = y.isSubsetOf(x)

    def inject(x: Int): I = ISet.singleton(x)
    def ceiling(n: I): I = n
    def toFloat[F](n: I)(implicit float: IsFloat[F]): F = n.foldMap(n => float.inject(n))
    def random(n: I): I = n.map(n => SchemeOps.random(n))
    def plus(n1: I, n2: I): I = n1.foldMap(n1 => n2.map(n2 => n1 + n2))
    def minus(n1: I, n2: I): I = n1.foldMap(n1 => n2.map(n2 => n1 - n2))
    def times(n1: I, n2: I): I = n1.foldMap(n1 => n2.map(n2 => n1 * n2))
    def div(n1: I, n2: I): I = n1.foldMap(n1 => n2.map(n2 => n1 / n2))
    def modulo(n1: I, n2: I): I = n1.foldMap(n1 => n2.map(n2 => SchemeOps.modulo(n1, n2)))
    def lt[B](n1: I, n2: I)(implicit bool: IsBoolean[B]): B = n1.foldMap(n1 => n2.foldMap(n2 => bool.inject(n1 < n2)))
    def eql[B](n1: I, n2: I)(implicit bool: IsBoolean[B]): B = n1.foldMap(n1 => n2.foldMap(n2 => bool.inject(n1 == n2)))
    def toString[S](n: I)(implicit str: IsString[S]): S = n.foldMap(n => str.inject(n.toString))

    def order(x: I, y: I): Ordering = implicitly[Order[ISet[Int]]].order(x, y)
  }
}

object ConcreteFloat {
  type F = ISet[Float]
  implicit val isFloat = new IsFloat[F] {
    def name = "ConcreteFloat"
    override def shows(x: F): String = if (x.size == 1) { x.elems.head.toString } else { "{" + x.elems.mkString(",") + "}" }
    val bot: F = ISet.empty
    def top: F = throw new Error("Concrete lattice has no top value")
    def join(x: F, y: => F) = x.union(y)
    def subsumes(x: F, y: => F) = y.isSubsetOf(x)

    def inject(x: Float): F = ISet.singleton(x)
    def ceiling(n: F): F = n.map(_.ceil)
    def log(n: F): F = n.map(n => scala.math.log(n.toDouble).toFloat)
    def random(n: F): F = n.map(n => SchemeOps.random(n))
    def plus(n1: F, n2: F): F = n1.foldMap(n1 => n2.map(n2 => n1 + n2))
    def minus(n1: F, n2: F): F = n1.foldMap(n1 => n2.map(n2 => n1 - n2))
    def times(n1: F, n2: F): F = n1.foldMap(n1 => n2.map(n2 => n1 * n2))
    def div(n1: F, n2: F): F = n1.foldMap(n1 => n2.map(n2 => n1 / n2))
    def lt[B](n1: F, n2: F)(implicit bool: IsBoolean[B]): B = n1.foldMap(n1 => n2.foldMap(n2 => bool.inject(n1 < n2)))
    def eql[B](n1: F, n2: F)(implicit bool: IsBoolean[B]): B = n1.foldMap(n1 => n2.foldMap(n2 => bool.inject(n1 == n2)))
    def toString[S](n: F)(implicit str: IsString[S]): S = n.foldMap(n => str.inject(n.toString))

    def order(x: F, y: F): Ordering = implicitly[Order[ISet[Float]]].order(x, y)
  }
}

object ConcreteChar {
  type C = ISet[Char]
  implicit val isChar = new IsChar[C] {
    def name = "ConcreteChar"
    private def showChar(c: Char) = s"#\\$c"
    override def shows(x: C): String = if (x.size == 1) { showChar(x.elems.head) } else { "{" + x.elems.map(showChar _).mkString(",") + "}" }

    val bot: C = ISet.empty
    def top: C = throw new Error("Concrete lattice has no top value")
    def join(x: C, y: => C) = x.union(y)
    def subsumes(x: C, y: => C) = y.isSubsetOf(x)

    def inject(x: Char): C = ISet.singleton(x)
    def eql[B](c1: C, c2: C)(implicit bool: IsBoolean[B]): B = c1.foldMap(c1 => c2.foldMap(c2 => bool.inject(c1 == c2)))

    def order(x: C, y: C): Ordering = implicitly[Order[ISet[Char]]].order(x, y)
  }
}

object ConcreteSymbol {
  sealed trait Symbol
  type Sym = ISet[String @@ Symbol]
  implicit val symOrder = new Order[String @@ Symbol] {
    def order(x: String @@ Symbol, y: String @@ Symbol) = implicitly[Order[String]].order(Tag.unwrap(x), Tag.unwrap(y))
  }
  implicit val isSymbol = new IsSymbol[Sym] {
    def name = "ConcreteSymbol"
    def showSym(x: String @@ Symbol): String = Tag.unwrap(x)
    override def shows(x: Sym): String = if (x.size == 1) { showSym(x.elems.head) } else { "{" + x.elems.map(showSym _).mkString(",") + "}" }

    val bot: Sym = ISet.empty
    def top: Sym = throw new Error("Concrete lattice has no top value")
    def join(x: Sym, y: => Sym) = x.union(y)
    def subsumes(x: Sym, y: => Sym) = y.isSubsetOf(x)

    def inject(x: String): Sym = ISet.singleton(Tag[String, Symbol](x))
    def eql[B](s1: Sym, s2: Sym)(implicit bool: IsBoolean[B]): B = s1.foldMap(s1 => s2.foldMap(s2 => bool.inject(s1 == s2)))

    def order(x: Sym, y: Sym): Ordering = implicitly[Order[ISet[String @@ Symbol]]].order(x, y)
  }
}

class BoundedInteger(bound: Int) {
  sealed trait I
  case object Top extends I
  case class Set(content: ISet[Int]) extends I
  implicit val isMonoid = new Monoid[I] {
    def zero: I = Set(ISet.empty)
    def append(x: I, y: => I) = x match {
      case Set(xs) => y match {
        case Set(ys) => Set(xs.union(ys))
        case Top => y
      }
      case Top => x
    }
  }
  implicit val isInteger = new IsInteger[I] {
    def name = s"BoundedInteger($bound)"
    override def shows(x: I): String = x match {
      case Set(xs) if xs.size == 1 => xs.elems.head.toString
      case Set(xs) => "{" + xs.elems.mkString(",") + "}"
      case Top => "Int"
    }
    val bot: I = implicitly[Monoid[I]].zero
    val top: I = Top
    def join(x: I, y: => I) = implicitly[Monoid[I]].append(x, y)
    def subsumes(x: I, y: => I) = x match {
      case Set(xs) => y match {
        case Set(ys) => ys.isSubsetOf(xs)
        case Top => false
      }
      case Top => true
    }
    private def promote(x: ISet[Int]): I = x.findMax match {
      case Some(i) if Math.abs(i) > bound => Top
      case _ => x.findMin match {
        case Some(i) if Math.abs(i) > bound => Top
        case _ => Set(x)
      }
    }
    private def fold[L](x: I, f: Int => L)(implicit b: LatticeElement[L]): L = x match {
      case Set(xs) => xs.foldMap(f)
      case Top => b.top
    }
    private def foldI(x: I, f: Int => I) = x match {
      case Set(xs) => xs.foldMap(f)(isMonoid)
      case Top => Top
    }
    def inject(x: Int): I = promote(ISet.singleton(x))
    def ceiling(n: I): I = n
    def toFloat[F](n: I)(implicit float: IsFloat[F]): F = fold(n, n => float.inject(n))
    def random(n: I): I = Top
    def plus(n1: I, n2: I): I = foldI(n1, n1 => foldI(n2, n2 => inject(n1 + n2)))
    def minus(n1: I, n2: I): I = foldI(n1, n1 => foldI(n2, n2 => inject(n1 - n2)))
    def times(n1: I, n2: I): I = foldI(n1, n1 => foldI(n2, n2 => inject(n1 * n2)))
    def div(n1: I, n2: I): I = foldI(n1, n1 => foldI(n2, n2 => inject(n1 / n2)))
    def modulo(n1: I, n2: I): I = foldI(n1, n1 => foldI(n2, n2 => inject(SchemeOps.modulo(n1, n2))))
    def lt[B](n1: I, n2: I)(implicit bool: IsBoolean[B]): B = fold(n1, n1 => fold(n2, n2 => bool.inject(n1 < n2)))
    def eql[B](n1: I, n2: I)(implicit bool: IsBoolean[B]): B = fold(n1, n1 => fold(n2, n2 => bool.inject(n1 == n2)))
    def toString[S](n: I)(implicit str: IsString[S]): S = fold(n, n => str.inject(n.toString))

    def order(x: I, y: I): Ordering = (x, y) match {
      case (Set(xs), Set(ys)) => implicitly[Order[ISet[Int]]].order(xs, ys)
      case (Top, _: Set) => Ordering.GT
      case (_: Set, Top) => Ordering.LT
      case (Top, Top) => Ordering.EQ
    }
  }
}

object Type {
  sealed trait T
  case object Top extends T
  case object Bottom extends T

  implicit val typeIsMonoid = new Monoid[T] {
    def zero: T = Bottom
    def append(x: T, y: => T): T = x match {
      case Top => Top
      case Bottom => y
    }
  }
  abstract class BaseInstance(typeName: String) extends LatticeElement[T] {
    def name = s"Type$typeName"
    override def shows(x: T): String = x match {
      case Top => typeName
      case Bottom => "⊥"
    }
    val bot: T = Bottom
    val top: T = Top
    def join(x: T, y: => T) = typeIsMonoid.append(x, y)
    def meet(x: T, y: => T): T = x match {
      case Bottom => Bottom
      case Top => y
    }
    def subsumes(x: T, y: => T) = x match {
      case Top => true
      case Bottom => y match {
        case Top => false
        case Bottom => true
      }
    }
    def eql[B](n1: T, n2: T)(implicit bool: IsBoolean[B]): B = (n1, n2) match {
      case (Top, Top) => bool.top
      case _ => bool.bot
    }
    // def order(x: T, y: T): Ordering = implicitly[Order[T]].order(x, y)
    def order(x: T, y: T): Ordering = (x, y) match {
      case (Top, Top) => Ordering.EQ
      case (Top, Bottom) => Ordering.GT
      case (Bottom, Top) => Ordering.LT
      case (Bottom, Bottom) => Ordering.EQ
    }
  }
  implicit val typeIsString: IsString[T] = new BaseInstance("Str") with IsString[T] {
    def inject(x: String): T = Top
    def length[I](s: T)(implicit int: IsInteger[I]) = s match {
      case Top => int.top
      case Bottom => int.bot
    }
    def append(s1: T, s2: T) = (s1, s2) match {
      case (Top, _) => Top
      case (_, Top) => Top
      case _ => Bottom
    }
  }
  implicit val typeIsBoolean: IsBoolean[T] = new BaseInstance("Bool") with IsBoolean[T] {
    def inject(x: Boolean): T = Top
    def isTrue(b: T) = b == Top
    def isFalse(b: T) = b == Top
    def not(b: T) = b
  }
  implicit val typeIsInteger: IsInteger[T] = new BaseInstance("Int") with IsInteger[T] {
    def inject(x: Int): T = Top
    def ceiling(n: T): T = n
    def toFloat[F](n: T)(implicit float: IsFloat[F]): F = n match {
      case Top => float.top
      case Bottom => float.bot
    }
    def random(n: T): T = n
    def plus(n1: T, n2: T): T = meet(n1, n2)
    def minus(n1: T, n2: T): T = meet(n1, n2)
    def times(n1: T, n2: T): T = meet(n1, n2)
    def div(n1: T, n2: T): T = meet(n1, n2)
    def modulo(n1: T, n2: T): T = meet(n1, n2)
    def lt[B](n1: T, n2: T)(implicit bool: IsBoolean[B]): B = (n1, n2) match {
      case (Top, Top) => bool.top
      case _ => bool.bot
    }
    def toString[S](n: T)(implicit str: IsString[S]): S = n match {
      case Top => str.top
      case Bottom => str.bot
    }
  }
  implicit val typeIsFloat: IsFloat[T] = new BaseInstance("Float") with IsFloat[T] {
    def inject(x: Float): T = Top
    def ceiling(n: T): T = n
    def log(n: T): T = n
    def random(n: T): T = n
    def plus(n1: T, n2: T): T = meet(n1, n2)
    def minus(n1: T, n2: T): T = meet(n1, n2)
    def times(n1: T, n2: T): T = meet(n1, n2)
    def div(n1: T, n2: T): T = meet(n1, n2)
    def lt[B](n1: T, n2: T)(implicit bool: IsBoolean[B]): B = (n1, n2) match {
      case (Top, Top) => bool.top
      case _ => bool.bot
    }
    def toString[S](n: T)(implicit str: IsString[S]): S = n match {
      case Top => str.top
      case Bottom => str.bot
    }
  }
  implicit val typeIsChar: IsChar[T] = new BaseInstance("Char") with IsChar[T] {
    def inject(c: Char): T = Top
  }
  implicit val typeIsSymbol: IsSymbol[T] = new BaseInstance("Sym") with IsSymbol[T] {
    def inject(sym: String): T = Top
  }
}

/** A lattice for Scheme should support the following operations */
trait SchemeLattice[L] extends JoinLattice[L] {
  /** Can this value be considered true for conditionals? */
  def isTrue(x: L): Boolean
  /** Can this value be considered false for conditionals? */
  def isFalse(x: L): Boolean
  /** Performs a unary operation on the abstract value x */
  def unaryOp(op: SchemeOps.UnaryOperator)(x: L): L
  /** Performs a binary operation on abstract values x and y */
  def binaryOp(op: SchemeOps.BinaryOperator)(x: L, y: L): L
  /** Conjunction */
  def and(x: L, y: => L): L
  /** Disjunction */
  def or(x: L, y: => L): L
  /** Extract closures contained in this value */
  def getClosures[Exp : Expression, Addr : Address](x: L): Set[(Exp, Environment[Addr])]
  /** Extract primitives contained in this value */
  def getPrimitives[Addr : Address, Abs : JoinLattice](x: L): Set[Primitive[Addr, Abs]]


  /** Injection of an integer */
  def inject(x: Int): L
  /** Injection of a float */
  def inject(x: Float): L
  /** Injection of a string */
  def inject(x: String): L
  /** Injection of a boolean */
  def inject(x: Boolean): L
  /** Injection of a character */
  def inject(x: Char): L
  /** Injection of a primitive function */
  def inject[Addr : Address, Abs : JoinLattice](x: Primitive[Addr, Abs]): L
  /** Injection of a closure */
  def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])): L
  /** Injection of a symbol */
  def injectSymbol(x: String): L
  /** Creates a cons cell */
  def cons[Addr : Address](car: Addr, cdr: Addr): L
  /** Nil value */
  def nil: L
}

/** A lattice for Concurrent Scheme */
trait ConcurrentSchemeLattice[L] extends SchemeLattice[L] {
  /** Extract thread ids contained in this value */
  def getTids[TID : ThreadIdentifier](x: L): Set[TID]
  /** Extract lock addresses contained in this value */
  def getLocks[Addr : Address](x: L): Set[Addr]

  /** Inject a thread id */
  def injectTid[TID : ThreadIdentifier](tid: TID): L
  /** Creates a lock wrapper (that contains the address of the lock) */
  def lock[Addr : Address](addr: Addr): L
  /** The locked value */
  def lockedValue: L
  /** The unlocked value */
  def unlockedValue: L
}

/** Internals of a lattice for Scheme, used by the primitives' definitions */
trait SchemeLatticeInternals[L] extends SchemeLattice[L] {
  /** Injects an error */
  def error(x: L): L
  /** Takes the car of a cons cell */
  def car[Addr : Address](x: L): Set[Addr]
  /** Takes the cdr of a cons cell */
  def cdr[Addr : Address](x: L): Set[Addr]
  /** Get a value from a vector. Returns either an error or the addresses where to look for the values */
  def vectorRef[Addr : Address](vector: L, index: L): Set[Either[L, Addr]]
  /** Changes a value inside a vector. The address given is an address where the
   * value can be stored if needed.  Returns the vector value, as well as the
   * addresses to update in the store. The value stored is not passed to
   * vectorSet, but will be stored in the returned addresses. */
  def vectorSet[Addr : Address](vector: L, index: L, addr: Addr): (L, Set[Addr])
  /** Extract vector addresses contained in this value */
  def getVectors[Addr : Address](x: L): Set[Addr]
  def vector[Addr : Address](addr: Addr, size: L, init: Addr): (L, L)
}

/** Abstract values are abstract representations of the possible values of a variable */
trait AbstractValue[A] extends SchemeLatticeInternals[A] with ConcurrentSchemeLattice[A] {
  /** Returns the string representation of this value */
  def toString[Addr : Address](x: A, store: Store[Addr, A]): String

}

object AbstractValue

trait Lattice {
  type L
  val isAbstractValue: AbstractValue[L]
}
