import scalaz._
import scalaz.Scalaz._

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
  def bottom: L
  def top: L
  def join(x: L, y: => L): L
  def subsumes(x: L, y: => L): Boolean
  def eql[B : IsBoolean](s1: L, s2: L): B

  /* For Monoid[L] */
  final def zero: L = bottom
  final def append(x: L, y: => L): L = join(x, y)
}

/** A lattice for strings */
trait IsString[S] extends LatticeElement[S] {
  def inject(s: String): S
  def length[I : IsInteger](s: S): I
  def append(s1: S, s2: S): S
}

/** A lattice for booleans */
trait IsBoolean[B] extends LatticeElement[B] {
  def inject(b: Boolean): B
  def isTrue(b: B): Boolean
  def isFalse(b: B): Boolean
  def not(b: B): B
}

/** A lattice for integers */
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

/** A lattice for floats */
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

/** A lattice for characters */
trait IsChar[C] extends LatticeElement[C] {
  def inject(c: Char): C
}

/** A lattice for symbols */
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
    val bottom: S = ISet.empty
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
    val bottom: B = ISet.empty
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
    val bottom: I = ISet.empty
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
    val bottom: F = ISet.empty
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

    val bottom: C = ISet.empty
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
  type Sym = ISet[String]
  implicit val isSymbol = new IsSymbol[Sym] {
    def name = "ConcreteSymbol"
    override def shows(x: Sym): String = if (x.size == 1) { x.elems.head } else { "{" + x.elems.mkString(",") + "}" }

    val bottom: Sym = ISet.empty
    def top: Sym = throw new Error("Concrete lattice has no top value")
    def join(x: Sym, y: => Sym) = x.union(y)
    def subsumes(x: Sym, y: => Sym) = y.isSubsetOf(x)

    def inject(x: String): Sym = ISet.singleton(x)
    def eql[B](s1: Sym, s2: Sym)(implicit bool: IsBoolean[B]): B = s1.foldMap(s1 => s2.foldMap(s2 => bool.inject(s1 == s2)))

    def order(x: Sym, y: Sym): Ordering = implicitly[Order[ISet[String]]].order(x, y)
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
    val bottom: I = implicitly[Monoid[I]].zero
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
    val bottom: T = Bottom
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
      case _ => bool.bottom
    }
    // def order(x: T, y: T): Ordering = implicitly[Order[T]].order(x, y)
    def order(x: T, y: T): Ordering = (x, y) match {
      case (Top, Top) => Ordering.EQ
      case (Top, Bottom) => Ordering.GT
      case (Bottom, Top) => Ordering.LT
      case (Bottom, Bottom) => Ordering.EQ
    }
  }
  implicit val typeIsLatticeElement: LatticeElement[T] = new BaseInstance("Type") {
  }
  implicit val typeIsString: IsString[T] = new BaseInstance("Str") with IsString[T] {
    def inject(x: String): T = Top
    def length[I](s: T)(implicit int: IsInteger[I]) = s match {
      case Top => int.top
      case Bottom => int.bottom
    }
    def append(s1: T, s2: T) = (s1, s2) match {
      case (Bottom, _) => Bottom
      case (_, Bottom) => Bottom
      case (Top, _) => Top
      case (_, Top) => Top
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
      case Bottom => float.bottom
    }
    def random(n: T): T = n
    def plus(n1: T, n2: T): T = meet(n1, n2)
    def minus(n1: T, n2: T): T = meet(n1, n2)
    def times(n1: T, n2: T): T = meet(n1, n2)
    def div(n1: T, n2: T): T = meet(n1, n2)
    def modulo(n1: T, n2: T): T = meet(n1, n2)
    def lt[B](n1: T, n2: T)(implicit bool: IsBoolean[B]): B = (n1, n2) match {
      case (Top, Top) => bool.top
      case _ => bool.bottom
    }
    def toString[S](n: T)(implicit str: IsString[S]): S = n match {
      case Top => str.top
      case Bottom => str.bottom
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
      case _ => bool.bottom
    }
    def toString[S](n: T)(implicit str: IsString[S]): S = n match {
      case Top => str.top
      case Bottom => str.bottom
    }
  }
  implicit val typeIsChar: IsChar[T] = new BaseInstance("Char") with IsChar[T] {
    def inject(c: Char): T = Top
  }
  implicit val typeIsSymbol: IsSymbol[T] = new BaseInstance("Sym") with IsSymbol[T] {
    def inject(sym: String): T = Top
  }
}

class ConstantPropagation[A](implicit ordering: Order[A]) {
  sealed trait L
  case object Top extends L
  case class Constant(s: A) extends L
  case object Bottom extends L

  implicit val constantIsMonoid = new Monoid[L] {
    def zero: L = Bottom
    def append(x: L, y: => L): L = x match {
      case Top => Top
      case Constant(_) => if (x == y) { x } else { Top }
      case Bottom => y
    }
  }

  abstract class BaseInstance(typeName: String) extends LatticeElement[L] {
    def name = s"ConstantPropagation$typeName"
    override def shows(x: L): String = x match {
      case Top => typeName
      case Constant(x) => x.toString
      case Bottom => "⊥"
    }
    val bottom: L = Bottom
    val top: L = Top
    def join(x: L, y: => L) = constantIsMonoid.append(x, y)
    def meet(x: L, y: => L): L = x match {
      case Bottom => Bottom
      case Constant(_) => y match {
        case Top => x
        case Constant(_) => if (x == y) { x } else { Bottom }
        case Bottom => Bottom
      }
      case Top => y
    }
    def subsumes(x: L, y: => L) = x match {
      case Top => true
      case Constant(_) => y match {
        case Top => false
        case Constant(_) => x == y
        case Bottom => true
      }
      case Bottom => y match {
        case Top => false
        case Constant(_) => false
        case Bottom => true
      }
    }
    def eql[B](n1: L, n2: L)(implicit bool: IsBoolean[B]): B = (n1, n2) match {
      case (Top, Top) => bool.top
      case (Top, Constant(_)) => bool.top
      case (Constant(_), Top) => bool.top
      case (Constant(x), Constant(y)) => if (x == y) { bool.inject(true) } else { bool.inject(false) }
      case (Bottom, _) => bool.bottom
      case (_, Bottom) => bool.bottom
    }
    def order(x: L, y: L): Ordering = (x, y) match {
      case (Top, Top) => Ordering.EQ
      case (Top, _) => Ordering.GT
      case (Constant(_), Top) => Ordering.LT
      case (Constant(x), Constant(y)) => ordering.order(x, y)
      case (Constant(_), Bottom) => Ordering.GT
      case (Bottom, Bottom) => Ordering.EQ
      case (Bottom, _) => Ordering.LT
    }
  }
}

object StringConstantPropagation extends ConstantPropagation[String] {
  type S = L
  implicit val isString: IsString[S] = new BaseInstance("Str") with IsString[S] {
    def inject(x: String): S = Constant(x)
    def length[I](s: S)(implicit int: IsInteger[I]) = s match {
      case Top => int.top
      case Constant(s) => int.inject(s.size)
      case Bottom => int.bottom
    }
    def append(s1: S, s2: S) = (s1, s2) match {
      case (Bottom, _) => Bottom
      case (_, Bottom) => Bottom
      case (Top, _) => Top
      case (_, Top) => Top
      case (Constant(x), Constant(y)) => Constant(x ++ y)
    }
  }
}

object IntegerConstantPropagation extends ConstantPropagation[Int] {
  type I = L
  implicit val isInteger: IsInteger[I] = new BaseInstance("Int") with IsInteger[I] {
    def inject(x: Int): I = Constant(x)
    def ceiling(n: I): I = n
    def toFloat[F](n: I)(implicit float: IsFloat[F]): F = n match {
      case Top => float.top
      case Constant(x) => float.inject(x)
      case Bottom => float.bottom
    }
    def random(n: I): I = n match {
      case Constant(x) => Constant(SchemeOps.random(x))
      case _ => n
    }
    private def binop(op: (Int, Int) => Int, n1: I, n2: I) = (n1, n2) match {
      case (Top, Top) => Top
      case (Top, Constant(_)) => Top
      case (Constant(_), Top) => Top
      case (Constant(x), Constant(y)) => Constant(op(x, y))
      case _ => Bottom
    }
    def plus(n1: I, n2: I): I = binop(_ + _, n1, n2)
    def minus(n1: I, n2: I): I = binop(_ - _, n1, n2)
    def times(n1: I, n2: I): I = binop(_ * _, n1, n2)
    def div(n1: I, n2: I): I = binop(_ / _, n1, n2)
    def modulo(n1: I, n2: I): I = binop(SchemeOps.modulo _, n1, n2)
    def lt[B](n1: I, n2: I)(implicit bool: IsBoolean[B]): B = (n1, n2) match {
      case (Top, Top) => bool.top
      case (Top, Constant(_)) => bool.top
      case (Constant(_), Top) => bool.top
      case (Constant(x), Constant(y)) => if (x < y) { bool.inject(true) } else { bool.inject(false) }
      case _ => bool.bottom
    }
    def toString[S](n: I)(implicit str: IsString[S]): S = n match {
      case Top => str.top
      case Constant(x) => str.inject(x.toString)
      case Bottom => str.bottom
    }
  }
}

object FloatConstantPropagation extends ConstantPropagation[Float] {
  type F = L
  implicit val isFloat: IsFloat[F] = new BaseInstance("Float") with IsFloat[F] {
    def inject(x: Float) = Constant(x)
    def ceiling(n: F): F = n
    def random(n: F): F = n match {
      case Constant(x) => Constant(SchemeOps.random(x))
      case _ => n
    }
    def log(n: F): F = n match {
      case Constant(x) => Constant(scala.math.log(x.toDouble).toFloat)
      case _ => n
    }
    private def binop(op: (Float, Float) => Float, n1: F, n2: F) = (n1, n2) match {
      case (Top, Top) => Top
      case (Top, Constant(_)) => Top
      case (Constant(_), Top) => Top
      case (Constant(x), Constant(y)) => Constant(op(x, y))
      case _ => Bottom
    }
    def plus(n1: F, n2: F): F = binop(_ + _, n1, n2)
    def minus(n1: F, n2: F): F = binop(_ - _, n1, n2)
    def times(n1: F, n2: F): F = binop(_ * _, n1, n2)
    def div(n1: F, n2: F): F = binop(_ / _, n1, n2)
    def lt[B](n1: F, n2: F)(implicit bool: IsBoolean[B]): B = (n1, n2) match {
      case (Top, Top) => bool.top
      case (Top, Constant(_)) => bool.top
      case (Constant(_), Top) => bool.top
      case (Constant(x), Constant(y)) => if (x < y) { bool.inject(true) } else { bool.inject(false) }
      case _ => bool.bottom
    }
    def toString[S](n: F)(implicit str: IsString[S]): S = n match {
      case Top => str.top
      case Constant(x) => str.inject(x.toString)
      case Bottom => str.bottom
    }
  }
}

object CharConstantPropagation extends ConstantPropagation[Char] {
  type C = L
  implicit val isChar: IsChar[C] = new BaseInstance("Char") with IsChar[C] {
    def inject(x: Char) = Constant(x)
  }
}

object SymbolConstantPropagation extends ConstantPropagation[String] {
  type Sym = L
  implicit val isSymbol: IsSymbol[Sym] = new BaseInstance("Sym") with IsSymbol[Sym] {
    def inject(x: String) = Constant(x)
  }
}

trait AbstractValue[A] extends SchemeLatticeInternals[A] with ConcurrentSchemeLattice[A]

object AbstractValue

trait Lattice {
  type L
  val isAbstractValue: AbstractValue[L]
}
