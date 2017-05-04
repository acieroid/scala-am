/* TODO: fix the bot case: any binary operation applied to bot should return bot. This holds for eql, modulo, etc. */

import scalaz._
import scalaz.Scalaz._

/**
 * Some implementations of these abstract domains
 */
object Concrete {
  sealed trait L[+A] {
    def foldMap[B : LatticeElement](f: A => B): B = this match {
      case Top => LatticeElement[B].top
      case Values(content) => content.foldMap(f)
    }
    def map[B](f: A => B)(implicit order: Order[B]): L[B] = this match {
      case Top => Top
      case Values(content) => Values(content.map(f))
    }
    def guardBot[B : LatticeElement](body: => B): B = this match {
      case Values(content) if content.length == 0 => LatticeElement[B].bottom
      case _ => body
    }
  }
  case object Top extends L[Nothing]
  case class Values[A](content: ISet[A]) extends L[A]

  type S = L[String]
  type B = L[Boolean]
  type I = L[Int]
  type F = L[Float]
  type C = L[Char]
  type Sym = L[String]

  object L {
    abstract class BaseInstance[A](typeName: String)(implicit ordering: Order[A], show: Show[A]) extends LatticeElement[L[A]] {
      def name = s"Concrete$typeName"
      override def shows(x: L[A]): String = x match {
        case Top => typeName
        case Values(content) if content.size == 1 => Show[A].shows(content.elems.head)
        case Values(content) => "{" + content.elems.map(Show[A].shows).mkString(",") + "}"
      }
      val top: L[A] = Top
      val bottom: L[A] = Values[A](ISet.empty)
      def join(x: L[A], y: => L[A]): L[A] = x match {
        case Top => Top
        case Values(content1) => y match {
          case Top => Top
          case Values(content2) => Values(content1.union(content2))
        }
      }
      def subsumes(x: L[A], y: => L[A]) = x match {
        case Top => true
        case Values(content1) => y match {
          case Top => false
          case Values(content2) => content2.isSubsetOf(content1)
        }
      }
      def eql[B : BoolLattice](x: L[A], y: L[A]): B = y.guardBot { x.foldMap(a => y.foldMap(b => BoolLattice[B].inject(a == b))) }
      def order(x: L[A], y: L[A]): Ordering = (x, y) match {
        case (Top, Top) => Ordering.EQ
        case (Top, _) => Ordering.GT
        case (_, Top) => Ordering.LT
        case (Values(content1), Values(content2)) => Order[ISet[A]].order(content1, content2)
      }
      def cardinality(x: L[A]): Cardinality = x match {
        case Top => CardinalityPrimitiveLikeInf()
        case Values(content) => CardinalityPrimitiveLikeNumber(content.toList.length)
      }
    }
    val stringShow: Show[String] = new Show[String] {
      override def shows(s: String): String = "\"" + s + "\""
    }
    implicit val stringConcrete: StringLattice[S] = new BaseInstance[String]("Str")(Order[String], stringShow) with StringLattice[S] {
      def inject(x: String): S = Values(ISet.singleton(x))
      def length[I : IntLattice](s: S): I = s.foldMap(s => IntLattice[I].inject(s.length))
      def append(s1: S, s2: S): S = (s1, s2) match {
        case (Values(bot), _) if bot.length == 0 => Values(bot)
        case (_, Values(bot)) if bot.length == 0 => Values(bot)
        case (Top, _) | (_, Top) => Top
        case (Values(content1), Values(content2)) => Values(content1.foldMap(s1 => content2.map(s2 => s1 + s2)))
      }
      def lt[B : BoolLattice](s1: S, s2: S): B = (s1, s2) match {
        case (Values(bot), _) if bot.length == 0 => BoolLattice[B].bottom
        case (_, Values(bot)) if bot.length == 0 => BoolLattice[B].bottom
        case (Top, _) | (_, Top) => BoolLattice[B].top
        case (Values(content1), Values(content2)) => content1.foldMap(s1 => content2.foldMap(s2 => BoolLattice[B].inject(s1 < s2)))
      }
    }
    val boolShow: Show[Boolean] = new Show[Boolean] {
      override def shows(b: Boolean): String = if (b) { "#t" } else { "#f" }
    }
    implicit val boolConcrete: BoolLattice[B] = new BaseInstance[Boolean]("Bool")(Order[Boolean], boolShow) with BoolLattice[B] {
      def inject(x: Boolean): B = Values(ISet.singleton(x))
      def isTrue(b: B): Boolean = b match {
        case Top => true
        case Values(content) => content.contains(true)
      }
      def isFalse(b: B): Boolean = b match {
        case Top => true
        case Values(content) => content.contains(false)
      }
      def not(b: B): B = b.map(x => !x)
    }

    implicit val intConcrete: IntLattice[I] = new BaseInstance[Int]("Int") with IntLattice[I] {
      def inject(x: Int): I = Values(ISet.singleton(x))
      def toFloat[F : FloatLattice](n: I): F = n match {
        case Top => FloatLattice[F].top
        case Values(content) => content.foldMap(n => FloatLattice[F].inject(n))
      }
      def random(n: I): I = n.map(n => SchemeOps.random(n))
      def plus(n1: I, n2: I): I = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 + n2)) }
      def minus(n1: I, n2: I): I = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 - n2)) }
      def times(n1: I, n2: I): I = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 * n2)) }
      def quotient(n1: I, n2: I): I = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 / n2)) }
      def div[F : FloatLattice](n1: I, n2: I): F = n2.guardBot { n1.foldMap(n1 => n2.foldMap(n2 => FloatLattice[F].inject(n1 / n2.toFloat))) }
      def modulo(n1: I, n2: I): I = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => SchemeOps.modulo(n1, n2))) }
      def remainder(n1: I, n2: I): I = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => SchemeOps.remainder(n1, n2))) }
      def lt[B : BoolLattice](n1: I, n2: I): B = n2.guardBot { n1.foldMap(n1 => n2.foldMap(n2 => BoolLattice[B].inject(n1 < n2))) }
      def toString[S : StringLattice](n: I): S = n.foldMap(n => StringLattice[S].inject(n.toString))
    }

    implicit val floatConcrete: FloatLattice[F] = new BaseInstance[Float]("Float") with FloatLattice[F] {
      def inject(x: Float): F = Values(ISet.singleton(x))
      def toInt[I : IntLattice](n: F): I = n.foldMap(n => IntLattice[I].inject(n.toInt))
      def ceiling(n: F): F = n.map(_.ceil)
      def floor(n: F): F = n.map(_.floor)
      def round(n: F): F = n.map(n => SchemeOps.round(n))
      def log(n: F): F = n.map(n => scala.math.log(n.toDouble).toFloat)
      def random(n: F): F = n.map(n => SchemeOps.random(n))
      def sin(n: F): F = n.map(n => scala.math.sin(n.toDouble).toFloat)
      def asin(n: F): F = n.map(n => scala.math.asin(n.toDouble).toFloat)
      def cos(n: F): F = n.map(n => scala.math.cos(n.toDouble).toFloat)
      def acos(n: F): F = n.map(n => scala.math.acos(n.toDouble).toFloat)
      def tan(n: F): F = n.map(n => scala.math.tan(n.toDouble).toFloat)
      def atan(n: F): F = n.map(n => scala.math.atan(n.toDouble).toFloat)
      def sqrt(n: F): F = n.map(n => scala.math.sqrt(n.toDouble).toFloat)
      def plus(n1: F, n2: F): F = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 + n2)) }
      def minus(n1: F, n2: F): F = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 - n2)) }
      def times(n1: F, n2: F): F = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 * n2)) }
      def div(n1: F, n2: F): F = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 / n2)) }
      def lt[B : BoolLattice](n1: F, n2: F): B = n2.guardBot { n1.foldMap(n1 => n2.foldMap(n2 => BoolLattice[B].inject(n1 < n2))) }
      def toString[S : StringLattice](n: F): S = n.foldMap(n => StringLattice[S].inject(n.toString))
    }
    val charShow: Show[Char] = new Show[Char] {
      override def shows(c: Char): String = s"#\\$c"
    }
    implicit val charConcrete: CharLattice[C] = new BaseInstance[Char]("Char")(Order[Char], charShow) with CharLattice[C] {
      def inject(x: Char): C = Values(ISet.singleton(x))
    }

    implicit val symConcrete: SymbolLattice[Sym] = new BaseInstance[String]("Sym") with SymbolLattice[Sym] {
      def inject(x: String): Sym = Values(ISet.singleton(x))
      def toString[S : StringLattice](s: Sym): S = s.foldMap(s => StringLattice[S].inject(s))
    }
  }
}

object ConcreteBooleanEfficient {
  sealed trait B
  case object Top extends B
  case object True extends B
  case object False extends B
  case object Bottom extends B

  object B {
    implicit val isBoolean: BoolLattice[B] = new BoolLattice[B] {
      def name = "ConcreteBoolean"
      private def showBool(b: Boolean) = if (b) "#t" else "#f"
      override def shows(x: B): String = x match {
        case Top => "{#t,#f}"
        case True => "#t"
        case False => "#f"
        case Bottom => "{}"
      }
      val bottom: B = Bottom
      val top: B = Top
      def join(x: B, y: => B) = x match {
        case Top => Top
        case Bottom => y
        case _ => y match {
          case Top => Top
          case Bottom => x
          case _ if (x == y) => x
          case _ => Top
        }
      }
      def subsumes(x: B, y: => B) = x match {
        case Top => true
        case True => y == Bottom || y == True
        case False => y == Bottom || y == False
        case Bottom => y == Bottom
      }

      def inject(x: Boolean): B = if (x) { True } else { False }
      def isTrue(b: B): Boolean = b == True || b == Top
      def isFalse(b: B): Boolean = b == False || b == Top
      def not(b: B): B = b match {
        case Top => Top
        case Bottom => Bottom
        case True => False
        case False => True
      }
      def eql[B1 : BoolLattice](b1: B, b2: B): B1 = (b1, b2) match {
        case (Bottom, _) | (_, Bottom) => BoolLattice[B1].bottom
        case (Top, _) | (_, Top) => BoolLattice[B1].top
        case _ => BoolLattice[B1].inject(b1 == b2)
      }

      def order(x: B, y: B): Ordering = (x, y) match {
        case (Top, Top) => Ordering.EQ
        case (Top, _) => Ordering.GT
        case (_, Top) => Ordering.LT
        case (True, True) => Ordering.EQ
        case (True, _) => Ordering.GT
        case (_, True) => Ordering.LT
        case (False, False) => Ordering.EQ
        case (False, _) => Ordering.GT
        case (_, False) => Ordering.LT
        case (Bottom, Bottom) => Ordering.EQ
      }
      def cardinality(x: B): Cardinality = CardinalityPrimitiveLikeNumber(x match {
        case Top => 2
        case Bottom => 0
        case _ => 1
      })
    }
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
  object I {
    implicit val isInteger = new IntLattice[I] {
      def name = s"BoundedInteger($bound)"
      override def shows(x: I): String = x match {
        case Set(xs) if xs.size == 1 => xs.elems.head.toString
        case Set(xs) => "{" + xs.elems.mkString(",") + "}"
        case Top => "Int"
      }
      val bottom: I = Monoid[I].zero
      val top: I = Top
      def join(x: I, y: => I) = Monoid[I].append(x, y)
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
      private def fold[L : LatticeElement](x: I, f: Int => L): L = x match {
        case Set(xs) => xs.foldMap(f)
        case Top => LatticeElement[L].top
      }
      private def foldI(x: I, f: Int => I) = x match {
        case Set(xs) => xs.foldMap(f)(isMonoid)
        case Top => Top
      }
      def inject(x: Int): I = promote(ISet.singleton(x))
      def toFloat[F : FloatLattice](n: I): F = fold(n, n => FloatLattice[F].inject(n))
      def random(n: I): I = Top
      def plus(n1: I, n2: I): I = foldI(n1, n1 => foldI(n2, n2 => inject(n1 + n2)))
      def minus(n1: I, n2: I): I = foldI(n1, n1 => foldI(n2, n2 => inject(n1 - n2)))
      def times(n1: I, n2: I): I = foldI(n1, n1 => foldI(n2, n2 => inject(n1 * n2)))
      def div[F : FloatLattice](n1: I, n2: I): F = fold(n1, n1 => fold(n2, n2 => FloatLattice[F].inject(n1 / n2.toFloat)))
      def quotient(n1: I, n2: I): I = foldI(n1, n1 => foldI(n2, n2 => inject(n1 / n2)))
      def modulo(n1: I, n2: I): I = foldI(n1, n1 => foldI(n2, n2 => inject(SchemeOps.modulo(n1, n2))))
      def remainder(n1: I, n2: I): I = foldI(n1, n1 => foldI(n2, n2 => inject(SchemeOps.remainder(n1, n2))))
      def lt[B : BoolLattice](n1: I, n2: I): B = fold(n1, n1 => fold(n2, n2 => BoolLattice[B].inject(n1 < n2)))
      def eql[B : BoolLattice](n1: I, n2: I): B = fold(n1, n1 => fold(n2, n2 => BoolLattice[B].inject(n1 == n2)))
      def toString[S : StringLattice](n: I): S = fold(n, n => StringLattice[S].inject(n.toString))

      def order(x: I, y: I): Ordering = (x, y) match {
        case (Set(xs), Set(ys)) => Order[ISet[Int]].order(xs, ys)
        case (Top, _: Set) => Ordering.GT
        case (_: Set, Top) => Ordering.LT
        case (Top, Top) => Ordering.EQ
      }
      def cardinality(x: I): Cardinality = x match {
        case Set(xs) => CardinalityPrimitiveLikeNumber(xs.toList.length)
        case Top => CardinalityPrimitiveLikeInf()
      }
    }
  }
}

object Type {
  sealed trait T
  case object Top extends T
  case object Bottom extends T

  type S = T
  type B = T
  type I = T
  type F = T
  type C = T
  type Sym = T

  object T {
    implicit val typeIsMonoid: Monoid[T] = new Monoid[T] {
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
      def eql[B : BoolLattice](n1: T, n2: T): B = (n1, n2) match {
        case (Top, Top) => BoolLattice[B].top
        case _ => BoolLattice[B].bottom
      }
      // def order(x: T, y: T): Ordering = implicitly[Order[T]].order(x, y)
      def order(x: T, y: T): Ordering = (x, y) match {
        case (Top, Top) => Ordering.EQ
        case (Top, Bottom) => Ordering.GT
        case (Bottom, Top) => Ordering.LT
        case (Bottom, Bottom) => Ordering.EQ
      }
      def cardinality(x: T): Cardinality = x match {
        case Top => CardinalityInf
        case Bottom => CardinalityPrimitiveLikeNumber(0)
      }
    }
    /* TODO: not needed?  implicit val typeIsLatticeElement: LatticeElement[T] = new BaseInstance("Type") {} */
    implicit val typeIsString: StringLattice[S] = new BaseInstance("Str") with StringLattice[S] {
      def inject(x: String): T = Top
      def length[I : IntLattice](s: T) = s match {
        case Top => IntLattice[I].top
        case Bottom => IntLattice[I].bottom
      }
      def append(s1: T, s2: T) = (s1, s2) match {
        case (Bottom, _) | (_, Bottom) => Bottom
        case (Top, _) | (Top, _) => Top
      }
      def lt[B : BoolLattice](s1: T, s2: T) = (s1, s2) match {
        case (Bottom, _) | (_, Bottom) => BoolLattice[B].bottom
        case (Top, _) | (Top, _) => BoolLattice[B].top
      }
    }
    implicit val typeIsBoolean: BoolLattice[B] = new BaseInstance("Bool") with BoolLattice[B] {
      def inject(x: Boolean): T = Top
      def isTrue(b: T) = b == Top
      def isFalse(b: T) = b == Top
      def not(b: T) = b
      /* TODO: we could redefine cardinality for booleans, as they have max. two
       * values. But it makes sense to keep it as inf, because if it's top we have
       * lost all precision on that boolean */
      /*
       override def cardinality(x: T): Cardinality = x match {
       case Top => CardinalityPrimitiveLikeNumber(2)
       case Bottom => CardinalityPrimitiveLikeNumber(0)
       }*/
    }
    implicit val typeIsInteger: IntLattice[I] = new BaseInstance("Int") with IntLattice[I] {
      def inject(x: Int): T = Top
      def toFloat[F : FloatLattice](n: T): F = n match {
        case Top => FloatLattice[F].top
        case Bottom => FloatLattice[F].bottom
      }
      def random(n: T): T = n
      def plus(n1: T, n2: T): T = meet(n1, n2)
      def minus(n1: T, n2: T): T = meet(n1, n2)
      def times(n1: T, n2: T): T = meet(n1, n2)
      def div[F : FloatLattice](n1: T, n2: T): F = (n1, n2) match {
        case(Top, Top) => FloatLattice[F].top
        case _ => FloatLattice[F].bottom
      }
      def quotient(n1: T, n2: T): T = meet(n1, n2)
      def modulo(n1: T, n2: T): T = meet(n1, n2)
      def remainder(n1: T, n2: T): T = meet(n1, n2)
      def lt[B : BoolLattice](n1: T, n2: T): B = (n1, n2) match {
        case (Top, Top) => BoolLattice[B].top
        case _ => BoolLattice[B].bottom
      }
      def toString[S : StringLattice](n: T): S = n match {
        case Top => StringLattice[S].top
        case Bottom => StringLattice[S].bottom
      }
    }
    implicit val typeIsFloat: FloatLattice[F] = new BaseInstance("Float") with FloatLattice[F] {
      def inject(x: Float): T = Top
      def toInt[I : IntLattice](n: T): I = n match {
        case Top => IntLattice[I].top
        case Bottom => IntLattice[I].bottom
      }
      def ceiling(n: T): T = n
      def floor(n: T): T = n
      def round(n: T): T = n
      def log(n: T): T = n
      def random(n: T): T = n
      def sin(n: T): T = n
      def asin(n: T): T = n
      def cos(n: T): T = n
      def acos(n: T): T = n
      def tan(n: T): T = n
      def atan(n: T): T = n
      def sqrt(n: T): T = n
      def plus(n1: T, n2: T): T = meet(n1, n2)
      def minus(n1: T, n2: T): T = meet(n1, n2)
      def times(n1: T, n2: T): T = meet(n1, n2)
      def div(n1: T, n2: T): T = meet(n1, n2)
      def lt[B : BoolLattice](n1: T, n2: T): B = (n1, n2) match {
        case (Top, Top) => BoolLattice[B].top
        case _ => BoolLattice[B].bottom
      }
      def toString[S : StringLattice](n: T): S = n match {
        case Top => StringLattice[S].top
        case Bottom => StringLattice[S].bottom
      }
    }
    implicit val typeIsChar: CharLattice[C] = new BaseInstance("Char") with CharLattice[C] {
      def inject(c: Char): T = Top
    }
    implicit val typeIsSymbol: SymbolLattice[Sym] = new BaseInstance("Sym") with SymbolLattice[Sym] {
      def inject(sym: String): T = Top
      def toString[S : StringLattice](s: T): S = StringLattice[S].top
    }
  }
}

object ConstantPropagation {
  sealed trait L[+A]
  case object Top extends L[Nothing]
  case class Constant[A](x: A) extends L[A]
  case object Bottom extends L[Nothing]

  type S = L[String]
  type I = L[Int]
  type F = L[Float]
  type C = L[Char]
  type Sym = L[String]

  object L {
    implicit def constantIsMonoid[A]: Monoid[L[A]] = new Monoid[L[A]] {
      def zero: L[A] = Bottom
      def append(x: L[A], y: => L[A]): L[A] = x match {
        case Top => Top
        case Constant(_) => y match {
          case Top => Top
          case Constant(_) => if (x == y) { x } else { Top }
          case Bottom => x
        }
        case Bottom => y
      }
    }

    abstract class BaseInstance[A : Order](typeName: String) extends LatticeElement[L[A]] {
      def name = s"ConstantPropagation$typeName"
      override def shows(x: L[A]): String = x match {
        case Top => typeName
        case Constant(x) => x.toString
        case Bottom => "⊥"
      }
      val bottom: L[A] = Bottom
      val top: L[A] = Top
      def join(x: L[A], y: => L[A]) = Monoid[L[A]].append(x, y)
      def meet(x: L[A], y: => L[A]): L[A] = x match {
        case Bottom => Bottom
        case Constant(_) => y match {
          case Top => x
          case Constant(_) => if (x == y) { x } else { Bottom }
          case Bottom => Bottom
        }
        case Top => y
      }
      def subsumes(x: L[A], y: => L[A]) = x match {
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
      def eql[B : BoolLattice](n1: L[A], n2: L[A]): B = (n1, n2) match {
        case (Top, Top) => BoolLattice[B].top
        case (Top, Constant(_)) => BoolLattice[B].top
        case (Constant(_), Top) => BoolLattice[B].top
        case (Constant(x), Constant(y)) => BoolLattice[B].inject(x == y)
        case (Bottom, _) => BoolLattice[B].bottom
        case (_, Bottom) => BoolLattice[B].bottom
      }
      def order(x: L[A], y: L[A]): Ordering = (x, y) match {
        case (Top, Top) => Ordering.EQ
        case (Top, _) => Ordering.GT
        case (Constant(_), Top) => Ordering.LT
        case (Constant(x), Constant(y)) => Order[A].order(x, y)
        case (Constant(_), Bottom) => Ordering.GT
        case (Bottom, Bottom) => Ordering.EQ
        case (Bottom, _) => Ordering.LT
      }
      def cardinality(x: L[A]): Cardinality = x match {
        case Top => CardinalityInf
        case Constant(_) => CardinalityPrimitiveLikeNumber(1)
        case Bottom => CardinalityPrimitiveLikeNumber(0)
      }
    }

    implicit val stringCP: StringLattice[S] = new BaseInstance[String]("Str") with StringLattice[S] {
      def inject(x: String): S = Constant(x)
      def length[I : IntLattice](s: S) = s match {
        case Top => IntLattice[I].top
        case Constant(s) => IntLattice[I].inject(s.size)
        case Bottom => IntLattice[I].bottom
      }
      def append(s1: S, s2: S) = (s1, s2) match {
        case (Bottom, _) | (_, Bottom) => Bottom
        case (Top, _) | (_, Top) => Top
        case (Constant(x), Constant(y)) => Constant(x ++ y)
      }
      def lt[B : BoolLattice](s1: S, s2: S) = (s1, s2) match {
        case (Bottom, _) | (_, Bottom) => BoolLattice[B].bottom
        case (Top, _) | (_, Top) => BoolLattice[B].top
        case (Constant(x), Constant(y)) => BoolLattice[B].inject(x < y)
      }
    }
    implicit val intCP: IntLattice[I] = new BaseInstance[Int]("Int") with IntLattice[I] {
      def inject(x: Int): I = Constant(x)
      def toFloat[F : FloatLattice](n: I): F = n match {
        case Top => FloatLattice[F].top
        case Constant(x) => FloatLattice[F].inject(x)
        case Bottom => FloatLattice[F].bottom
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
      def div[F : FloatLattice](n1: I, n2: I): F = (n1, n2) match {
        case (Top, _) | (_, Top) => FloatLattice[F].top
        case (Constant(x), Constant(y)) => FloatLattice[F].inject(x / y.toFloat)
        case _ => FloatLattice[F].bottom
      }
      def quotient(n1: I, n2: I): I = binop(_ / _, n1, n2)
      def modulo(n1: I, n2: I): I = binop(SchemeOps.modulo _, n1, n2)
      def remainder(n1: I, n2: I): I = binop(SchemeOps.remainder _, n1, n2)
      def lt[B : BoolLattice](n1: I, n2: I): B = (n1, n2) match {
        case (Top, Top) => BoolLattice[B].top
        case (Top, Constant(_)) => BoolLattice[B].top
        case (Constant(_), Top) => BoolLattice[B].top
        case (Constant(x), Constant(y)) => BoolLattice[B].inject(x < y)
        case _ => BoolLattice[B].bottom
      }
      def toString[S : StringLattice](n: I): S = n match {
        case Top => StringLattice[S].top
        case Constant(x) => StringLattice[S].inject(x.toString)
        case Bottom => StringLattice[S].bottom
      }
    }
    implicit val floatCP: FloatLattice[F] = new BaseInstance[Float]("Float") with FloatLattice[F] {
      def inject(x: Float) = Constant(x)
      def toInt[I : IntLattice](n: F): I = n match {
        case Top => IntLattice[I].top
        case Constant(x) => IntLattice[I].inject(x.toInt)
        case Bottom => IntLattice[I].bottom
      }
      def ceiling(n: F): F = n match {
        case Constant(x) => Constant(x.ceil)
        case _ => n
      }
      def floor(n: F): F = n match {
        case Constant(x) => Constant(x.floor)
        case _ => n
      }
      def round(n: F): F = n match {
        case Constant(x) => Constant(SchemeOps.round(x))
        case _ => n
      }
      def random(n: F): F = n match {
        case Constant(x) => Constant(SchemeOps.random(x))
        case _ => n
      }
      def log(n: F): F = n match {
        case Constant(x) => Constant(scala.math.log(x.toDouble).toFloat)
        case _ => n
      }
      def sin(n: F): F = n match {
        case Constant(x) => Constant(scala.math.sin(x.toDouble).toFloat)
        case _ => n
      }
      def asin(n: F): F = n match {
        case Constant(x) => Constant(scala.math.asin(x.toDouble).toFloat)
        case _ => n
      }
      def cos(n: F): F = n match {
        case Constant(x) => Constant(scala.math.cos(x.toDouble).toFloat)
        case _ => n
      }
      def acos(n: F): F = n match {
        case Constant(x) => Constant(scala.math.acos(x.toDouble).toFloat)
        case _ => n
      }
      def tan(n: F): F = n match {
        case Constant(x) => Constant(scala.math.tan(x.toDouble).toFloat)
        case _ => n
      }
      def atan(n: F): F = n match {
        case Constant(x) => Constant(scala.math.atan(x.toDouble).toFloat)
        case _ => n
      }
      def sqrt(n: F): F = n match {
        case Constant(x) => Constant(scala.math.sqrt(x.toDouble).toFloat)
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
      def lt[B : BoolLattice](n1: F, n2: F): B = (n1, n2) match {
        case (Top, Top) => BoolLattice[B].top
        case (Top, Constant(_)) => BoolLattice[B].top
        case (Constant(_), Top) => BoolLattice[B].top
        case (Constant(x), Constant(y)) => BoolLattice[B].inject(x < y)
        case _ => BoolLattice[B].bottom
      }
      def toString[S : StringLattice](n: F): S = n match {
        case Top => StringLattice[S].top
        case Constant(x) => StringLattice[S].inject(x.toString)
        case Bottom => StringLattice[S].bottom
      }
    }
    implicit val charCP: CharLattice[C] = new BaseInstance[Char]("Char") with CharLattice[C] {
      def inject(x: Char) = Constant(x)
    }
    implicit val symCP: SymbolLattice[Sym] = new BaseInstance[String]("Symbol") with SymbolLattice[Sym] {
      def inject(x: String) = Constant(x)
      def toString[S : StringLattice](s: Sym): S = s match {
        case Top => StringLattice[S].top
        case Constant(x) => StringLattice[S].inject(x)
        case Bottom => StringLattice[S].bottom
      }
    }
  }
}
