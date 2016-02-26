import scalaz._
import scalaz.Scalaz._

object Concrete {
  type S = ISet[String]
  implicit val isString = new IsString[S] {
    def name = "ConcreteString"
    private def showString(s: String) = "\"" + s + "\""
    override def shows(x: S): String = if (x.size == 1) { showString(x.elems.head) } else { "{" + x.toList.map(showString _).mkString(",") + "}" }
    def bot: S = ISet.empty
    def join(x: S, y: S) = x.union(y)
    def subsumes(x: S, y: S) = y.isSubsetOf(x)

    def inject(x: String): S = ISet.singleton(x)
    def length[I](s: S)(implicit int: IsInteger[I]): I = s.foldMap(s => int.inject(s.size))
    def eql[B](s1: S, s2: S)(implicit bool: IsBoolean[B]): B = s1.foldMap(s1 => s2.foldMap(s2 => bool.inject(s1 == s2)))

    def order(x: S, y: S): Ordering = implicitly[Order[ISet[String]]].order(x, y)
  }

  type B = ISet[Boolean]
  implicit val isBoolean = new IsBoolean[B] {
    def name = "ConcreteBoolean"
    private def showBool(b: Boolean) = if (b) "#t" else "#f"
    override def shows(x: B): String = if (x.size == 1) { showBool(x.elems.head) } else { "{" + x.elems.map(showBool _).mkString(",") + "}" }
    def bot: B = ISet.empty
    def join(x: B, y: B) = x.union(y)
    def subsumes(x: B, y: B) = y.isSubsetOf(x)

    def inject(x: Boolean): B = ISet.singleton(x)
    def isTrue(b: B): Boolean = b.contains(true)
    def isFalse(b: B): Boolean = b.contains(false)
    def not(b: B): B = b.map(x => !x)
    def eql(b1: B, b2: B): B = b1.foldMap(b1 => b2.map(b2 => b1 == b2))

    def order(x: B, y: B): Ordering = implicitly[Order[ISet[Boolean]]].order(x, y)
  }

  type I = ISet[Int]
  implicit val isInteger = new IsInteger[I] {
    def name = "ConcreteInteger"
    override def shows(x: I): String = if (x.size == 1) { x.elems.head.toString } else { "{" + x.elems.mkString(",") + "}" }
    def bot: I = ISet.empty
    def join(x: I, y: I) = x.union(y)
    def subsumes(x: I, y: I) = y.isSubsetOf(x)

    def inject(x: Int): I = ISet.singleton(x)
    def ceiling(n: I): I = n
    def toFloat[F](n: I)(implicit float: IsFloat[F]): F = n.foldMap(n => float.inject(n))
    def random(n: I): I = n.map(n => scala.util.Random.nextInt % n)
    def plus(n1: I, n2: I): I = n1.foldMap(n1 => n2.map(n2 => n1 + n2))
    def minus(n1: I, n2: I): I = n1.foldMap(n1 => n2.map(n2 => n1 - n2))
    def times(n1: I, n2: I): I = n1.foldMap(n1 => n2.map(n2 => n1 * n2))
    def div(n1: I, n2: I): I = n1.foldMap(n1 => n2.map(n2 => n1 / n2))
    def modulo(n1: I, n2: I): I = n1.foldMap(n1 => n2.map(n2 => if (n1 * n2 < 0) {
      /* different sign, behaviour not the same between Scheme and Scala, adjust it */
      (Math.abs(n2) - Math.abs(n1) % Math.abs(n2)) % Math.abs(n2) * (if (n2 < 0) -1 else 1)
    } else {
      /* same sign, same behaviour */
      n1 % n2
    }))
    def lt[B](n1: I, n2: I)(implicit bool: IsBoolean[B]): B = n1.foldMap(n1 => n2.foldMap(n2 => bool.inject(n1 < n2)))
    def eql[B](n1: I, n2: I)(implicit bool: IsBoolean[B]): B = n1.foldMap(n1 => n2.foldMap(n2 => bool.inject(n1 == n2)))

    def order(x: I, y: I): Ordering = implicitly[Order[ISet[Int]]].order(x, y)
  }

  type F = ISet[Float]
  implicit val isFloat = new IsFloat[F] {
    def name = "ConcreteFloat"
    override def shows(x: F): String = if (x.size == 1) { x.elems.head.toString } else { "{" + x.elems.mkString(",") + "}" }
    def bot: F = ISet.empty
    def join(x: F, y: F) = x.union(y)
    def subsumes(x: F, y: F) = y.isSubsetOf(x)

    def inject(x: Float): F = ISet.singleton(x)
    def ceiling(n: F): F = n.map(_.ceil)
    def log(n: F): F = n.map(n => scala.math.log(n.toDouble).toFloat)
    def random(n: F): F = n.map(n => scala.util.Random.nextFloat % n)
    def plus(n1: F, n2: F): F = n1.foldMap(n1 => n2.map(n2 => n1 + n2))
    def minus(n1: F, n2: F): F = n1.foldMap(n1 => n2.map(n2 => n1 - n2))
    def times(n1: F, n2: F): F = n1.foldMap(n1 => n2.map(n2 => n1 * n2))
    def div(n1: F, n2: F): F = n1.foldMap(n1 => n2.map(n2 => n1 / n2))
    def lt[B](n1: F, n2: F)(implicit bool: IsBoolean[B]): B = n1.foldMap(n1 => n2.foldMap(n2 => bool.inject(n1 < n2)))
    def eql[B](n1: F, n2: F)(implicit bool: IsBoolean[B]): B = n1.foldMap(n1 => n2.foldMap(n2 => bool.inject(n1 == n2)))

    def order(x: F, y: F): Ordering = implicitly[Order[ISet[Float]]].order(x, y)
  }

  type C = ISet[Char]
  implicit val isChar = new IsChar[C] {
    def name = "ConcreteChar"
    private def showChar(c: Char) = s"#\\$c"
    override def shows(x: C): String = if (x.size == 1) { showChar(x.elems.head) } else { "{" + x.elems.map(showChar _).mkString(",") + "}" }

    def bot: C = ISet.empty
    def join(x: C, y: C) = x.union(y)
    def subsumes(x: C, y: C) = y.isSubsetOf(x)

    def inject(x: Char): C = ISet.singleton(x)
    def eql[B](c1: C, c2: C)(implicit bool: IsBoolean[B]): B = c1.foldMap(c1 => c2.foldMap(c2 => bool.inject(c1 == c2)))

    def order(x: C, y: C): Ordering = implicitly[Order[ISet[Char]]].order(x, y)
  }

  sealed trait Symbol
  type Sym = ISet[String @@ Symbol]
  implicit val symOrder = new Order[String @@ Symbol] {
    def order(x: String @@ Symbol, y: String @@ Symbol) = implicitly[Order[String]].order(Tag.unwrap(x), Tag.unwrap(y))
  }
  implicit val isSymbol = new IsSymbol[Sym] {
    def name = "ConcreteSymbol"
    def showSym(x: String @@ Symbol): String = Tag.unwrap(x)
    override def shows(x: Sym): String = if (x.size == 1) { showSym(x.elems.head) } else { "{" + x.elems.map(showSym _).mkString(",") + "}" }

    def bot: Sym = ISet.empty
    def join(x: Sym, y: Sym) = x.union(y)
    def subsumes(x: Sym, y: Sym) = y.isSubsetOf(x)

    def inject(x: String): Sym = ISet.singleton(Tag[String, Symbol](x))
    def eql[B](s1: Sym, s2: Sym)(implicit bool: IsBoolean[B]): B = s1.foldMap(s1 => s2.foldMap(s2 => bool.inject(s1 == s2)))

    def order(x: Sym, y: Sym): Ordering = implicitly[Order[ISet[String @@ Symbol]]].order(x, y)
  }
}

object ConcreteLatticeNew extends Lattice {
  import Concrete._
  val lattice = new MakeLattice[S, B, I, F, C, Sym]
  type L = lattice.LSet
  implicit val isAbstractValue: AbstractValue[L] = lattice.isAbstractValueSet
}
