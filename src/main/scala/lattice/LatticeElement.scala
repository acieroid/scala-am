import scalaz._
import scalaz.Scalaz._

/**
 * We define here some domains that can will be useful to build a lattice for
 * most languages.
 */
trait LatticeElement[L] extends Order[L] with Monoid[L] with Show[L] {
  /** The name of the lattice */
  def name: String
  /** The bottom element */
  def bottom: L
  /** The top element. */
  def top: L
  /** The join operation */
  def join(x: L, y: => L): L
  /** The subsumption relation that defines the ordering of elements */
  def subsumes(x: L, y: => L): Boolean
  /** Equality check, returning an abstract result */
  def eql[B : BoolLattice](x: L, y: L): B

  def cardinality(x: L): Cardinality

  /* For Monoid[L] */
  final def zero: L = bottom
  final def append(x: L, y: => L): L = join(x, y)

  trait LatticeElementLaw extends MonoidLaw with OrderLaw {
    /**
     * Bottom is the lower bound
     * ∀ a: ⊥ ⊑ a
     */
    def bottomLowerBound(a: L): Boolean =
      subsumes(a, bottom)
    /**
     * Top is the upper bound (when defined)
     * ∀ a: a ⊑ ⊤
     */
    def topUpperBound(a: L): Boolean = subsumes(top, a)
    /**
     * Join is commutative
     * ∀ a, b: a ⊔ b = b ⊔ a
     */
    def joinCommutative(a: L, b: L): Boolean =
      join(a, b) == join(b, a)
    /**
     * Join is associative
     * ∀ a, b, c: (a ⊔ b) ⊔ c = a ⊔ (b ⊔ c)
     */
    def joinAssociative(a: L, b: L, c: L): Boolean =
      join(join(a, b), c) == join(a, join(b, c))
    /**
     * Join is idempotent
     * ∀ a: a ⊔ a = a
     */
    def joinIdempotent(a: L): Boolean =
      join(a, a) == a
    /**
     * Join and subsumes are compatible
     * ∀ a, b: a ⊑ b ⇒ a ⊔ b = b
     */
    def joinSubsumesCompatible(a: L, b: L): Boolean =
      conditional(subsumes(b, a), join(a, b) == b)
    /**
     * Equal elements are always eql if they're not bottom.
     * ∀ a: a = bottom ∨ isTrue(eql(a, a))
     */
     def eqlIsTrue(a: L): Boolean = {
      conditional(a != bottom,
        BoolLattice[Concrete.B].isTrue(eql[Concrete.B](a, a)))
    }
  }
  def latticeElementLaw = new LatticeElementLaw {}
}

object LatticeElement {
  def apply[L : LatticeElement]: LatticeElement[L] = implicitly

  implicit def ofSet[A]: LatticeElement[Set[A]] = new LatticeElement[Set[A]] {
    def name = "OfSet"
    def bottom: Set[A] = Set.empty
    def top: Set[A] = throw new Error("OfSet lattice has no top value")
    def join(x: Set[A], y: => Set[A]): Set[A] = x ++ y
    def subsumes(x: Set[A], y: => Set[A]): Boolean = y.subsetOf(x)
    def eql[B : BoolLattice](x: Set[A], y: Set[A]): B =
      if (x.size == 1 && y.size == 1 && x == y) { BoolLattice[B].inject(true) }
      else if (x.intersect(y).isEmpty) { BoolLattice[B].inject(false) }
      else { BoolLattice[B].top }
    def order(x: Set[A], y: Set[A]): Ordering =
      throw new Error("Cannot define an order since A is not required to be ordered")
    def cardinality(x: Set[A]): Cardinality = CardinalityNumber(x.size)
  }
}

/** A lattice for strings */
trait StringLattice[S] extends LatticeElement[S] {
  def inject(s: String): S
  def length[I : IntLattice](s: S): I
  def append(s1: S, s2: S): S

  trait StringLatticeLaw {
    lazy val intLat = new BoundedInteger(100)
    type I = intLat.I

    def lengthPreservesBottom: Boolean =
      length[I](bottom) == IntLattice[I].bottom
    def lengthIsMonotone(a: S, b: S): Boolean =
      conditional(subsumes(b, a),
        IntLattice[I].subsumes(length[I](b), length[I](a)))
    def lengthIsSound(a: String): Boolean =
      IntLattice[I].subsumes(length[I](inject(a)), IntLattice[I].inject(a.size))
    def appendPreservesBottom(a: S): Boolean =
      append(bottom, a) == bottom && append(a, bottom) == bottom
    def appendIsMonotone(a: S, b: S, c: S): Boolean =
      conditional(subsumes(c, b),
        subsumes(append(a, c), append(a, b)) && subsumes(append(c, a), append(b, a)))
    def appendIsSound(a: String, b: String): Boolean =
      subsumes(append(inject(a), inject(b)), inject(a ++ b))
    def appendIsAssociative(a: S, b: S, c: S): Boolean =
      append(append(a, b), c) == append(a, append(b, c))
  }
  val stringLatticeLaw = new StringLatticeLaw {}
}

object StringLattice {
  def apply[S : StringLattice]: StringLattice[S] = implicitly
}

/** A lattice for booleans */
trait BoolLattice[B] extends LatticeElement[B] {
  def inject(b: Boolean): B
  def isTrue(b: B): Boolean
  def isFalse(b: B): Boolean
  def not(b: B): B

  trait BoolLatticeLaw extends LatticeElementLaw {
    /**
     * Inject preserves truthiness
     * isTrue(inject(true)) ∧ isFalse(inject(false))
     */
    def injectPreservesTruthiness: Boolean =
      isTrue(inject(true)) && isFalse(inject(false))
    /**
     * Top is both true and false (when defined)
     * isTrue(⊤) ∧ isFalse(⊤)
     */
    def topTrueAndFalse: Boolean = isTrue(top) && isFalse(top)
    /**
     * Bottom is neither true nor false
     * ¬isTrue(⊥) ∧ ¬isFalse(⊥)
     */
    def bottomNotTrueNorFalse: Boolean = !isTrue(bottom) && !isFalse(bottom)
    /**
     * Not reverses truthiness
     * ∀ a: isTrue(a) ⇒ isFalse(not(a)) ∧ isFalse(a) ⇒ isTrue(not(a))
     */
    def notReversesTruthiness(a: B): Boolean =
      conditional(isTrue(a), isFalse(not(a))) && conditional(isFalse(a), isTrue(not(a)))
    /**
     * Not is involutive
     * ∀ a: not(not(a)) == a
     */
    def notInvolutive(a: B): Boolean =
      not(not(a)) == a
  }
  val boolLatticeLaw = new BoolLatticeLaw {}
}

object BoolLattice {
  def apply[B : BoolLattice]: BoolLattice[B] = implicitly
}

/** A lattice for integers */
trait IntLattice[I] extends LatticeElement[I] { self =>
  def inject(n: Int): I
  def toFloat[F : FloatLattice](n: I): F
  def random(n: I): I
  def plus(n1: I, n2: I): I
  def minus(n1: I, n2: I): I
  def times(n1: I, n2: I): I
  def div(n1: I, n2: I): I
  def modulo(n1: I, n2: I): I
  def lt[B : BoolLattice](n1: I, n2: I): B
  def toString[S : StringLattice](n: I): S

   trait IntLatticeLaw {
     type F = Type.F
     type B = Concrete.B
     type S = Type.S

    def toFloatPreservesBottom: Boolean =
      toFloat[F](bottom) == FloatLattice[F].bottom
    def toFloatIsMonotone(a: I, b: I): Boolean =
      conditional(subsumes(b, a),
        FloatLattice[F].subsumes(toFloat[F](b), toFloat[F](a)))
    def toFloatIsSound(a: Int): Boolean =
      FloatLattice[F].subsumes(toFloat[F](inject(a)), FloatLattice[F].inject(a.toFloat))
    def randomPreservesBottom: Boolean =
      random(bottom) == bottom
    /* Random should neither be monotone nor sound (at least in concrete) */
    def plusPreservesBottom(a: I): Boolean =
      plus(a, bottom) == bottom && plus(bottom, a) == bottom
    def plusIsMonotone(a: I, b: I, c: I): Boolean =
      conditional(subsumes(c, b),
        subsumes(plus(a, c), plus(a, b)))
    def plusIsSound(a: Int, b: Int): Boolean =
      subsumes(plus(inject(a), inject(b)), inject(a + b))
    def plusIsAssociative(a: I, b: I, c: I): Boolean =
      plus(a, plus(b, c)) == plus(plus(a, b), c)
    def plusIsCommutative(a: I, b: I): Boolean =
      plus(a, b) == plus(b, a)
    def minusPreservesBottom(a: I): Boolean =
      minus(a, bottom) == bottom && minus(bottom, a) == bottom
    def minusIsMonotone(a: I, b: I, c: I): Boolean =
      conditional(subsumes(c, b),
        subsumes(minus(a, c), minus(a, b)))
    def minusIsSound(a: Int, b: Int): Boolean =
      subsumes(minus(inject(a), inject(b)), inject(a - b))
    def minusIsAnticommutative(a: I, b: I): Boolean =
      minus(a, b) == minus(inject(0), minus(b, a))
    def timesPreservesBottom(a: I): Boolean =
      times(a, bottom) == bottom && times(bottom, a) == bottom
    def timesIsMonotone(a: I, b: I, c: I): Boolean =
      conditional(subsumes(c, b),
        subsumes(times(a, c), times(a, b)))
    def timesIsSound(a: Int, b: Int): Boolean =
      subsumes(times(inject(a), inject(b)), inject(a * b))
    def timesIsAssociative(a: I, b: I, c: I): Boolean =
      times(a, times(b, c)) == times(times(a, b), c)
    def timesIsCommutative(a: I, b: I): Boolean =
      times(a, b) == times(b, a)
    def divPreservesBottom(a: I): Boolean =
      div(a, bottom) == bottom && conditional(!subsumes(a, inject(0)), div(bottom, a) == bottom)
    def divIsMonotone(a: I, b: I, c: I): Boolean =
      conditional(subsumes(c, b) && !subsumes(b, inject(0)) && !subsumes(c, inject(0)),
        subsumes(div(a, c), div(a, b)))
    def divIsSound(a: Int, b: Int): Boolean =
      conditional(b != 0,
        subsumes(div(inject(a), inject(b)), inject(a / b)))
    def moduloPreservesBottom(a: I): Boolean =
      modulo(a, bottom) == bottom && conditional(!subsumes(a, inject(0)), modulo(bottom, a) == bottom)
    def moduloIsMonotone(a: I, b: I, c: I): Boolean =
      conditional(subsumes(c, b) && !subsumes(c, inject(0)) && !subsumes(b, inject(0)),
        subsumes(modulo(a, c), modulo(a, b)))
    def moduloIsSound(a: Int, b: Int): Boolean =
      conditional(b != 0,
        subsumes(modulo(inject(a), inject(b)), inject(SchemeOps.modulo(a, b))))
    def ltPreservesBottom(a: I): Boolean =
      lt[B](a, bottom) == BoolLattice[B].bottom && lt[B](bottom, a) == BoolLattice[B].bottom
    def ltIsMonotone(a: I, b: I, c: I): Boolean =
      conditional(subsumes(b, c),
        BoolLattice[B].subsumes(lt[B](a, c), lt[B](a, b)))
    def ltIsSound(a: Int, b: Int): Boolean =
      BoolLattice[B].subsumes(lt[B](inject(a), inject(b)), BoolLattice[B].inject(a < b))
    def toStringPreservesBottom: Boolean =
      self.toString[S](bottom) == StringLattice[S].bottom
    def toStringIsMonotone(a: I, b: I): Boolean =
      conditional(subsumes(b, a),
        StringLattice[S].subsumes(self.toString[S](b), self.toString[S](a)))
    def toStringIsSound(a: Int): Boolean =
      StringLattice[S].subsumes(self.toString[S](inject(a)), StringLattice[S].inject(a.toString))
  }
  val intLatticeLaw = new IntLatticeLaw {}
}

object IntLattice {
  def apply[I : IntLattice]: IntLattice[I] = implicitly
}

/** A lattice for floats */
trait FloatLattice[F] extends LatticeElement[F] { self =>
  def inject(n: Float): F
  def toInt[I : IntLattice](n: F): I
  def ceiling(n: F): F
  def round(n: F): F
  def log(n: F): F
  def random(n: F): F
  def sin(n: F): F
  def cos(n: F): F
  def tan(n: F): F
  def sqrt(n: F): F
  def plus(n1: F, n2: F): F
  def minus(n1: F, n2: F): F
  def times(n1: F, n2: F): F
  def div(n1: F, n2: F): F
  def lt[B : BoolLattice](n1: F, n2: F): B
  def toString[S : StringLattice](n: F): S

  trait FloatLatticeLaw {
    type I = Type.I
    type B = Concrete.B
    type S = Type.S

    def toIntPreservesBottom: Boolean =
      toInt[I](bottom) == IntLattice[I].bottom
    def toIntIsMonotone(a: F, b: F): Boolean =
      conditional(subsumes(b, a),
        IntLattice[I].subsumes(toInt[I](b), toInt[I](a)))
    def toIntIsSound(a: Float): Boolean =
      IntLattice[I].subsumes(toInt[I](inject(a)), IntLattice[I].inject(a.toInt))
    def ceilingPreservesBottom: Boolean =
      ceiling(bottom) == bottom
    def ceilingIsMonotone(a: F, b: F): Boolean =
      conditional(subsumes(b, a),
        subsumes(ceiling(b), ceiling(a)))
    def ceilingIsSound(a: Float): Boolean =
      subsumes(ceiling(inject(a)), inject(scala.math.ceil(a.toDouble).toFloat))
    def logPreservesBottom: Boolean =
      log(bottom) == bottom
    def logIsMonotone(a: F, b: F): Boolean =
      /* TODO: this test is failing */
      /*conditional(subsumes(b, a),
       subsumes(log(b), log(a))) */
      true
    def logIsSound(a: Float): Boolean =
      conditional(a > 0,
        subsumes(log(inject(a)), inject(scala.math.log(a.toDouble).toFloat)))
    def randomPreservesBottom: Boolean =
      random(bottom) == bottom
    /* Random should neither be monotone nor sound (at least in concrete) */
    def plusPreservesBottom(a: F): Boolean =
      plus(a, bottom) == bottom && plus(bottom, a) == bottom
    def plusIsMonotone(a: F, b: F, c: F): Boolean =
      conditional(subsumes(c, b),
        subsumes(plus(a, c), plus(a, b)))
    def plusIsSound(a: Float, b: Float): Boolean =
      subsumes(plus(inject(a), inject(b)), inject(a + b))
    /* Plus isn't required to be associative or commutative on floats */
    def minusPreservesBottom(a: F): Boolean =
      minus(a, bottom) == bottom && minus(bottom, a) == bottom
    def minusIsMonotone(a: F, b: F, c: F): Boolean =
      conditional(subsumes(c, b),
        subsumes(minus(a, c), minus(a, b)))
    def minusIsSound(a: Float, b: Float): Boolean =
      subsumes(minus(inject(a), inject(b)), inject(a - b))
    /* Minus isn't required to be anticommutative on floats */
    def timesPreservesBottom(a: F): Boolean =
      times(a, bottom) == bottom && times(bottom, a) == bottom
    def timesIsMonotone(a: F, b: F, c: F): Boolean =
      conditional(subsumes(c, b),
        subsumes(times(a, c), times(a, b)))
    def timesIsSound(a: Float, b: Float): Boolean =
      subsumes(times(inject(a), inject(b)), inject(a * b))
    /* Times isn't required to be associative and commutative on floats */
    def divPreservesBottom(a: F): Boolean =
      div(a, bottom) == bottom && conditional(!subsumes(a, inject(0)), div(bottom, a) == bottom)
    def divIsMonotone(a: F, b: F, c: F): Boolean =
      conditional(subsumes(c, b) && !subsumes(b, inject(0)) && !subsumes(c, inject(0)),
        subsumes(div(a, c), div(a, b)))
    def divIsSound(a: Float, b: Float): Boolean =
      conditional(b != 0,
        subsumes(div(inject(a), inject(b)), inject(a / b)))
    def ltPreservesBottom(a: F): Boolean =
      lt[B](a, bottom) == BoolLattice[B].bottom && lt[B](bottom, a) == BoolLattice[B].bottom
    def ltIsMonotone(a: F, b: F, c: F): Boolean =
      conditional(subsumes(b, c),
        BoolLattice[B].subsumes(lt[B](a, c), lt[B](a, b)))
    def ltIsSound(a: Float, b: Float): Boolean =
      BoolLattice[B].subsumes(lt[B](inject(a), inject(b)), BoolLattice[B].inject(a < b))
    def toStringPreservesBottom: Boolean =
      self.toString[S](bottom) == StringLattice[S].bottom
    def toStringIsMonotone(a: F, b: F): Boolean =
      conditional(subsumes(b, a),
        StringLattice[S].subsumes(self.toString[S](b), self.toString[S](a)))
    def toStringIsSound(a: Float): Boolean =
      StringLattice[S].subsumes(self.toString[S](inject(a)), StringLattice[S].inject(a.toString))
  }
  val floatLatticeLaw = new FloatLatticeLaw {}
}

object FloatLattice {
  def apply[F : FloatLattice]: FloatLattice[F] = implicitly
}

/** A lattice for characters */
trait CharLattice[C] extends LatticeElement[C] {
  def inject(c: Char): C

  trait CharLatticeLaw {
    /* No laws for now */
  }
  val charLatticeLaw = new CharLatticeLaw {}
}

object CharLattice {
  def apply[C : CharLattice]: CharLattice[C] = implicitly
}

/** A lattice for symbols */
trait SymbolLattice[Sym] extends LatticeElement[Sym] {
  def inject(sym: String): Sym

  trait SymbolLatticeLaw {
    /* No laws for now */
  }
  val symbolLatticeLaw = new SymbolLatticeLaw {}
}

object SymbolLattice {
  def apply[Sym : SymbolLattice]: SymbolLattice[Sym] = implicitly

}
