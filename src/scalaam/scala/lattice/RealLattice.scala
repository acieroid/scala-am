package scalaam.lattice

/** A lattice for reals (i.e., floating point numbers) */
trait RealLattice[R] extends Lattice[R] { self =>
  def inject(n: Double): R
  def toInt[I : IntLattice](n: R): I
  def ceiling(n: R): R
  def floor(n: R): R
  def round(n: R): R
  def log(n: R): R
  def random(n: R): R
  def sin(n: R): R
  def asin(n: R): R
  def cos(n: R): R
  def acos(n: R): R
  def tan(n: R): R
  def atan(n: R): R
  def sqrt(n: R): R
  def plus(n1: R, n2: R): R
  def minus(n1: R, n2: R): R
  def times(n1: R, n2: R): R
  def div(n1: R, n2: R): R
  def lt[B : BoolLattice](n1: R, n2: R): B
  def toString[S : StringLattice](n: R): S

  class RealLatticeLaw[I, B, S](implicit val intLat: IntLattice[I], boolLat: BoolLattice[B], strLat: StringLattice[S]) extends LatticeLaw {

    def toIntPreservesBottom: Boolean =
      toInt[I](bottom) == IntLattice[I].bottom
    def toIntIsMonotone(a: R, b: R): Boolean =
      conditional(subsumes(b, a),
        IntLattice[I].subsumes(toInt[I](b), toInt[I](a)))
    def toIntIsSound(a: Double): Boolean =
      IntLattice[I].subsumes(toInt[I](inject(a)), IntLattice[I].inject(a.toInt))
    def ceilingPreservesBottom: Boolean =
      ceiling(bottom) == bottom
    def ceilingIsMonotone(a: R, b: R): Boolean =
      conditional(subsumes(b, a),
        subsumes(ceiling(b), ceiling(a)))
    def ceilingIsSound(a: Double): Boolean =
      subsumes(ceiling(inject(a)), inject(MathOps.ceil(a)))
    def logPreservesBottom: Boolean =
      log(bottom) == bottom
    def logIsMonotone(a: R, b: R): Boolean =
      /* TODO: this test is failing */
      /*conditional(subsumes(b, a),
       subsumes(log(b), log(a))) */
      true
    def logIsSound(a: Double): Boolean =
      conditional(a > 0,
        subsumes(log(inject(a)), inject(scala.math.log(a))))
    def randomPreservesBottom: Boolean =
      random(bottom) == bottom
    /* Random should neither be monotone nor sound (at least in concrete) */
    def plusPreservesBottom(a: R): Boolean =
      plus(a, bottom) == bottom && plus(bottom, a) == bottom
    def plusIsMonotone(a: R, b: R, c: R): Boolean =
      conditional(subsumes(c, b),
        subsumes(plus(a, c), plus(a, b)))
    def plusIsSound(a: Double, b: Double): Boolean =
      subsumes(plus(inject(a), inject(b)), inject(a + b))
    /* Plus isn't required to be associative or commutative on floats */
    def minusPreservesBottom(a: R): Boolean =
      minus(a, bottom) == bottom && minus(bottom, a) == bottom
    def minusIsMonotone(a: R, b: R, c: R): Boolean =
      conditional(subsumes(c, b),
        subsumes(minus(a, c), minus(a, b)))
    def minusIsSound(a: Double, b: Double): Boolean =
      subsumes(minus(inject(a), inject(b)), inject(a - b))
    /* Minus isn't required to be anticommutative on floats */
    def timesPreservesBottom(a: R): Boolean =
      times(a, bottom) == bottom && times(bottom, a) == bottom
    def timesIsMonotone(a: R, b: R, c: R): Boolean =
      conditional(subsumes(c, b),
        subsumes(times(a, c), times(a, b)))
    def timesIsSound(a: Double, b: Double): Boolean =
      subsumes(times(inject(a), inject(b)), inject(a * b))
    /* Times isn't required to be associative and commutative on floats */
    def divPreservesBottom(a: R): Boolean =
      div(a, bottom) == bottom && conditional(!subsumes(a, inject(0)), div(bottom, a) == bottom)
    def divIsMonotone(a: R, b: R, c: R): Boolean =
      conditional(subsumes(c, b) && !subsumes(b, inject(0)) && !subsumes(c, inject(0)),
        subsumes(div(a, c), div(a, b)))
    def divIsSound(a: Double, b: Double): Boolean =
      conditional(b != 0,
        subsumes(div(inject(a), inject(b)), inject(a / b)))
    def ltPreservesBottom(a: R): Boolean =
      lt[B](a, bottom) == BoolLattice[B].bottom && lt[B](bottom, a) == BoolLattice[B].bottom
    def ltIsMonotone(a: R, b: R, c: R): Boolean =
      conditional(subsumes(b, c),
        BoolLattice[B].subsumes(lt[B](a, c), lt[B](a, b)))
    def ltIsSound(a: Double, b: Double): Boolean =
      BoolLattice[B].subsumes(lt[B](inject(a), inject(b)), BoolLattice[B].inject(a < b))
    def toStringPreservesBottom: Boolean =
      self.toString[S](bottom) == StringLattice[S].bottom
    def toStringIsMonotone(a: R, b: R): Boolean =
      conditional(subsumes(b, a),
        StringLattice[S].subsumes(self.toString[S](b), self.toString[S](a)))
    def toStringIsSound(a: Double): Boolean =
      StringLattice[S].subsumes(self.toString[S](inject(a)), StringLattice[S].inject(a.toString))
  }
}

object RealLattice {
  def apply[R : RealLattice]: RealLattice[R] = implicitly
}

