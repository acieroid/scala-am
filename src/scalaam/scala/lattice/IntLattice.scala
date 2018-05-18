package scalaam.lattice

/** A lattice for integers */
trait IntLattice[I] extends Lattice[I] { self =>
  def inject(n: Int): I
  def toReal[R : RealLattice](n: I): R
  def random(n: I): I
  def plus(n1: I, n2: I): I
  def minus(n1: I, n2: I): I
  def times(n1: I, n2: I): I
  def quotient(n1: I, n2: I): I
  def div[R : RealLattice](n1: I, n2: I): R
  def modulo(n1: I, n2: I): I
  def remainder(n1: I, n2: I): I
  def lt[B : BoolLattice](n1: I, n2: I): B
  def toString[S : StringLattice](n: I): S

  class IntLatticeLaw[R, B, S](implicit val realLat: RealLattice[R], boolLat: BoolLattice[B], strLat: StringLattice[S]) extends LatticeLaw {

    def toRealPreservesBottom: Boolean =
      toReal[R](bottom) == RealLattice[R].bottom
    def toRealIsMonotone(a: I, b: I): Boolean =
      conditional(subsumes(b, a),
        RealLattice[R].subsumes(toReal[R](b), toReal[R](a)))
    def toRealIsSound(a: Int): Boolean =
      RealLattice[R].subsumes(toReal[R](inject(a)), RealLattice[R].inject(a))
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
    def quotientPreservesBottom(a: I): Boolean =
      quotient(a, bottom) == bottom && conditional(!subsumes(a, inject(0)), quotient(bottom, a) == bottom)
    def quotientIsMonotone(a: I, b: I, c: I): Boolean =
      conditional(subsumes(c, b) && !subsumes(b, inject(0)) && !subsumes(c, inject(0)),
        subsumes(quotient(a, c), quotient(a, b)))
    def quotientIsSound(a: Int, b: Int): Boolean =
      conditional(b != 0,
        subsumes(quotient(inject(a), inject(b)), inject(a / b)))
    def moduloPreservesBottom(a: I): Boolean =
      modulo(a, bottom) == bottom && conditional(!subsumes(a, inject(0)), modulo(bottom, a) == bottom)
    def moduloIsMonotone(a: I, b: I, c: I): Boolean =
      conditional(subsumes(c, b) && !subsumes(c, inject(0)) && !subsumes(b, inject(0)),
        subsumes(modulo(a, c), modulo(a, b)))
    def moduloIsSound(a: Int, b: Int): Boolean =
      conditional(b != 0,
        subsumes(modulo(inject(a), inject(b)), inject(MathOps.modulo(a, b))))
    def remainderPreservesBottom(a: I): Boolean =
      remainder(a, bottom) == bottom && conditional(!subsumes(a, inject(0)), remainder(bottom, a) == bottom)
    def remainderIsMonotone(a: I, b: I, c: I): Boolean =
      conditional(subsumes(c, b) && !subsumes(c, inject(0)) && !subsumes(b, inject(0)),
        subsumes(remainder(a, c), remainder(a, b)))
    def remainderIsSound(a: Int, b: Int): Boolean =
      conditional(b != 0,
        subsumes(remainder(inject(a), inject(b)), inject(MathOps.remainder(a, b))))
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
}

object IntLattice {
  def apply[I : IntLattice]: IntLattice[I] = implicitly
}
