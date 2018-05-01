package scalaam.lattice

/** A lattice for strings */
trait StringLattice[S] extends Lattice[S] {
  def inject(s: String): S
  def length[I : IntLattice](s: S): I
  def append(s1: S, s2: S): S
  def lt[B : BoolLattice](s1: S, s2: S): B
  def toSymbol[Sym : SymbolLattice](s: S): Sym

  class StringLatticeLaw[I]()(implicit val intLat: IntLattice[I]) extends LatticeLaw {
    import scalaz.std.boolean.conditional

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
}

object StringLattice {
  def apply[S : StringLattice]: StringLattice[S] = implicitly
}
