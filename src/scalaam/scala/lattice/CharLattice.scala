package scalaam.lattice

/** A lattice for characters */
trait CharLattice[C] extends Lattice[C] {
  def inject(c: Char): C

  trait CharLatticeLaw extends LatticeLaw {
    /* No laws for now */
  }
}

object CharLattice {
  def apply[C : CharLattice]: CharLattice[C] = implicitly
}
