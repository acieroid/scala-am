package scalaam.lattice

/** A lattice for characters */
trait CharLattice[C] extends Lattice[C] {
  def inject(c: Char): C

  trait CharLatticeLaw {
    /* No laws for now */
  }
  val charLatticeLaw = new CharLatticeLaw {}
}

object CharLattice {
  def apply[C : CharLattice]: CharLattice[C] = implicitly
}
