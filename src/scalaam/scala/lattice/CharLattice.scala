package scalaam.lattice

import scalaam.core.Lattice

/** A lattice for characters */
trait CharLattice[C] extends Lattice[C] {
  def inject(c: Char): C
}

object CharLattice {
  def apply[C: CharLattice]: CharLattice[C] = implicitly
}
