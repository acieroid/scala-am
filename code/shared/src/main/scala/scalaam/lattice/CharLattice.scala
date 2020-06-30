package scalaam.lattice

import scalaam.core.Lattice

/** A scalaam.lattice for characters */
trait CharLattice[C] extends Lattice[C] {
  def inject(c: Char): C
  def downCase(c: C): C
  def toInt[I : IntLattice](c: C): I
  def toString[S: StringLattice](c: C): S
}

object CharLattice {
  def apply[C: CharLattice]: CharLattice[C] = implicitly
}
