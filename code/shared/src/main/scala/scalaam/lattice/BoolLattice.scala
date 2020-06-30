package scalaam.lattice

import scalaam.core.Lattice

/** A scalaam.lattice for booleans */
trait BoolLattice[B] extends Lattice[B] {
  def inject(b: Boolean): B
  def isTrue(b: B): Boolean
  def isFalse(b: B): Boolean
  def not(b: B): B
  def top: B
}

object BoolLattice {
  def apply[B: BoolLattice]: BoolLattice[B] = implicitly
}
