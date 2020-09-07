package scalaam.lattice

import scalaam.core.Lattice

/** A scalaam.lattice for characters */
trait CharLattice[C] extends Lattice[C] {
  def inject(c: Char): C
  def downCase(c: C): C
  def upCase(c: C): C
  def toInt[I: IntLattice](c: C): I
  def toString[S: StringLattice](c: C): S

  def isLower[B: BoolLattice](c: C): B
  def isUpper[B: BoolLattice](c: C): B

  def charEq[B: BoolLattice](c1: C, c2: C): B
  def charLt[B: BoolLattice](c1: C, c2: C): B

  def charEqCI[B: BoolLattice](c1: C, c2: C): B
  def charLtCI[B: BoolLattice](c1: C, c2: C): B
}

object CharLattice {
  def apply[C: CharLattice]: CharLattice[C] = implicitly
}
