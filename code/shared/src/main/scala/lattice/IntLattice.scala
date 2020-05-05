package scalaam.lattice

import scalaam.core.Lattice

/** A lattice for integers */
trait IntLattice[I] extends Lattice[I] { self =>
  def inject(n: Int): I
  def toReal[R: RealLattice](n: I): R
  def random(n: I): I
  def plus(n1: I, n2: I): I
  def minus(n1: I, n2: I): I
  def times(n1: I, n2: I): I
  def quotient(n1: I, n2: I): I
  def div[R: RealLattice](n1: I, n2: I): R
  def expt(n1: I, n2: I): I
  def modulo(n1: I, n2: I): I
  def remainder(n1: I, n2: I): I
  def lt[B: BoolLattice](n1: I, n2: I): B
  def valuesBetween(n1: I, n2: I): Set[I]
  def toString[S: StringLattice](n: I): S
}

object IntLattice {
  def apply[I: IntLattice]: IntLattice[I] = implicitly
}
