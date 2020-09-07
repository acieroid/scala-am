package scalaam.lattice

import scalaam.core.Lattice

/** A lattice for reals (i.e., floating point numbers) */
trait RealLattice[R] extends Lattice[R] { self =>
  def inject(n: Double): R
  def toInt[I: IntLattice](n: R): I
  def ceiling[I: IntLattice](n: R): I
  def floor[I: IntLattice](n: R): I
  def round[I: IntLattice](n: R): I
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
  def expt(n1: R, n2: R): R
  def lt[B: BoolLattice](n1: R, n2: R): B
  def toString[S: StringLattice](n: R): S
}

object RealLattice {
  def apply[R: RealLattice]: RealLattice[R] = implicitly
}
