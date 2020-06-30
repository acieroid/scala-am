package scalaam.lattice

import scalaam.core._

case object NotANumberString extends Error

/** A scalaam.lattice for strings */
trait StringLattice[S] extends Lattice[S] {
  def inject(s: String): S
  def length[I: IntLattice](s: S): I
  def append(s1: S, s2: S): S
  def ref[I: IntLattice, C: CharLattice](s: S, i: I): C
  def lt[B: BoolLattice](s1: S, s2: S): B
  def toSymbol[Sym: SymbolLattice](s: S): Sym
  def toNumber[I: IntLattice](s: S): MayFail[I, Error]
}

object StringLattice {
  def apply[S: StringLattice]: StringLattice[S] = implicitly
}
