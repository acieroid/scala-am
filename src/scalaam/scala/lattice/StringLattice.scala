package scalaam.lattice

import scalaam.core.Lattice

/** A lattice for strings */
trait StringLattice[S] extends Lattice[S] {
  def inject(s: String): S
  def length[I: IntLattice](s: S): I
  def append(s1: S, s2: S): S
  def lt[B: BoolLattice](s1: S, s2: S): B
  def toSymbol[Sym: SymbolLattice](s: S): Sym
}

object StringLattice {
  def apply[S: StringLattice]: StringLattice[S] = implicitly
}
