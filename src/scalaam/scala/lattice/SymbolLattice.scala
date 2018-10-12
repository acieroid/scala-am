package scalaam.lattice

import scalaam.core.Lattice

/** A lattice for symbols */
trait SymbolLattice[Sym] extends Lattice[Sym] {
  def inject(sym: String): Sym
  def toString[S : StringLattice](n: Sym): S

  trait SymbolLatticeLaw extends LatticeLaw {
    /* No laws for now */
  }
}

object SymbolLattice {
  def apply[Sym : SymbolLattice]: SymbolLattice[Sym] = implicitly

}
