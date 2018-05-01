package scalaam.lattice

/** A lattice for symbols */
trait SymbolLattice[Sym] extends Lattice[Sym] {
  def inject(sym: String): Sym
  def toString[S : StringLattice](n: Sym): S

  trait SymbolLatticeLaw {
    /* No laws for now */
  }
  val symbolLatticeLaw = new SymbolLatticeLaw {}
}

object SymbolLattice {
  def apply[Sym : SymbolLattice]: SymbolLattice[Sym] = implicitly

}
