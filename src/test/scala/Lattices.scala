object Lattices {
  /* TODO: do something about counting */
  object ConcreteLattice extends MakeSchemeLattice[Concrete.S, Concrete.B, Concrete.I, Concrete.F, Concrete.C, Concrete.Sym](true)
  object TypeLattice extends MakeSchemeLattice[Type.S, Concrete.B, Type.I, Type.F, Type.C, Type.Sym](false)
  val bounded = new BoundedInteger(100)
  object BoundedIntLattice extends MakeSchemeLattice[Type.S, Concrete.B, bounded.I, Type.F, Type.C, Type.Sym](false)
  object ConstantPropagationLattice extends MakeSchemeLattice[ConstantPropagation.S, Concrete.B, ConstantPropagation.I, ConstantPropagation.F, ConstantPropagation.C, ConstantPropagation.Sym](false)
}
