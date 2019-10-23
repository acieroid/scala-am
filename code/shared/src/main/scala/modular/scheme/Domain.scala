package scalaam.modular.scheme

import scalaam.lattice._
import scalaam.language.scheme._
import scalaam.modular.{GlobalStore, ModAnalysis}

/* A type lattice for ModF */
trait TypeDomain {
  this: ModAnalysis[SchemeExp] with GlobalStore[SchemeExp] =>
  lazy val valueLattice: MakeSchemeLattice[SchemeExp,
                                           Addr,
                                           Type.S,
                                           Concrete.B,
                                           Type.I,
                                           Type.R,
                                           Type.C,
                                           Type.Sym] = new MakeSchemeLattice
  type Value = valueLattice.L
  lazy val lattice = valueLattice.schemeLattice
}

/* A constant propagation lattice for ModF */
trait ConstantPropagationDomain {
  this: ModAnalysis[SchemeExp] with GlobalStore[SchemeExp] =>
  lazy val valueLattice: MakeSchemeLattice[SchemeExp,
                                           Addr,
                                           ConstantPropagation.S,
                                           Concrete.B,
                                           ConstantPropagation.I,
                                           ConstantPropagation.R,
                                           ConstantPropagation.C,
                                           ConstantPropagation.Sym] = new MakeSchemeLattice
  type Value = valueLattice.L
  lazy val lattice = valueLattice.schemeLattice
}


/* A powerset lattice for ModF */
trait PowersetDomain {
  this: ModAnalysis[SchemeExp] with GlobalStore[SchemeExp] =>
  lazy val valueLattice: MakeSchemeLattice[SchemeExp,
                                           Addr,
                                           Concrete.S,
                                           Concrete.B,
                                           Concrete.I,
                                           Concrete.R,
                                           Concrete.C,
                                           Concrete.Sym] = new MakeSchemeLattice
  type Value = valueLattice.L
  lazy val lattice = valueLattice.schemeLattice
}
