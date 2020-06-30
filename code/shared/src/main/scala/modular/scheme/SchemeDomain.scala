package scalaam.modular.scheme

import scalaam.lattice._
import scalaam.language.scheme._

trait AbstractSchemeDomain extends SchemeSemantics {
  // parameterized by different abstract domains for each type
  type S
  type B
  type I
  type R
  type C
  type Sym
  // which are used to construct a "modular" (~ product) lattice
  val valueLattice: ModularSchemeLattice[Addr,S,B,I,R,C,Sym]
  type Value = valueLattice.L
  lazy val lattice = valueLattice.schemeLattice
}

/* A type lattice for ModF */
trait SchemeTypeDomain extends AbstractSchemeDomain {
  // use type domains everywhere, except for booleans
  type S    = Type.S
  type B    = Concrete.B
  type I    = Type.I
  type R    = Type.R
  type C    = Type.C
  type Sym  = Concrete.Sym
  // make the scheme lattice
  lazy val valueLattice = new ModularSchemeLattice
}

/* A constant propagation lattice for ModF */
trait SchemeConstantPropagationDomain extends AbstractSchemeDomain {
  // use constant propagation domains everywhere, except for booleans
  type S    = ConstantPropagation.S
  type B    = ConstantPropagation.B
  type I    = ConstantPropagation.I
  type R    = ConstantPropagation.R
  type C    = ConstantPropagation.C
  type Sym  = Concrete.Sym
  // make the scheme lattice
  lazy val valueLattice = new ModularSchemeLattice
}

/* A powerset lattice for ModF */
trait SchemePowersetDomain extends AbstractSchemeDomain {
  // use powerset domains everywhere
  type S    = Concrete.S
  type B    = Concrete.B
  type I    = Concrete.I
  type R    = Concrete.R
  type C    = Concrete.C
  type Sym  = Concrete.Sym
  // make the scheme lattice
  lazy val valueLattice = new ModularSchemeLattice
}