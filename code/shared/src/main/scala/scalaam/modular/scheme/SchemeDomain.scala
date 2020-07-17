package scalaam.modular.scheme

import scalaam.core._
import scalaam.modular._
import scalaam.lattice._
import scalaam.language.scheme._
import scalaam.language.scheme.primitives._

trait SchemeDomain extends AbstractDomain[SchemeExp] {
  type Prim = SchemePrimitive[Value, Address]
  implicit val lattice: SchemeLattice[Value, Address, Prim] 
}

trait SchemeLatticeDefinition {
  type S
  type B
  type I
  type R
  type C
  type Sym
  val valueLattice: ModularSchemeLattice[Address, S, B, I, R, C, Sym]
}

trait ModularSchemeDomain extends SchemeDomain {
  val ld: SchemeLatticeDefinition
  // which are used to construct a "modular" (~ product) lattice
  implicit val valueLattice: ModularSchemeLattice[Address, ld.S, ld.B, ld.I, ld.R, ld.C, ld.Sym] = ld.valueLattice
  type Value = valueLattice.L
  lazy val lattice: SchemeLattice[valueLattice.L, Address, Prim] = valueLattice.schemeLattice
}

object SchemeTypeDomain extends SchemeLatticeDefinition {
  // use type domains everywhere, except for booleans
  type S    = Type.S
  type B    = ConstantPropagation.B
  type I    = Type.I
  type R    = Type.R
  type C    = Type.C
  type Sym  = Concrete.Sym
  // make the scheme lattice
  lazy val valueLattice = new ModularSchemeLattice
}

/* A type scalaam.lattice for ModF */
trait SchemeTypeDomain extends ModularSchemeDomain {
  // make the scheme lattice
  val ld = SchemeTypeDomain
}

object SchemeConstantPropagationDomain extends SchemeLatticeDefinition {
  // use constant propagation domains everywhere, except for booleans
  type S    = ConstantPropagation.S
  type B    = ConstantPropagation.B
  type I    = ConstantPropagation.I
  type R    = ConstantPropagation.R
  type C    = ConstantPropagation.C
  type Sym  = Concrete.Sym
  // make the scheme scalaam.lattice
  lazy val valueLattice = new ModularSchemeLattice
}

/* A constant propagation scalaam.lattice for ModF */
trait SchemeConstantPropagationDomain extends ModularSchemeDomain {
  val ld = SchemeConstantPropagationDomain
}

object SchemePowersetDomain extends SchemeLatticeDefinition {
  // use powerset domains everywhere
  type S    = Concrete.S
  type B    = Concrete.B
  type I    = Concrete.I
  type R    = Concrete.R
  type C    = Concrete.C
  type Sym  = Concrete.Sym
  lazy val valueLattice = new ModularSchemeLattice
}

/* A powerset scalaam.lattice for ModF */
trait SchemePowersetDomain extends ModularSchemeDomain {
  val ld = SchemePowersetDomain
}