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

trait ModularSchemeDomain extends SchemeDomain {
  // gets a domain from a modular lattice wrapper
  val modularLatticeWrapper: ModularSchemeLatticeWrapper
  // extracts value and lattice definition from this wrapper
  type Value = modularLatticeWrapper.modularLattice.L
  lazy val lattice = modularLatticeWrapper.modularLattice.schemeLattice
}

trait ModularSchemeLatticeWrapper {
  // consists of several types
  type S
  type B
  type I
  type R
  type C
  type Sym
  // holds a modular lattice
  val modularLattice: ModularSchemeLattice[Address,S,B,I,R,C,Sym]
}

//
// TYPE DOMAIN
//

/* A type scalaam.lattice for ModF */
object SchemeTypeDomain extends ModularSchemeLatticeWrapper {
  // use type domains everywhere, except for booleans
  type S    = Type.S
  type B    = ConstantPropagation.B
  type I    = Type.I
  type R    = Type.R
  type C    = Type.C
  type Sym  = Concrete.Sym
  // make the scheme lattice
  lazy val modularLattice = new ModularSchemeLattice
}

trait SchemeTypeDomain extends ModularSchemeDomain {
  lazy val modularLatticeWrapper = SchemeTypeDomain
}

//
// CONSTANT PROPAGATION DOMAIN
//

object SchemeConstantPropagationDomain extends ModularSchemeLatticeWrapper {
  // use constant propagation domains everywhere, except for booleans
  type S    = ConstantPropagation.S
  type B    = ConstantPropagation.B
  type I    = ConstantPropagation.I
  type R    = ConstantPropagation.R
  type C    = ConstantPropagation.C
  type Sym  = Concrete.Sym
  // make the scheme scalaam.lattice
  lazy val modularLattice = new ModularSchemeLattice
}

trait SchemeConstantPropagationDomain extends ModularSchemeDomain {
  lazy val modularLatticeWrapper = SchemeConstantPropagationDomain
}

//
// POWERSET DOMAIN
//

/* A powerset scalaam.lattice for ModF */
object SchemePowersetDomain extends ModularSchemeLatticeWrapper {
  // use powerset domains everywhere
  type S    = Concrete.S
  type B    = Concrete.B
  type I    = Concrete.I
  type R    = Concrete.R
  type C    = Concrete.C
  type Sym  = Concrete.Sym
  // make the scheme scalaam.lattice
  lazy val modularLattice = new ModularSchemeLattice
}

trait SchemePowersetDomain extends ModularSchemeDomain {
  lazy val modularLatticeWrapper = SchemePowersetDomain
}