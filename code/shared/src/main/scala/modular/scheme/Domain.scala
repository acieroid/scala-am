package scalaam.modular.scheme

import scalaam.lattice._
import scalaam.language.scheme._
import scalaam.modular.ModAnalysis._

trait AbstractDomain extends SchemeModFSemantics {
  // parameterized by different abstract domains for each type
  type S
  type B
  type I
  type R
  type C
  type Sym
  // which are used to construct a "modular" (~ product) lattice
  val valueLattice: ModularSchemeLattice[Addr,ComponentPointer,S,B,I,R,C,Sym]
  type Value = valueLattice.L
  lazy val lattice = valueLattice.schemeLattice
}

trait AdaptiveAbstractDomain extends AdaptiveSchemeModFSemantics with AbstractDomain {
  override def alphaValue(value: Value) = value match {
    case valueLattice.Element(v)    => valueLattice.Element(alphaV(v))
    case valueLattice.Elements(vs)  => valueLattice.Elements(vs.map(alphaV))
  }
  private def alphaV(value: valueLattice.Value): valueLattice.Value = value match {
    case valueLattice.Pointer(addr)     => valueLattice.Pointer(alphaAddr(addr))
    case valueLattice.Clo(lam,cmp,nam)  => valueLattice.Clo(lam,alpha(cmp),nam)
    case valueLattice.Cons(car,cdr)     => valueLattice.Cons(alphaValue(car),alphaValue(cdr))
    case valueLattice.Vec(siz,els,ini)  => valueLattice.Vec(siz,els.view.mapValues(alphaValue).toMap,alphaValue(ini))
    case _                              => value
  }
}

/* A type lattice for ModF */
trait TypeDomain extends AbstractDomain {
  // use type domains everywhere, except for booleans
  type S    = Type.S
  type B    = Concrete.B
  type I    = Type.I
  type R    = Type.R
  type C    = Type.C
  type Sym  = Type.Sym
  // make the scheme lattice
  lazy val valueLattice = new ModularSchemeLattice
}

/* A constant propagation lattice for ModF */
trait ConstantPropagationDomain extends AbstractDomain {
  // use constant propagation domains everywhere, except for booleans
  type S    = ConstantPropagation.S
  type B    = Concrete.B
  type I    = ConstantPropagation.I
  type R    = ConstantPropagation.R
  type C    = ConstantPropagation.C
  type Sym  = ConstantPropagation.Sym
  // make the scheme lattice
  lazy val valueLattice = new ModularSchemeLattice
}

/* A powerset lattice for ModF */
trait PowersetDomain extends AbstractDomain {
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
