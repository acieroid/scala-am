import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import Prop.forAll

import scalaz._
import scalaz.Scalaz._

import SchemeOps._

object LatticeProperties {
  private def newProperties(name: String)(f: Properties => Unit): Properties = {
    /* Code taken from Scalaz's test suite */
    val p = new Properties(name)
    f(p)
    p
  }

  /* TODO: import scalaz properties for monoids */

  object latticeElement {
    def laws[L](implicit l: LatticeElement[L], gen: LatticeGenerator[L]): Properties = {
      implicit val arb = gen.anyArb
      newProperties("LatticeElement") { p =>
        p.property("∀ a: ⊥ ⊑ a") =
          forAll(l.latticeElementLaw.bottomLowerBound _)
        p.property("∀ a: a ⊑ ⊤") =
          forAll(l.latticeElementLaw.topUpperBound _)
        p.property("∀ a, b: a ⊔ b = b ⊔ a") =
          forAll(l.latticeElementLaw.joinCommutative _)
        p.property("∀ a, b, c: (a ⊔ b) ⊔ c = a ⊔ (b ⊔ c)") =
          forAll(l.latticeElementLaw.joinAssociative _)
        p.property("∀ a: a ⊔ a = a") =
          forAll(l.latticeElementLaw.joinIdempotent _)
        p.property("∀ a, b: a ⊑ b ⇒ a ⊔ b = b") = forAll { (b: L) =>
          forAll(gen.le(b)) { (a: L) =>
            l.latticeElementLaw.joinSubsumesCompatible(a, b)
          }
        }
        p.property("∀ a: a = bottom ∨ isTrue(eql(a, a))") =
          forAll(l.latticeElementLaw.eqlIsTrue _)
      }
    }
  }
  object boolLattice {
    def laws[B](implicit l: BoolLattice[B], gen: LatticeGenerator[B]): Properties = {
      implicit val arb = gen.anyArb
      newProperties("BoolLattice") { p =>
        p.include(latticeElement.laws[B])
        p.property("isTrue(inject(true)) ∧ isFalse(inject(false))") =
          l.boolLatticeLaw.injectPreservesTruthiness
        p.property("isTrue(⊤) ∧ isFalse(⊤)") =
          l.boolLatticeLaw.topTrueAndFalse
        p.property("¬isTrue(⊥) ∧ ¬isFalse(⊥)") =
          l.boolLatticeLaw.bottomNotTrueNorFalse
        p.property("∀ a: isTrue(a) ⇒ isFalse(not(a)) ∧ isFalse(a) ⇒ isTrue(not(a))") =
          forAll(l.boolLatticeLaw.notReversesTruthiness _)
        p.property("∀ a: not(not(a)) == a") =
          forAll(l.boolLatticeLaw.notInvolutive _)
      }
    }
  }
  object stringLattice {
    def laws[S](implicit l: StringLattice[S], gen: LatticeGenerator[S]): Properties = {
      implicit val arb = gen.anyArb
      newProperties("StringLattice") { p =>
        p.include(latticeElement.laws[S])
        p.property("length(⊥) = ⊥") =
          l.stringLatticeLaw.lengthPreservesBottom
        p.property("∀ a, b: a ⊑ b ⇒ length(a) ⊑ length(b)") = forAll { (b: S) =>
          forAll(gen.le(b)) { (a: S) =>
            l.stringLatticeLaw.lengthIsMonotone(a, b)
          }
        }
        p.property("∀ a: length(inject(a)) ⊑ inject(length(a))") =
          forAll(l.stringLatticeLaw.lengthIsSound _)
        p.property("∀ a: append(a, ⊥) = ⊥ = append(⊥, a)") =
          forAll(l.stringLatticeLaw.appendPreservesBottom _)
        p.property("∀ a, b, c: b ⊑ c ⇒ append(a, b) ⊑ append(b, c)") = forAll { (a: S, c: S) =>
          forAll(gen.le(c)) { (b: S) =>
            l.stringLatticeLaw.appendIsMonotone(a, b, c)
          }
        }
        p.property("∀ a, b: append(inject(a), inject(b)) ⊑ inject(append(a, b))") =
          forAll(l.stringLatticeLaw.appendIsSound _)
        p.property("∀ a, b, c: append(a, append(b, c)) == append(append(a, b), c)") =
          forAll(l.stringLatticeLaw.appendIsAssociative _)
      }
    }
  }
  object intLattice {
    def laws[I](implicit l: IntLattice[I], gen: LatticeGenerator[I]): Properties = {
      implicit val arb = gen.anyArb
      newProperties("IntLattice") { p =>
        p.include(latticeElement.laws[I])
        p.property("toFloat(⊥) = ⊥") =
          l.intLatticeLaw.toFloatPreservesBottom
        p.property("∀ a, b: a ⊑ b ⇒ toFloat(a) ⊑ toFloat(b)") = forAll { (b: I) =>
          forAll(gen.le(b)) { (a: I) =>
            l.intLatticeLaw.toFloatIsMonotone(a, b)
          }
        }
        p.property("∀ a: a.toFloat ⊑ toFloat(inject(a))") =
          forAll(l.intLatticeLaw.toFloatIsSound _)
        p.property("random(⊥) = ⊥") =
          l.intLatticeLaw.randomPreservesBottom
        p.property("plus(a, ⊥) = ⊥ = plus(⊥, a)") =
          forAll(l.intLatticeLaw.plusPreservesBottom _)
        p.property("∀ a, b, c: b ⊑ c ⇒ plus(a, b) ⊑ plus(a, c)") = forAll { (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            l.intLatticeLaw.plusIsMonotone(a, b, c)
          }
        }
        p.property("∀ a, b: inject(a + b) ⊑ plus(inject(a), inject(b))") =
          forAll(l.intLatticeLaw.plusIsSound _)
        p.property("∀ a, b, c: plus(a, plus(b, c)) == plus(plus(a, b), c)") =
          forAll(l.intLatticeLaw.plusIsAssociative _)
        p.property("∀ a, b: plus(a, b) == plus(b, a)") =
          forAll(l.intLatticeLaw.plusIsCommutative _)
        p.property("minus(a, ⊥) = ⊥ = minus(⊥, a)") =
          forAll(l.intLatticeLaw.minusPreservesBottom _)
        p.property("∀ a, b, c: b ⊑ c ⇒ minus(a, b) ⊑ minus(a, c)") = forAll { (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            l.intLatticeLaw.minusIsMonotone(a, b, c)
          }
        }
        p.property("∀ a, b: inject(a - b) ⊑ minus(inject(a), inject(b))") =
          forAll(l.intLatticeLaw.minusIsSound _)
        p.property("∀ a, b: minus(a, b) == minus(inject(0), minus(b, a))") =
          forAll(l.intLatticeLaw.minusIsAnticommutative _)
        p.property("times(a, ⊥) = ⊥ = times(⊥, a)") =
          forAll(l.intLatticeLaw.timesPreservesBottom _)
        p.property("∀ a, b, c: b ⊑ c ⇒ times(a, b) ⊑ times(a, c)") = forAll { (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            l.intLatticeLaw.timesIsMonotone(a, b, c)
          }
        }
        p.property("∀ a, b: inject(a + b) ⊑ times(inject(a), inject(b))") =
          forAll(l.intLatticeLaw.timesIsSound _)
        p.property("∀ a, b, c: times(a, times(b, c)) == times(times(a, b), c)") =
          forAll(l.intLatticeLaw.timesIsAssociative _)
        p.property("∀ a, b: times(a, b) == times(b, a)") =
          forAll(l.intLatticeLaw.timesIsCommutative _)
        p.property("div(a, ⊥) = ⊥ = div(⊥, a)") =
          forAll(l.intLatticeLaw.divPreservesBottom _)
        p.property("∀ a, b, c: b ⊑ c ⇒ div(a, b) ⊑ div(a, c)") = forAll { (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            l.intLatticeLaw.divIsMonotone(a, b, c)
          }
        }
        p.property("∀ a, b ≠ 0: inject(a / b) ⊑ div(inject(a), inject(b))") =
          forAll(l.intLatticeLaw.divIsSound _)
        p.property("modulo(a, ⊥) = ⊥ = modulo(⊥, a)") =
          forAll(l.intLatticeLaw.moduloPreservesBottom _)
        p.property("∀ a, b, c: b ⊑ c ⇒ modulo(a, b) ⊑ modulo(a, c)") = forAll { (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            l.intLatticeLaw.moduloIsMonotone(a, b, c)
          }
        }
        p.property("∀ a, b ≠ 0: inject(a / b) ⊑ modulo(inject(a), inject(b))") =
          forAll(l.intLatticeLaw.moduloIsSound _)
        p.property("lt(a, ⊥) = ⊥ = lt(⊥, a)") =
          forAll(l.intLatticeLaw.ltPreservesBottom _)
        p.property("∀ a, b, c: b ⊑ c ⇒ lt(a, b) ⊑ lt(a, c)") = forAll { (a: I, c: I) =>
          forAll(gen.le(c)) { (b: I) =>
            l.intLatticeLaw.ltIsMonotone(a, b, c)
          }
        }
        p.property("∀ a, b ≠ 0: inject(a < b) ⊑ lt(inject(a), inject(b))") =
          forAll(l.intLatticeLaw.ltIsSound _)
        p.property("toString(⊥) = ⊥") =
          l.intLatticeLaw.toStringPreservesBottom
        p.property("∀ a, b: a ⊑ b ⇒ toString(a) ⊑ toString(b)") = forAll { (b: I) =>
          forAll(gen.le(b)) { (a: I) =>
            l.intLatticeLaw.toStringIsMonotone(a, b)
          }
        }
        p.property("∀ a: inject(a.toString) ⊑ toString(inject(a))") =
          forAll(l.intLatticeLaw.toStringIsSound _)
      }
    }
  }

  object floatLattice {
    def laws[F](implicit l: FloatLattice[F], gen: LatticeGenerator[F]): Properties = {
      implicit val arb = gen.anyArb
      newProperties("FloatLattice") { p =>
        p.include(latticeElement.laws[F])
        p.property("toInt(⊥) = ⊥") =
          l.floatLatticeLaw.toIntPreservesBottom
        p.property("∀ a, b: a ⊑ b ⇒ toInt(a) ⊑ toInt(b)") = forAll { (b: F) =>
          forAll(gen.le(b)) { (a: F) =>
            l.floatLatticeLaw.toIntIsMonotone(a, b)
          }
        }
        p.property("∀ a: a.toInt ⊑ toInt(inject(a))") =
          forAll(l.floatLatticeLaw.toIntIsSound _)
        p.property("ceiling(⊥) = ⊥") =
          l.floatLatticeLaw.ceilingPreservesBottom
        p.property("∀ a, b: a ⊑ b ⇒ ceiling(a) ⊑ ceiling(b)") = forAll { (b: F) =>
          forAll(gen.le(b)) { (a: F) =>
            l.floatLatticeLaw.ceilingIsMonotone(a, b)
          }
        }
        p.property("∀ a: inject(a.ceil) ⊑ ceiling(inject(a))") =
          forAll(l.floatLatticeLaw.ceilingIsSound _)
        p.property("log(⊥) = ⊥") =
          l.floatLatticeLaw.logPreservesBottom
        p.property("∀ a, b: a ⊑ b ⇒ log(a) ⊑ log(b)") = forAll { (b: F) =>
          forAll(gen.le(b)) { (a: F) =>
            l.floatLatticeLaw.logIsMonotone(a, b)
          }
        }
        p.property("∀ a: inject(a.log) ⊑ log(inject(a))") =
          forAll(l.floatLatticeLaw.logIsSound _)
        p.property("random(⊥) = ⊥") =
          l.floatLatticeLaw.randomPreservesBottom
        p.property("plus(a, ⊥) = ⊥ = plus(⊥, a)") =
          forAll(l.floatLatticeLaw.plusPreservesBottom _)
        p.property("∀ a, b, c: b ⊑ c ⇒ plus(a, b) ⊑ plus(a, c)") = forAll { (a: F, c: F) =>
          forAll(gen.le(c)) { (b: F) =>
            l.floatLatticeLaw.plusIsMonotone(a, b, c)
          }
        }
        p.property("∀ a, b: inject(a + b) ⊑ float(inject(a), inject(b))") =
          forAll(l.floatLatticeLaw.plusIsSound _)
        p.property("minus(a, ⊥) = ⊥ = minus(⊥, a)") =
          forAll(l.floatLatticeLaw.minusPreservesBottom _)
        p.property("∀ a, b, c: b ⊑ c ⇒ minus(a, b) ⊑ minus(a, c)") = forAll { (a: F, c: F) =>
          forAll(gen.le(c)) { (b: F) =>
            l.floatLatticeLaw.minusIsMonotone(a, b, c)
          }
        }
        p.property("∀ a, b: inject(a - b) ⊑ minus(inject(a), inject(b))") =
          forAll(l.floatLatticeLaw.minusIsSound _)
        p.property("times(a, ⊥) = ⊥ = times(⊥, a)") =
          forAll(l.floatLatticeLaw.timesPreservesBottom _)
        p.property("∀ a, b, c: b ⊑ c ⇒ times(a, b) ⊑ times(a, c)") = forAll { (a: F, c: F) =>
          forAll(gen.le(c)) { (b: F) =>
            l.floatLatticeLaw.timesIsMonotone(a, b, c)
          }
        }
        p.property("∀ a, b: inject(a + b) ⊑ times(inject(a), inject(b))") =
          forAll(l.floatLatticeLaw.timesIsSound _)
        p.property("div(a, ⊥) = ⊥ = div(⊥, a)") =
          forAll(l.floatLatticeLaw.divPreservesBottom _)
        p.property("∀ a, b, c: b ⊑ c ⇒ div(a, b) ⊑ div(a, c)") = forAll { (a: F, c: F) =>
          forAll(gen.le(c)) { (b: F) =>
            l.floatLatticeLaw.divIsMonotone(a, b, c)
          }
        }
        p.property("∀ a, b ≠ 0: inject(a / b) ⊑ div(inject(a), inject(b))") =
          forAll(l.floatLatticeLaw.divIsSound _)
        p.property("lt(a, ⊥) = ⊥ = lt(⊥, a)") =
          forAll(l.floatLatticeLaw.ltPreservesBottom _)
        p.property("∀ a, b, c: b ⊑ c ⇒ lt(a, b) ⊑ lt(a, c)") = forAll { (a: F, c: F) =>
          forAll(gen.le(c)) { (b: F) =>
            l.floatLatticeLaw.ltIsMonotone(a, b, c)
          }
        }
        p.property("∀ a, b ≠ 0: inject(a < b) ⊑ lt(inject(a), inject(b))") =
          forAll(l.floatLatticeLaw.ltIsSound _)
        p.property("toString(⊥) = ⊥") =
          l.floatLatticeLaw.toStringPreservesBottom
        p.property("∀ a, b: a ⊑ b ⇒ toString(a) ⊑ toString(b)") = forAll { (b: F) =>
          forAll(gen.le(b)) { (a: F) =>
            l.floatLatticeLaw.toStringIsMonotone(a, b)
          }
        }
        p.property("∀ a: inject(a.toString) ⊑ toString(inject(a))") =
          forAll(l.floatLatticeLaw.toStringIsSound _)
      }
    }
  }

  object charLattice {
    def laws[C](implicit l: CharLattice[C], gen: LatticeGenerator[C]): Properties = {
      implicit val arb = gen.anyArb
      newProperties("CharLattice") { p =>
        p.include(latticeElement.laws[C])
      }
    }
  }

  object symbolLattice {
    def laws[Sym](implicit l: SymbolLattice[Sym], gen: LatticeGenerator[Sym]): Properties = {
      implicit val arb = gen.anyArb
      newProperties("SymbolLattice") { p =>
        p.include(latticeElement.laws[Sym])
      }
    }
  }
}

abstract class Specification extends Properties("") {
  /* Taken from Scalaz's SpecLite class */
  override val name = this.getClass.getName.stripSuffix("$")
  def checkAll(props: Properties) {
    for ((name, prop) <- props.properties) yield {
      property(name) = prop
    }
  }
  def newProperties(name: String)(f: Properties => Unit): Properties = {
    val p = new Properties(name)
    f(p)
    p
  }
}

abstract class BoolLatticeTest[B : BoolLattice](gen: LatticeGenerator[B]) extends Specification {
  implicit val g = gen
  checkAll(LatticeProperties.boolLattice.laws[B])
}

abstract class StringLatticeTest[S : StringLattice](gen: LatticeGenerator[S]) extends Specification {
  implicit val g = gen
  checkAll(LatticeProperties.stringLattice.laws[S])
}

abstract class IntLatticeTest[I : IntLattice](gen: LatticeGenerator[I]) extends Specification {
  implicit val g = gen
  checkAll(LatticeProperties.intLattice.laws[I])
}

abstract class FloatLatticeTest[F : FloatLattice](gen: LatticeGenerator[F]) extends Specification {
  implicit val g = gen
  checkAll(LatticeProperties.floatLattice.laws[F])
}

abstract class CharLatticeTest[C : CharLattice](gen: LatticeGenerator[C]) extends Specification {
  implicit val g = gen
  checkAll(LatticeProperties.charLattice.laws[C])
}

abstract class SymbolLatticeTest[Sym : SymbolLattice](gen: LatticeGenerator[Sym]) extends Specification {
  implicit val g = gen
  checkAll(LatticeProperties.symbolLattice.laws[Sym])
}

class ConcreteBoolTest extends BoolLatticeTest(ConcreteBooleanGenerator)
class ConcreteStringTest extends StringLatticeTest(ConcreteStringGenerator)
class ConcreteIntTest extends IntLatticeTest(ConcreteIntGenerator)
class ConcreteFloatTest extends FloatLatticeTest(ConcreteFloatGenerator)
class ConcreteCharTest extends CharLatticeTest(ConcreteCharGenerator)
class ConcreteSymbolTest extends SymbolLatticeTest(ConcreteSymbolGenerator)

class TypeStringTest extends StringLatticeTest(TypeGenerator)
class TypeIntTest extends IntLatticeTest(TypeGenerator)
class TypeFloatTest extends FloatLatticeTest(TypeGenerator)
class TypeCharTest extends CharLatticeTest(TypeGenerator)
class TypeSymbolTest extends SymbolLatticeTest(TypeGenerator)

class ConstantPropagationStringTest extends StringLatticeTest(StringConstantPropagationGenerator)
class ConstantPropagationIntTest extends IntLatticeTest(IntegerConstantPropagationGenerator)
class ConstantPropagationFloatTest extends FloatLatticeTest(FloatConstantPropagationGenerator)
class ConstantPropagationCharTest extends CharLatticeTest(CharConstantPropagationGenerator)
class ConstantPropagationSymbolTest extends SymbolLatticeTest(SymbolConstantPropagationGenerator)
