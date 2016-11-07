import org.scalatest._
import org.scalatest.prop._
import org.scalatest.prop.TableDrivenPropertyChecks._

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import Prop.forAll

import scalaz.{Plus => _, _}
import scalaz.Scalaz._

import SchemeOps._

abstract class LatticePropSpec(val lattice: SchemeLattice)
    extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with TableDrivenPropertyChecks {
  type Abs = lattice.L
  val abs = lattice.isSchemeLattice
  property("lattice should preserve boolean value and correctly implement not") {
    forAll { (b: Boolean) => {
      val v = abs.inject(b)
      if (b) assert(abs.isTrue(v)) else assert(abs.isFalse(v))
      val nottest = for { notv <- abs.unaryOp(Not)(v) } yield if (b) { abs.isFalse(notv) } else { abs.isTrue(notv) }
      assert(nottest == true.point[MayFail])
    }}
  }
  property("lattice should correctly implement boolean operations") {
    forAll { (b1: Boolean, b2: Boolean) => {
      val v1 = abs.inject(b1)
      val v2 = abs.inject(b2)
      if (b1 && b2) assert(abs.isTrue(abs.and(v1, v2))) else assert(abs.isFalse(abs.and(v1, v2)))
      if (b1 || b2) assert(abs.isTrue(abs.or(v1, v2))) else assert(abs.isFalse(abs.and(v1, v2)))
    }}
  }
  property("lattice should correctly implement numerical comparisons") {
    forAll { (n1: Int, n2: Int) => {
      val v1 = abs.inject(n1)
      val v2 = abs.inject(n2)
      val lttest = for { lt <- abs.binaryOp(Lt)(v1, v2) } yield if (n1 < n2) { abs.isTrue(lt) } else { abs.isFalse(lt) }
      assert(lttest == true.point[MayFail])

      val eqtest = for { eq <- abs.binaryOp(NumEq)(v1, v2) } yield if (n1 == n2) { abs.isTrue(eq) } else { abs.isFalse(eq) }
      assert(eqtest == true.point[MayFail])
    }}
  }
  def err(v: MayFail[Abs]): Unit = v match {
    case _: MayFailSuccess[Abs] => assert(false)
    case _ => assert(true)
  }
  property("lattice should report errors on invalid operations") {
    val v1 = abs.inject(1)
    val v2 = abs.inject(true)
    err(abs.binaryOp(Plus)(v1, v2)); err(abs.binaryOp(Plus)(v2, v1))
    err(abs.binaryOp(Minus)(v1, v2)); err(abs.binaryOp(Minus)(v2, v1))
    err(abs.binaryOp(Times)(v1, v2)); err(abs.binaryOp(Times)(v2, v1))
    err(abs.binaryOp(Div)(v1, v2)); err(abs.binaryOp(Div)(v2, v1))
    err(abs.binaryOp(Modulo)(v1, v2)); err(abs.binaryOp(Modulo)(v2, v1))
    err(abs.binaryOp(Lt)(v1, v2)); err(abs.binaryOp(Lt)(v2, v1))
    err(abs.binaryOp(NumEq)(v1, v2)); err(abs.binaryOp(NumEq)(v2, v1))
  }
  property("bottom should be subsumed by any other value") {
    val values = Table(
      ("v"),
      (abs.inject(1)),
      (abs.inject(2)),
      (abs.inject(true)),
      (abs.inject(false)),
      (abs.inject("foo")),
      (abs.injectSymbol("foo")))
    forAll (values) { (v: Abs) =>
      assert(abs.subsumes(v, abs.bottom))
      assert(!abs.subsumes(abs.bottom, v))
    }
  }
  property("bottom should subsume itself") {
    assert(abs.subsumes(abs.bottom, abs.bottom))
  }
}

abstract class JoinLatticePropSpec(lattice: SchemeLattice)
    extends LatticePropSpec(lattice) {
  property("lattice should join values correctly or raise a CannotJoin error") {
    val bot = abs.bottom
    val t = abs.inject(true)
    val f = abs.inject(false)
    val tf = abs.join(t, f)
    val tf2 = abs.join(tf, bot)
    assert(abs.isTrue(tf)); assert(abs.isFalse(tf))
    assert(abs.isTrue(tf2)); assert(abs.isFalse(tf2));
    assert(abs.subsumes(tf, tf2)); assert(abs.subsumes(tf2, tf)); assert(tf.equals(tf2))
  }
  property("{#t, #f} joined with {#f} should give {#t, #f}") {
    /* bug detected on commit 1a31d78 */
    val tf = abs.join(abs.inject(true), abs.inject(false))
    val f = abs.inject(false)
    val tff = abs.join(f, tf)
    assert(abs.isTrue(tff)); assert(abs.isFalse(tff))
  }
  property("{#t, Str, Int} should subsume Str") {
    /* bug detected on commit 7546a519 */
    val str = abs.inject("foo")
    val t = abs.inject(true)
    val int = abs.inject(1000)
    val joined = abs.join(int, abs.join(t, str))
    assert(abs.subsumes(joined, str))
  }
}


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

class ConcreteBoolTest extends BoolLatticeTest(ConcreteBooleanGenerator)(ConcreteBoolean.isBoolean)
class ConcreteStringTest extends StringLatticeTest(ConcreteStringGenerator)(ConcreteString.isString)
class ConcreteIntTest extends IntLatticeTest(ConcreteIntegerGenerator)(ConcreteInteger.isInteger)
class ConcreteFloatTest extends FloatLatticeTest(ConcreteFloatGenerator)(ConcreteFloat.isFloat)
class ConcreteCharTest extends CharLatticeTest(ConcreteCharGenerator)(ConcreteChar.isChar)
class ConcreteSymbolTest extends SymbolLatticeTest(ConcreteSymbolGenerator)(ConcreteSymbol.isSymbol)

class TypeStringTest extends StringLatticeTest(TypeGenerator)(Type.typeIsString)
class TypeIntTest extends IntLatticeTest(TypeGenerator)(Type.typeIsInteger)
class TypeFloatTest extends FloatLatticeTest(TypeGenerator)(Type.typeIsFloat)
class TypeCharTest extends CharLatticeTest(TypeGenerator)(Type.typeIsChar)
class TypeSymbolTest extends SymbolLatticeTest(TypeGenerator)(Type.typeIsSymbol)

class ConstantPropagationStringTest extends StringLatticeTest(StringConstantPropagationGenerator)(StringConstantPropagation.isString)
class ConstantPropagationIntTest extends IntLatticeTest(IntegerConstantPropagationGenerator)(IntegerConstantPropagation.isInteger)
class ConstantPropagationFloatTest extends FloatLatticeTest(FloatConstantPropagationGenerator)(FloatConstantPropagation.isFloat)
class ConstantPropagationCharTest extends CharLatticeTest(CharConstantPropagationGenerator)(CharConstantPropagation.isChar)
class ConstantPropagationSymbolTest extends SymbolLatticeTest(SymbolConstantPropagationGenerator)(SymbolConstantPropagation.isSymbol)

class ConcreteCountingTest extends LatticePropSpec(new ConcreteLattice(true))
class ConcreteNoCountingTest extends JoinLatticePropSpec(new ConcreteLattice(false))
class TypeSetCountingTest extends JoinLatticePropSpec(new TypeSetLattice(true))
class TypeSetNoCountingTest extends JoinLatticePropSpec(new TypeSetLattice(false))
class BoundedIntCountingTest extends JoinLatticePropSpec(new BoundedIntLattice(100, true))
class BoundedIntNoCountingTest extends JoinLatticePropSpec(new BoundedIntLattice(100, false))
class ConstantPropagationCountingTest extends JoinLatticePropSpec(new ConstantPropagationLattice(true))
class ConstantPropagationNoCountingTest extends JoinLatticePropSpec(new ConstantPropagationLattice(false))

class CSchemeConcreteCountingTest extends LatticePropSpec(new CSchemeConcreteLattice(true))
class CSchemeConcreteNoCountingTest extends JoinLatticePropSpec(new CSchemeConcreteLattice(false))
class CSchemeTypeSetCountingTest extends JoinLatticePropSpec(new CSchemeTypeSetLattice(true))
class CSchemeTypeSetNoCountingTest extends JoinLatticePropSpec(new CSchemeTypeSetLattice(false))
class CSchemeBoundedIntCountingTest extends JoinLatticePropSpec(new CSchemeBoundedIntLattice(100, true))
class CSchemeBoundedIntNoCountingTest extends JoinLatticePropSpec(new CSchemeBoundedIntLattice(100, false))
class CSchemeConstantPropagationCountingTest extends JoinLatticePropSpec(new CSchemeConstantPropagationLattice(true))
class CSchemeConstantPropagationNoCountingTest extends JoinLatticePropSpec(new CSchemeConstantPropagationLattice(false))
