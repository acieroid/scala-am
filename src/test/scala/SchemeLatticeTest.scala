import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import Prop.forAll

import scalaz._
import scalaz.Scalaz._

import SchemeOps._

abstract class SchemeLatticeProperties[L : IsSchemeLattice] extends Specification {
  val abs = implicitly[IsSchemeLattice[L]]
  implicit val arbitraryUnaryOp = Arbitrary(Gen.oneOf(UnaryOperator.values.toSeq))
  implicit val arbitraryBinaryOp = Arbitrary(Gen.oneOf(BinaryOperator.values.toSeq))

  /* TODO: use a better generator */
  implicit val arbitraryL = Arbitrary(Gen.oneOf(abs.inject(1), abs.inject(2), abs.inject(-1), abs.inject(1000),
    abs.inject(true), abs.inject(false), abs.inject("foo"), abs.injectSymbol("foo"),
    abs.join(abs.inject(1), abs.inject(2)), abs.join(abs.inject(true), abs.inject(false)),
    abs.join(abs.inject(1), abs.inject("foo")), abs.nil, abs.bottom))

  implicit val equalL = new Equal[L] {
    def equal(x: L, y: L) = x == y
  }

  checkAll(newProperties("SchemeLattice") { p =>
    p.property("satisfies Semigroup[_]'s associativity") = forAll(abs.schemeLatticeLaw.associative _)
    p.property("satisfies Monoid[_]'s left identity") = forAll(abs.schemeLatticeLaw.leftIdentity _)
    p.property("satisfies Monoid[_]'s right identity") = forAll(abs.schemeLatticeLaw.rightIdentity _)
    p.property("⊥ ⊑ ⊥)") = abs.schemeLatticeLaw.bottomSubsumesItself
    p.property("∀ x, ⊥ ⊑ x ∧ (x ≠ ⊥ ⇒ ¬(x ⊑ ⊥))") = forAll(abs.schemeLatticeLaw.bottomAlwaysSubsumed _)
    p.property("∀ x, y: y ⊑ x ⇒ (x ⊔ y) = x") = forAll(abs.schemeLatticeLaw.joinedWithSubsumesRemainsEqual _)
    p.property("∀ x, y: x ⊑ x ⊔ y ∧ y ⊑ x ⊔ y") = forAll(abs.schemeLatticeLaw.joinedSubsumes _)
    p.property("∀ x, y, z: x ⊑ x ⊔ y ⊔ z ∧ y ⊑ x ⊔ y ⊔ z ∧ z ⊑ x ⊔ y ⊔ z") = forAll(abs.schemeLatticeLaw.joinedSubsumes3 _)
    p.property("isTrue(inject(true)) ∧ isFalse(inject(false))") = abs.schemeLatticeLaw.injectBoolPreservesTruth
    p.property("!isTrue(⊥) ∧ !isFalse(⊥)") = abs.schemeLatticeLaw.bottomNeitherTrueNorFalse
    p.property("isTrue(⊤) ∧ isFalse(⊤)") = abs.schemeLatticeLaw.boolTopIsTrueAndFalse
    p.property("unOp(⊥) = ⊥") = forAll(abs.schemeLatticeLaw.unaryOpPreservesBottom _)
    p.property("∀ x, binOp(⊥, x) = ⊥ = binop(x, ⊥)") = forAll(abs.schemeLatticeLaw.binaryOpPreservesBottom _)
    p.property("isFalse(not(inject(true))) ∧ isTrue(not(inject(false)))") = abs.schemeLatticeLaw.notIsCorrect
    p.property("∀ b1, b2: if (b1 ∧ b2) isTrue(and(inject(b1), inject(b2))) else isFalse(and(inject(b1), inject(b2))") =
      forAll(abs.schemeLatticeLaw.andIsCorrect _)
    p.property("∀ b1, b2: if (b1 ∨ b2) isTrue(or(inject(b1), inject(b2))) else isFalse(or(inject(b1), inject(b2))") =
      forAll(abs.schemeLatticeLaw.orIsCorrect _)
    p.property("∀ x, y: x < y ⇒ (isTrue(lt(inject(x), inject(y))) ∧ isFalse(lt(inject(y), inject(x)))") =
      forAll(abs.schemeLatticeLaw.ltIsCorrect _)
    p.property("∀ x, y: (x = y ⇒ isTrue(eq(inject(x), inject(y)) ∧ (x ≠ y ⇒ isFalse(eq(inject(x), inject(y))))") =
      forAll(abs.schemeLatticeLaw.eqIsCorrect _)

    p.property("invalid operations return errors") = {
      val v1 = abs.inject(1)
      val v2 = abs.inject(true)
      def err(v: MayFail[L]): Boolean = v match {
        case _: MayFailSuccess[L] => false
        case _ => true
      }
      (err(abs.binaryOp(BinaryOperator.Plus)(v1, v2)) && err(abs.binaryOp(BinaryOperator.Plus)(v2, v1)) &&
        err(abs.binaryOp(BinaryOperator.Minus)(v1, v2)) && err(abs.binaryOp(BinaryOperator.Minus)(v2, v1)) &&
        err(abs.binaryOp(BinaryOperator.Times)(v1, v2)) && err(abs.binaryOp(BinaryOperator.Times)(v2, v1)) &&
        err(abs.binaryOp(BinaryOperator.Div)(v1, v2)) && err(abs.binaryOp(BinaryOperator.Div)(v2, v1)) &&
        err(abs.binaryOp(BinaryOperator.Modulo)(v1, v2)) && err(abs.binaryOp(BinaryOperator.Modulo)(v2, v1)) &&
        err(abs.binaryOp(BinaryOperator.Lt)(v1, v2)) && err(abs.binaryOp(BinaryOperator.Lt)(v2, v1)) &&
        err(abs.binaryOp(BinaryOperator.NumEq)(v1, v2)) && err(abs.binaryOp(BinaryOperator.NumEq)(v2, v1)))
    }
  })
}

class SchemeConcreteLatticeTest extends SchemeLatticeProperties[Lattices.ConcreteLattice.L]
class SchemeTypeLatticeTest extends SchemeLatticeProperties[Lattices.TypeLattice.L]
class SchemeBoundedIntLatticeTest extends SchemeLatticeProperties[Lattices.BoundedIntLattice.L]
class SchemeConstantPropagationLatticeTest extends SchemeLatticeProperties[Lattices.ConstantPropagationLattice.L]
