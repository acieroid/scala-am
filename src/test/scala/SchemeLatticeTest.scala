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
    abs.join(abs.inject(1), abs.inject("foo")), abs.nil))

  implicit val equalL = new Equal[L] {
    def equal(x: L, y: L) = x == y
  }

  checkAll(newProperties("SchemeLattice") { p =>
    p.property("satisfies Semigroup[_]'s associativity") = forAll(abs.schemeLatticeLaw.associative _)
    p.property("satisfies Monoid[_]'s left identity") = forAll(abs.schemeLatticeLaw.leftIdentity _)
    p.property("satisfies Monoid[_]'s right identity") = forAll(abs.schemeLatticeLaw.rightIdentity _)
    p.property("⊥ ⊑ ⊥)") = abs.schemeLatticeLaw.bottomSubsumesItself
    p.property("∀ x, ⊥ ⊑ x ∧ (x ≠ ⊥ ⇒ ¬(x ⊑ ⊥))") = forAll { abs.schemeLatticeLaw.bottomAlwaysSubsumed _ }
    p.property("isTrue(inject(true)) ∧ isFalse(inject(false))") = abs.schemeLatticeLaw.injectBoolPreservesTruth
    p.property("!isTrue(⊥) ∧ !isFalse(⊥)") = abs.schemeLatticeLaw.bottomNeitherTrueNorFalse
    p.property("isTrue(⊤) ∧ isFalse(⊤)") = abs.schemeLatticeLaw.boolTopIsTrueAndFalse
    p.property("unOp(⊥) = ⊥") = forAll { (op: UnaryOperator) => abs.schemeLatticeLaw.unaryOpPreservesBottom(op) }
    p.property("∀ x, binOp(⊥, x) = ⊥ = binop(x, ⊥)") = forAll { (op: BinaryOperator, x: L) => abs.schemeLatticeLaw.binaryOpPreservesBottom(op, x) }
    p.property("isFalse(not(inject(true))) ∧ isTrue(not(inject(false)))") = abs.schemeLatticeLaw.notIsCorrect
    p.property("∀ b1, b2: if (b1 ∧ b2) isTrue(and(inject(b1), inject(b2))) else isFalse(and(inject(b1), inject(b2))") =
      forAll { (b1: Boolean, b2: Boolean) => abs.schemeLatticeLaw.andIsCorrect(b1, b2) }
    p.property("∀ b1, b2: if (b1 ∨ b2) isTrue(or(inject(b1), inject(b2))) else isFalse(or(inject(b1), inject(b2))") =
      forAll { (b1: Boolean, b2: Boolean) => abs.schemeLatticeLaw.orIsCorrect(b1, b2) }
  })
}

class SchemeConcreteLattice extends SchemeLatticeProperties[Lattices.ConcreteLattice.L]()
class SchemeTypeLattice extends SchemeLatticeProperties[Lattices.TypeLattice.L]()
class SchemeBoundedIntLattice extends SchemeLatticeProperties[Lattices.BoundedIntLattice.L]()

/* TODO: use ScalaCheck for (most of) this */
import org.scalatest._
import org.scalatest.prop._
abstract class LatticePropSpec(val lattice: SchemeLattice)
    extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with TableDrivenPropertyChecks {
  type Abs = lattice.L
  val abs = lattice.isSchemeLattice

  property("lattice should correctly implement numerical comparisons") {
    forAll { (n1: Int, n2: Int) => {
      val v1 = abs.inject(n1)
      val v2 = abs.inject(n2)
      val lttest = for { lt <- abs.binaryOp(BinaryOperator.Lt)(v1, v2) } yield if (n1 < n2) { abs.isTrue(lt) } else { abs.isFalse(lt) }
      assert(lttest.extract == Some(true))

      val eqtest = for { eq <- abs.binaryOp(BinaryOperator.NumEq)(v1, v2) } yield if (n1 == n2) { abs.isTrue(eq) } else { abs.isFalse(eq) }
      assert(eqtest.extract == Some(true))
    }}
  }
  def err(v: MayFail[Abs]): Unit = v match {
    case _: MayFailSuccess[Abs] => assert(false)
    case _ => assert(true)
  }
  property("lattice should report errors on invalid operations") {
    val v1 = abs.inject(1)
    val v2 = abs.inject(true)
    err(abs.binaryOp(BinaryOperator.Plus)(v1, v2));   err(abs.binaryOp(BinaryOperator.Plus)(v2, v1))
    err(abs.binaryOp(BinaryOperator.Minus)(v1, v2));  err(abs.binaryOp(BinaryOperator.Minus)(v2, v1))
    err(abs.binaryOp(BinaryOperator.Times)(v1, v2));  err(abs.binaryOp(BinaryOperator.Times)(v2, v1))
    err(abs.binaryOp(BinaryOperator.Div)(v1, v2));    err(abs.binaryOp(BinaryOperator.Div)(v2, v1))
    err(abs.binaryOp(BinaryOperator.Modulo)(v1, v2)); err(abs.binaryOp(BinaryOperator.Modulo)(v2, v1))
    err(abs.binaryOp(BinaryOperator.Lt)(v1, v2));     err(abs.binaryOp(BinaryOperator.Lt)(v2, v1))
    err(abs.binaryOp(BinaryOperator.NumEq)(v1, v2));  err(abs.binaryOp(BinaryOperator.NumEq)(v2, v1))
  }
  property("lattice should join values correctly") { // TODO: prop: anything joined with bottom should remain the same
    val bot = abs.bottom
    val t = abs.inject(true)
    val f = abs.inject(false)
    val tf = abs.join(t, f)
    val tf2 = abs.join(tf, bot)
    assert(abs.isTrue(tf)); assert(abs.isFalse(tf))
    assert(abs.isTrue(tf2)); assert(abs.isFalse(tf2));
    assert(abs.subsumes(tf, tf2)); assert(abs.subsumes(tf2, tf)); assert(tf.equals(tf2))
  }
  property("{#t, #f} joined with {#f} should give {#t, #f}") { // TODO: prop: anything joined with something it subsumes should remain the same
    /* bug detected on commit 1a31d78 */
    val tf = abs.join(abs.inject(true), abs.inject(false))
    val f = abs.inject(false)
    val tff = abs.join(f, tf)
    assert(abs.isTrue(tff)); assert(abs.isFalse(tff))
  }
  property("{#t, Str, Int} should subsume Str") { // TODO: prop: join(x, y) should subsume both x and y. Same with join(x, join(y, z))
    /* bug detected on commit 7546a519 */
    val str = abs.inject("foo")
    val t = abs.inject(true)
    val int = abs.inject(1000)
    val joined = abs.join(int, abs.join(t, str))
    assert(abs.subsumes(joined, str))
  }
}
