import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import Prop.forAll

import scalaz._
import scalaz.Scalaz._

import SchemeOps._

abstract class SchemeLatticeProperties[L : IsSchemeLattice] extends Specification {
  val abs = implicitly[IsSchemeLattice[L]]
  implicit val arbitraryUnaryOp = Arbitrary(Gen.oneOf(UnaryOperator.values.toSeq))
  implicit val arbitraryBinaryOp = Arbitrary(Gen.oneOf(BinaryOperator.values.toSeq))

  checkAll(newProperties("SchemeLattice") { p =>
    p.property("isTrue(inject(true)) ∧ isFalse(inject(false))") = abs.schemeLatticeLaw.injectBoolPreservesTruth
    p.property("!isTrue(⊥) ∧ !isFalse(⊥)") = abs.schemeLatticeLaw.bottomNeitherTrueNorFalse
    p.property("isTrue(⊤) ∧ isFalse(⊤)") = abs.schemeLatticeLaw.boolTopIsTrueAndFalse
    p.property("unOp(⊥) = ⊥") = forAll { (op: UnaryOperator) => abs.schemeLatticeLaw.unaryOpPreservesBottom(op) }
    p.property("isFalse(not(inject(true))) ∧ isTrue(not(inject(false)))") = abs.schemeLatticeLaw.notIsSound
  })
}

object ConcreteCountingLattice extends ConcreteLattice(true)
object ConcreteNoCountingLattice extends ConcreteLattice(false)
class SchemeConcreteLattice extends SchemeLatticeProperties[ConcreteCountingLattice.L]()(ConcreteCountingLattice.isSchemeLattice)
class SchemeConcreteLatticeNoCounting extends SchemeLatticeProperties[ConcreteNoCountingLattice.L]()(ConcreteNoCountingLattice.isSchemeLattice)
/* TODO: others */

/* TODO: use ScalaCheck for (most of) this */
import org.scalatest._
import org.scalatest.prop._
abstract class LatticePropSpec(val lattice: SchemeLattice)
    extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with TableDrivenPropertyChecks {
  type Abs = lattice.L
  val abs = lattice.isSchemeLattice

  property("lattice should correctly implement boolean operations") {
    forAll { (b1: Boolean, b2: Boolean) => {
      val v1 = abs.inject(b1)
      val v2 = abs.inject(b2)
      if (b1 && b2) assert(abs.isTrue(abs.and(v1, v2))) else assert(abs.isFalse(abs.and(v1, v2)))
      if (b1 || b2) assert(abs.isTrue(abs.or(v1, v2))) else assert(abs.isFalse(abs.or(v1, v2)))
    }}
  }
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
  property("unary operation on bottom returns bottom") {
    SchemeOps.UnaryOperator.values.foreach(op =>
      for { res <- abs.unaryOp(op)(abs.bottom) } yield
        assert(res == abs.bottom))
  }
  property("binary operation on bottom returns bottom") {
    val v = abs.inject(1)
    SchemeOps.BinaryOperator.values.foreach(op =>
      for {
        res1 <- abs.binaryOp(op)(abs.bottom, abs.bottom)
        res2 <- abs.binaryOp(op)(v, abs.bottom)
        res3 <- abs.binaryOp(op)(abs.bottom, v)
      } yield {
        assert(res1 == abs.bottom && res2 == abs.bottom && res3 == abs.bottom)
      })
  }
}

abstract class JoinLatticePropSpec(lattice: SchemeLattice)
    extends LatticePropSpec(lattice) {
  property("lattice should join values correctly") {
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
