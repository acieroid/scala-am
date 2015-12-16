import org.scalatest._
import org.scalatest.prop._

import UnaryOperator._
import BinaryOperator._

abstract class LatticePropSpec[Abs : AbstractValue]
    extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  val abs = implicitly[AbstractValue[Abs]]
  property("lattice should preserve boolean value and correctly implement not") {
    forAll { (b: Boolean) =>
      try {
        val v = abs.inject(b)
        if (b) assert(abs.isTrue(v)) else assert(abs.isFalse(v))
        if (b) assert(abs.isFalse(abs.unaryOp(Not)(v))) else assert(abs.isTrue(abs.unaryOp(Not)(v)))
      } catch {
        case CannotJoin(_, _) => ()
      }
    }
  }
  property("lattice should correctly implement boolean operations") {
    forAll { (b1: Boolean, b2: Boolean) =>
      try {
        val v1 = abs.inject(b1)
        val v2 = abs.inject(b2)
        if (b1 && b2) assert(abs.isTrue(abs.and(v1, v2))) else assert(abs.isFalse(abs.and(v1, v2)))
        if (b1 || b2) assert(abs.isTrue(abs.or(v1, v2))) else assert(abs.isFalse(abs.and(v1, v2)))
      } catch {
        case CannotJoin(_, _) => ()
      }
    }
  }
  property("lattice should correctly implement numerical comparisons") {
    forAll { (n1: Int, n2: Int) =>
      try {
        val v1 = abs.inject(n1)
        val v2 = abs.inject(n2)
        if (n1 < n2) assert(abs.isTrue(abs.binaryOp(Lt)(v1, v2))) else assert(abs.isFalse(abs.binaryOp(Lt)(v1, v2)))
        if (n1 == n2) assert(abs.isTrue(abs.binaryOp(NumEq)(v1, v2))) else assert(abs.isFalse(abs.binaryOp(NumEq)(v1, v2)))
      } catch {
        case CannotJoin(_, _) => ()
      }
    }
  }
  property("lattice should report errors on invalid operations") {
    try {
      val v1 = abs.inject(1)
      val v2 = abs.inject(true)
      assert(abs.isError(abs.binaryOp(Plus)(v1, v2))); assert(abs.isError(abs.binaryOp(Plus)(v2, v1)))
      assert(abs.isError(abs.binaryOp(Minus)(v1, v2))); assert(abs.isError(abs.binaryOp(Minus)(v2, v1)))
      assert(abs.isError(abs.binaryOp(Times)(v1, v2))); assert(abs.isError(abs.binaryOp(Times)(v2, v1)))
      assert(abs.isError(abs.binaryOp(Div)(v1, v2))); assert(abs.isError(abs.binaryOp(Div)(v2, v1)))
      assert(abs.isError(abs.binaryOp(Modulo)(v1, v2))); assert(abs.isError(abs.binaryOp(Modulo)(v2, v1)))
      assert(abs.isError(abs.binaryOp(Lt)(v1, v2))); assert(abs.isError(abs.binaryOp(Lt)(v2, v1)))
      assert(abs.isError(abs.binaryOp(NumEq)(v1, v2))); assert(abs.isError(abs.binaryOp(NumEq)(v2, v1)))
    } catch {
      case CannotJoin(_, _) => ()
    }
  }
}

abstract class JoinLatticePropSpec[Abs : AbstractValue]
    extends LatticePropSpec[Abs] {
  property("lattice should join values correctly or raise a CannotJoin error") {
    try {
      val bot = abs.bottom
      val t = abs.inject(true)
      val f = abs.inject(false)
      val tf = abs.join(t, f)
      val tf2 = abs.join(tf, bot)
      assert(abs.isTrue(tf)); assert(abs.isFalse(tf))
      assert(abs.isTrue(tf2)); assert(abs.isFalse(tf2));
      assert(abs.subsumes(tf, tf2)); assert(abs.subsumes(tf2, tf)); assert(tf.equals(tf2))
    } catch {
      case CannotJoin(_, _) => ()
    }
  }
  property("{#t, #f} joined with {#f} should give {#t, #f}") {
    try {
      /* bug detected on commit 1a31d78 */
      val tf = abs.join(abs.inject(true), abs.inject(false))
      val f = abs.inject(false)
      val tff = abs.join(f, tf)
      assert(abs.isTrue(tff)); assert(abs.isFalse(tff))
    } catch {
      case CannotJoin(_, _) => ()
    }
  }
}

class ConcreteTest extends LatticePropSpec[ConcreteLattice.L]
class TypeTest extends JoinLatticePropSpec[TypeLattice.L]
class TypeSetTest extends JoinLatticePropSpec[TypeSetLattice.L]
