import org.scalatest._
import org.scalatest.prop._
import org.scalatest.prop.TableDrivenPropertyChecks._

import UnaryOperator._
import BinaryOperator._

abstract class LatticePropSpec(val lattice: Lattice)
    extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with TableDrivenPropertyChecks {
  type Abs = lattice.L
  val abs = lattice.isAbstractValue
  property("lattice should preserve boolean value and correctly implement not") {
    forAll { (b: Boolean) =>
      try {
        val v = abs.inject(b)
        if (b) assert(abs.isTrue(v)) else assert(abs.isFalse(v))
        if (b) assert(abs.isFalse(abs.unaryOp(Not)(v))) else assert(abs.isTrue(abs.unaryOp(Not)(v)))
      } catch {
        case CannotJoin(_) => ()
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
        case CannotJoin(_) => ()
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
        case CannotJoin(_) => ()
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
      case CannotJoin(_) => ()
    }
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

abstract class JoinLatticePropSpec(lattice: Lattice)
    extends LatticePropSpec(lattice) {
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
      case CannotJoin(_) => ()
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
      case CannotJoin(_) => ()
    }
  }
  property("{#t, Str, Int} should subsume Str") {
    try {
      /* bug detected on commit 7546a519 */
      val str = abs.inject("foo")
      val t = abs.inject(true)
      val int = abs.inject(1000)
      val joined = abs.join(int, abs.join(t, str))
      assert(abs.subsumes(joined, str))
    } catch {
      case CannotJoin(_) => ()
    }
  }
  property("isError and isNotError are correct") {
    try {
      val err = abs.error(abs.inject("Error"))
      val noterr = abs.inject(true)
      val joined = abs.join(err, noterr)
      /* isError is true for values that contain errors only */
      assert(abs.isError(err))
      /* isNotError is true for values that do not contain any error */
      assert(abs.isNotError(noterr))
      /* For values that contain errors and non-errors, both are false */
      assert(!abs.isError(joined))
      assert(!abs.isNotError(joined))
    } catch {
      case CannotJoin(_) => ()
    }
  }
}

class ConcreteTest extends LatticePropSpec(ConcreteLattice)
class ConcreteNewCountingTest extends JoinLatticePropSpec(new ConcreteLatticeNew(true))
class ConcreteNewNoCountingTest extends JoinLatticePropSpec(new ConcreteLatticeNew(false))
class TypeSetCountingTest extends JoinLatticePropSpec(new TypeSetLattice(true))
class TypeSetNoCountingTest extends JoinLatticePropSpec(new TypeSetLattice(false))
class BoundedIntCountingTest extends JoinLatticePropSpec(new BoundedIntLattice(100, true))
class BoundedIntNoCountingTest extends JoinLatticePropSpec(new BoundedIntLattice(100, false))
