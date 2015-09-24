import org.scalatest._
import org.scalatest.prop._

class LatticeFlatSpec extends FlatSpec with Matchers {
  "AbstractTypeSet" should "not accept sets" in {
    val v = AbstractTypeSet.AbstractSet(Set())
    intercept[java.lang.IllegalArgumentException] {
      val v2 = AbstractTypeSet.AbstractSet(Set(v))
    }
  }
}

abstract class LatticePropSpec[Abs](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs])
         extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  property("lattice should preserve boolean value and correctly implement not") {
    forAll { (b: Boolean) =>
      val v = absi.inject(b)
      if (b) assert(abs.isTrue(v)) else assert(abs.isFalse(v))
      if (b) assert(abs.isFalse(abs.not(v))) else assert(abs.isTrue(abs.not(v)))
    }
  }
  property("lattice should correctly implement boolean operations") {
    forAll { (b1: Boolean, b2: Boolean) =>
      val v1 = absi.inject(b1)
      val v2 = absi.inject(b2)
      if (b1 && b2) assert(abs.isTrue(abs.and(v1, v2))) else assert(abs.isFalse(abs.and(v1, v2)))
      if (b1 || b2) assert(abs.isTrue(abs.or(v1, v2))) else assert(abs.isFalse(abs.and(v1, v2)))
    }
  }
  property("lattice should correctly implement numerical comparisons") {
    forAll { (n1: Int, n2: Int) =>
      val v1 = absi.inject(n1)
      val v2 = absi.inject(n2)
      if (n1 < n2) assert(abs.isTrue(abs.lt(v1, v2))) else assert(abs.isFalse(abs.lt(v1, v2)))
      if (n1 == n2) assert(abs.isTrue(abs.numEq(v1, v2))) else assert(abs.isFalse(abs.numEq(v1, v2)))
    }
  }
  property("lattice should report errors on invalid operations") {
    val v1 = absi.inject(1)
    val v2 = absi.inject(true)
    assert(abs.isError(abs.plus(v1, v2))); assert(abs.isError(abs.plus(v2, v1)))
    assert(abs.isError(abs.minus(v1, v2))); assert(abs.isError(abs.minus(v2, v1)))
    assert(abs.isError(abs.times(v1, v2))); assert(abs.isError(abs.times(v2, v1)))
    assert(abs.isError(abs.div(v1, v2))); assert(abs.isError(abs.div(v2, v1)))
    assert(abs.isError(abs.modulo(v1, v2))); assert(abs.isError(abs.modulo(v2, v1)))
    assert(abs.isError(abs.lt(v1, v2))); assert(abs.isError(abs.lt(v2, v1)))
    assert(abs.isError(abs.numEq(v1, v2))); assert(abs.isError(abs.numEq(v2, v1)))
    assert(abs.isError(abs.not(v1)))
    assert(abs.isError(abs.and(v1, v2))); assert(abs.isError(abs.and(v2, v1)))
    assert(abs.isError(abs.or(v1, v2))); assert(abs.isError(abs.or(v2, v1)))
  }
}

abstract class JoinLatticePropSpec[Abs](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs])
    extends LatticePropSpec[Abs] {
  property("lattice should join values correctly") {
    val bot = absi.bottom
    val t = absi.inject(true)
    val f = absi.inject(false)
    val tf = abs.join(t, f)
    val tf2 = abs.join(tf, bot)
    assert(abs.isTrue(tf)); assert(abs.isFalse(tf))
    assert(abs.isTrue(tf2)); assert(abs.isFalse(tf2));
    assert(abs.subsumes(tf, tf2)); assert(abs.subsumes(tf2, tf)); assert(tf.equals(tf2))
  }
  property("{#t, #f} joined with {#f} should give {#t, #f}") {
    /* bug detected on commit 1a31d78 */
    val tf = abs.join(absi.inject(true), absi.inject(false))
    val f = absi.inject(false)
    val tff = abs.join(f, tf)
    assert(abs.isTrue(tff)); assert(abs.isFalse(tff))
  }
}

class AbstractConcreteTest extends LatticePropSpec[AbstractConcrete]
class AbstractTypeTest extends JoinLatticePropSpec[AbstractType]
class AbstractTypeSetTest extends JoinLatticePropSpec[AbstractTypeSet] {
  import AbstractTypeSet._
}
