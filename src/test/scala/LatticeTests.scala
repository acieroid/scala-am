import AbstractConcrete._
import org.scalatest._
import prop._

class LatticeFlatSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "AbstractTypeSet" should "not accept sets" in {
    val v = AbstractTypeSet.AbstractSet(Set())
    intercept[java.lang.IllegalArgumentException] {
      val v2 = AbstractTypeSet.AbstractSet(Set(v))
    }
  }
}

class LatticePropSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  property("lattice should preserve boolean value and correctly implement not") {
    forAll { (b: Boolean) =>
      val v = AbstractConcreteInjection.inject(b)
      if (b) assert(v.isTrue) else assert(v.isFalse)
      if (b) assert(v.not.isFalse) else assert(v.not.isTrue)
    }
  }
  property("lattice should correctly implement boolean operations") {
    forAll { (b1: Boolean, b2: Boolean) =>
      val v1 = AbstractConcreteInjection.inject(b1)
      val v2 = AbstractConcreteInjection.inject(b2)
      v1.and(v2).isTrue should be (b1 && b2)
      v1.or(v2).isTrue should be (b1 || b2)
    }
  }
  property("lattice should correctly implement numerical comparisons") {
    forAll { (n1: Int, n2: Int) =>
      val v1 = AbstractConcreteInjection.inject(n1)
      val v2 = AbstractConcreteInjection.inject(n2)
      if (n1 < n2) assert(v1.lt(v2).isTrue) else assert(v1.lt(v2).isFalse)
      if (n1 == n2) assert(v1.numEq(v2).isTrue) else assert(v1.numEq(v2).isFalse)
    }
  }
  property("lattice should report errors on invalid operations") {
    val v1 = AbstractConcreteInjection.inject(1)
    val v2 = AbstractConcreteInjection.inject(true)
    assert(v1.plus(v2).isError); assert(v2.plus(v1).isError)
    assert(v1.minus(v2).isError); assert(v2.minus(v1).isError)
    assert(v1.times(v2).isError); assert(v2.times(v1).isError)
    assert(v1.div(v2).isError); assert(v2.div(v1).isError)
    assert(v1.lt(v2).isError); assert(v2.lt(v1).isError)
    assert(v1.numEq(v2).isError); assert(v2.numEq(v1).isError)
    assert(v1.not.isError)
    assert(v1.and(v2).isError); assert(v2.and(v1).isError)
    assert(v1.or(v2).isError); assert(v2.or(v1).isError)
  }
}
