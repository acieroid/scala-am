import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

abstract class StorePropSpec[Addr : Address, Abs : AbstractValue]
extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  val abs = implicitly[AbstractValue[Abs]]
  val addr = implicitly[Address[Addr]]

  val absint: Gen[Abs] = for { n: Int <- Gen.choose(-100, 100) } yield abs.inject(n)
  val absstr: Gen[Abs] = for { s <- Gen.alphaStr } yield abs.inject(s)
  val absbool: Gen[Abs] = Gen.oneOf(abs.inject(true), abs.inject(false))
  val abschar: Gen[Abs] = for { c <- Gen.alphaNumChar } yield abs.inject(c)
  val abssym: Gen[Abs] = for { s <- Gen.identifier } yield abs.injectSymbol(s)
  /* TODO: cons, vectors */

  val abstractValues: Gen[Abs] = Gen.oneOf(absint, absstr, absbool, abschar, abssym)

  property("lookup after extend yields same object") { forAll(abstractValues) { v =>
    val a = addr.primitive("foo")
    val store = Store.empty[Addr, Abs].extend(a, v)
    store.lookup(a) should equal (v)
  }}

  property("lookupBot returns bot for unbound values") {
    val a = addr.primitive("foo")
    val store = Store.empty[Addr, Abs]
    store.lookupBot(a) should equal (abs.bottom)
  }

  property("store with abstract counting extended twice or more remains the same") { forAll(abstractValues) { v =>
    val a = addr.primitive("foo")
    val store = Store.empty[Addr, Abs](true).extend(a, v).extend(a, v)
    store.extend(a, v) should equal (store)
  }}
}

class ConcreteStoreTest extends StorePropSpec[ClassicalAddress, ConcreteLattice.L]
class TypeStoreTest extends StorePropSpec[ClassicalAddress, TypeLattice.L]
class TypeSetStoreTest extends StorePropSpec[ClassicalAddress, TypeSetLattice.L]
