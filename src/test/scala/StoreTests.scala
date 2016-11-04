import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

abstract class StorePropSpec[Addr : Address](val lattice: SchemeLattice)
    extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  type Abs = lattice.L
  implicit val abs = lattice.isSchemeLattice

  val absint: Gen[Abs] = for { n: Int <- Gen.choose(-100, 100) } yield abs.inject(n)
  val absstr: Gen[Abs] = for { s <- Gen.alphaStr } yield abs.inject(s)
  val absbool: Gen[Abs] = Gen.oneOf(abs.inject(true), abs.inject(false))
  val abschar: Gen[Abs] = for { c <- Gen.alphaNumChar } yield abs.inject(c)
  val abssym: Gen[Abs] = for { s <- Gen.identifier } yield abs.injectSymbol(s)
  /* TODO: cons, vectors */

  val abstractValues: Gen[Abs] = Gen.oneOf(absint, absstr, absbool, abschar, abssym)

  property("lookup after extend yields same object") { forAll(abstractValues) { v =>
    val a = Address[Addr].primitive("foo")
    val store = Store.empty[Addr, Abs].extend(a, v)
    store.lookup(a) should equal (Some(v))
  }}

  property("lookupBot returns bot for unbound values") {
    val a = Address[Addr].primitive("foo")
    val store = Store.empty[Addr, Abs]
    store.lookup(a) should equal (None)
    store.lookupBot(a) should equal (abs.bottom)
  }

  property("store with abstract counting extended twice or more remains the same") { forAll(abstractValues) { v =>
    val a = Address[Addr].primitive("foo")
    val store = Store.empty[Addr, Abs](true).extend(a, v).extend(a, v)
    store.extend(a, v) should equal (store)
  }}
}

class ConcreteCountingStoreTest extends StorePropSpec[ClassicalAddress.A](new ConcreteLattice(true))
class ConcreteNoCountingStoreTest extends StorePropSpec[ClassicalAddress.A](new ConcreteLattice(false))
class TypeSetStoreTest extends StorePropSpec[ClassicalAddress.A](new TypeSetLattice(false))
class BoundedIntStoreTest extends StorePropSpec[ClassicalAddress.A](new BoundedIntLattice(100, false))
class ConstantPropagationStoreTest extends StorePropSpec[ClassicalAddress.A](new ConstantPropagationLattice(false))

class CSchemeConcreteCountingStoreTest extends StorePropSpec[ClassicalAddress.A](new CSchemeConcreteLattice(true))
class CSchemeConcreteNoCountingStoreTest extends StorePropSpec[ClassicalAddress.A](new CSchemeConcreteLattice(false))
class CSchemeTypeSetStoreTest extends StorePropSpec[ClassicalAddress.A](new CSchemeTypeSetLattice(false))
class CSchemeBoundedIntStoreTest extends StorePropSpec[ClassicalAddress.A](new CSchemeBoundedIntLattice(100, false))
class CSchemeConstantPropagationStoreTest extends StorePropSpec[ClassicalAddress.A](new CSchemeConstantPropagationLattice(false))
