import org.scalatest._
import org.scalatest.prop._

abstract class Benchmarks[Exp : Expression, Addr : Address, Time : Timestamp](val lattice: SchemeLattice)
    extends FlatSpec with Matchers {
  type Abs = lattice.L
  implicit val abs = lattice.isSchemeLattice
  val sem: Semantics[Exp, lattice.L, Addr, Time]
  val machine: AbstractMachine[Exp, lattice.L, Addr, Time]

  def checkResult(file: String, expected: Abs): Unit = {
    val result = machine.eval(sem.parse(Util.fileContent(s"test/$file")), sem, false, None)
    assert(result.containsFinalValue(expected))
    println(s"${machine.name}, $file: ${result.numberOfStates}, ${result.time}")
  }
  def check(file: String, expected: Abs): Unit =
    file should s"eval to $expected" in { checkResult(file, expected) }

  val concrete = abs.name.contains("Concrete")

  check("blur.scm", abs.inject(true))
  check("count.scm", abs.inject("done"))
  if (!concrete) { check("cpstak.scm", abs.inject(6)) }
  check("fib.scm", abs.inject(3))
  check("eta.scm", abs.inject(false))
  check("fact.scm", abs.inject(120))
  check("gcipd.scm", abs.inject(36))
  check("inc.scm", abs.inject(4))
  check("kcfa2.scm", abs.inject(false))
  check("kcfa3.scm", abs.inject(false))
  check("mj09.scm", abs.inject(2))
  check("mut-rec.scm", abs.inject(true))
  check("rotate.scm", abs.inject("hallo"))
  check("sq.scm", abs.inject(9))
  check("sym.scm", abs.injectSymbol("foo"))
  check("ack.scm", abs.inject(4))
  check("collatz.scm", abs.inject(5))
  check("widen.scm", abs.inject(10))
  if (scala.util.Properties.envOrElse("SLOW_BENCHMARKS", "no") == "yes") {
    check("loop2.scm", abs.inject(550))
    check("rsa.scm", abs.inject(true))
    check("sat.scm", abs.inject(true))
    check("primtest.scm", abs.inject(1))
    if (!concrete) {
      // check("nqueens.scm", abs.inject(92)) // doesn't terminate in AAC !
      check("church.scm", abs.inject(true))
      check("boyer.scm", abs.inject(true))
      check("dderiv.scm", abs.inject(true))
      check("takl.scm", abs.inject(true))
    }
  }
}

abstract class OneResultTests[Exp : Expression, Addr : Address, Time : Timestamp](val lattice: SchemeLattice)
    extends FlatSpec with Matchers {
  type Abs = lattice.L
  implicit val abs = lattice.isSchemeLattice
  val sem: Semantics[Exp, lattice.L, Addr, Time]
  val machine: AbstractMachine[Exp, lattice.L, Addr, Time]

  def check(file: String, expected: Abs): Unit =
    file should s"have only one final state in concrete mode and return $expected" in {
      val program = fileContent(s"test/$file")
      program.isDefined should equal (true)
      val result = machine.eval(sem.parse(program.get), sem, false, None)
      result.finalValues.size should equal (1)
      result.finalValues.head should equal (expected)
    }

  check("blur.scm", abs.inject(true))
  check("count.scm", abs.inject("done"))
  // check("cpstak.scm", abs.inject(6)) disabled due to the time it takes
  check("fib.scm", abs.inject(3))
  check("eta.scm", abs.inject(false))
  check("fact.scm", abs.inject(120))
  check("gcipd.scm", abs.inject(36))
  check("inc.scm", abs.inject(4))
  check("kcfa2.scm", abs.inject(false))
  check("kcfa3.scm", abs.inject(false))
  check("mj09.scm", abs.inject(2))
  check("mut-rec.scm", abs.inject(true))
  check("rotate.scm", abs.inject("hallo"))
  check("sq.scm", abs.inject(9))
  check("sym.scm", abs.injectSymbol("foo"))
  check("ack.scm", abs.inject(4))
  check("collatz.scm", abs.inject(5))
  check("widen.scm", abs.inject(10))
  if (scala.util.Properties.envOrElse("SLOW_BENCHMARKS", "no") == "yes") {
    check("loop2.scm", abs.inject(550))
    check("rsa.scm", abs.inject(true))
    // check("sat.scm", abs.inject(true)) // doesn't terminate in AAC !
    check("primtest.scm", abs.inject(1))
    // check("nqueens.scm", abs.inject(92)) // doesn't terminate in AAC !
    check("church.scm", abs.inject(true))
    check("boyer.scm", abs.inject(true))
    check("dderiv.scm", abs.inject(true))
    check("takl.scm", abs.inject(true))
  }
}

abstract class AACBenchmarks[Addr : Address, Time : Timestamp](override val lattice: SchemeLattice)
    extends Benchmarks[SchemeExp, Addr, Time](lattice) {
  val sem = new SchemeSemantics[lattice.L, Addr, Time](new SchemePrimitives[Addr, lattice.L])
  val machine = new AAC[SchemeExp, lattice.L, Addr, Time]
}

abstract class AAMBenchmarks[Addr : Address, Time : Timestamp](override val lattice: SchemeLattice)
    extends Benchmarks[SchemeExp, Addr, Time](lattice) {
  val sem = new SchemeSemantics[lattice.L, Addr, Time](new SchemePrimitives[Addr, lattice.L])
  val machine = new AAM[SchemeExp, lattice.L, Addr, Time]
}

abstract class AAMGlobalStoreBenchmarks[Addr : Address, Time : Timestamp](override val lattice: SchemeLattice)
    extends Benchmarks[SchemeExp, Addr, Time](lattice) {
  val sem = new SchemeSemantics[lattice.L, Addr, Time](new SchemePrimitives[Addr, lattice.L])
  val machine = new AAMGlobalStore[SchemeExp, lattice.L, Addr, Time]
}

abstract class FreeBenchmarks[Addr : Address, Time : Timestamp](override val lattice: SchemeLattice)
    extends Benchmarks[SchemeExp, Addr, Time](lattice) {
  val sem = new SchemeSemantics[lattice.L, Addr, Time](new SchemePrimitives[Addr, lattice.L])
  val machine = new Free[SchemeExp, lattice.L, Addr, Time]
}

abstract class ConcreteMachineBenchmarks(override val lattice: SchemeLattice)
    extends Benchmarks[SchemeExp, ClassicalAddress.A, ConcreteTimestamp.T](lattice) {
  val sem = new SchemeSemantics[lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, lattice.L])
  val machine = new ConcreteMachine[SchemeExp, lattice.L, ClassicalAddress.A, ConcreteTimestamp.T]
}

class AACConcreteBenchmarks extends AACBenchmarks[ClassicalAddress.A, ConcreteTimestamp.T](new ConcreteLattice(true))
class AACTypeSetBenchmarks extends AACBenchmarks[ClassicalAddress.A, ZeroCFA.T](new TypeSetLattice(false))

class AAMConcreteBenchmarks extends AAMBenchmarks[ClassicalAddress.A, ConcreteTimestamp.T](new ConcreteLattice(true))
class AAMTypeSetBenchmarks extends AAMBenchmarks[ClassicalAddress.A, ZeroCFA.T](new TypeSetLattice(false))

class AAMGlobalStoreConcreteBenchmarks extends AAMGlobalStoreBenchmarks[ClassicalAddress.A, ConcreteTimestamp.T](new ConcreteLattice(true))
class AAMGlobalStoreTypeSetBenchmarks extends AAMGlobalStoreBenchmarks[ClassicalAddress.A, ZeroCFA.T](new TypeSetLattice(false))

class FreeConcreteBenchmarks extends FreeBenchmarks[ClassicalAddress.A, ConcreteTimestamp.T](new ConcreteLattice(true))
class FreeTypeSetBenchmarks extends FreeBenchmarks[ClassicalAddress.A, ZeroCFA.T](new TypeSetLattice(false))

class ConcreteMachineConcreteBenchmarks extends ConcreteMachineBenchmarks(new ConcreteLattice(true))

class AACOneResultTests extends OneResultTests[SchemeExp, ClassicalAddress.A, ConcreteTimestamp.T](new ConcreteLattice(true)) {
  val sem = new SchemeSemantics[lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, lattice.L])
  val machine = new AAC[SchemeExp, lattice.L, ClassicalAddress.A, ConcreteTimestamp.T]
}

class AAMOneResultTests extends OneResultTests[SchemeExp, ClassicalAddress.A, ConcreteTimestamp.T](new ConcreteLattice(true)) {
  val sem = new SchemeSemantics[lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, lattice.L])
  val machine = new AAM[SchemeExp, lattice.L, ClassicalAddress.A, ConcreteTimestamp.T]
}

class FreeOneResultTests extends OneResultTests[SchemeExp, ClassicalAddress.A, ConcreteTimestamp.T](new ConcreteLattice(true)) {
  val sem = new SchemeSemantics[lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, lattice.L])
  val machine = new Free[SchemeExp, lattice.L, ClassicalAddress.A, ConcreteTimestamp.T]
}

class ConcreteMachineOneResultTests extends OneResultTests[SchemeExp, ClassicalAddress.A, ConcreteTimestamp.T](new ConcreteLattice(true)) {
  val sem = new SchemeSemantics[lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, lattice.L])
  val machine = new ConcreteMachine[SchemeExp, lattice.L, ClassicalAddress.A, ConcreteTimestamp.T]
}
