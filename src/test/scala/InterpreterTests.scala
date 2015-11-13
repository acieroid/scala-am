import org.scalatest._
import org.scalatest.prop._

abstract class Benchmarks[Exp : Expression, Abs : AbstractValue, Addr : Address]
    extends FlatSpec with Matchers {
  val abs = implicitly[AbstractValue[Abs]]
  val sem: Semantics[Exp, Abs, Addr]
  val machine: AbstractMachine[Exp, Abs, Addr]

  def checkResult(file: String, expected: Abs): Unit = {
    val result = machine.eval(sem.parse(Main.fileContent(s"test/$file")), sem, false)
    assert(result.containsFinalValue(expected))
    println(s"${machine.name}, $file: ${result.numberOfStates}, ${result.time}")
  }
  def check(file: String, expected: Abs): Unit =
    file should s"eval to $expected" in { checkResult(file, expected) }

  check("blur.scm", abs.inject(true))
  check("count.scm", abs.inject("done"))
  check("cpstak.scm", abs.inject(6))
  check("fib.scm", abs.inject(3))
  check("eta.scm", abs.inject(false))
  check("fact.scm", abs.inject(120))
  check("gcipd.scm", abs.inject(36))
  check("inc.scm", abs.inject(4))
  check("kcfa2.scm", abs.inject(false))
  check("kcfa3.scm", abs.inject(false))
  check("loop2.scm", abs.inject(550))
  check("mj09.scm", abs.inject(2))
  check("mut-rec.scm", abs.inject(true))
  check("rotate.scm", abs.inject("hallo"))
  check("sq.scm", abs.inject(9))
  check("sym.scm", abs.injectSymbol("foo"))
  check("ack.scm", abs.inject(4))
  check("collatz.scm", abs.inject(5))
  check("widen.scm", abs.inject(10))
  /* The following tests are disabled because they can take a long time to evaluate */
  // check("rsa.scm", abs.inject(true))
  // check("sat.scm", abs.inject(true))
  // check("primtest.scm", abs.inject(1))
  // check("nqueens.scm", abs.inject(92))
  // check("church.scm", abs.inject(true))
  // check("boyer.scm", abs.inject(true))
  // check("dderiv.scm", abs.inject(true))
  // check("takl.scm", abs.inject(true))
}

abstract class OneResultTests[Exp : Expression, Abs : AbstractValue, Addr : Address]
    extends FlatSpec with Matchers {
  val abs = implicitly[AbstractValue[Abs]]
  val sem: Semantics[Exp, Abs, Addr]
  val machine: AbstractMachine[Exp, Abs, Addr]

  def check(file: String, expected: Abs): Unit =
    file should s"have only one final state in concrete mode and return $expected" in {
      val result = machine.eval(sem.parse(Main.fileContent(s"test/$file")), sem, false)
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
  check("loop2.scm", abs.inject(550))
  check("mj09.scm", abs.inject(2))
  check("mut-rec.scm", abs.inject(true))
  check("rotate.scm", abs.inject("hallo"))
  check("sq.scm", abs.inject(9))
  check("sym.scm", abs.injectSymbol("foo"))
  check("ack.scm", abs.inject(4))
  check("collatz.scm", abs.inject(5))
  check("widen.scm", abs.inject(10))
  /* Disabled for slower tests */
  // check("rsa.scm", abs.inject(true))
  // check("sat.scm", abs.inject(true))
  // check("primtest.scm", abs.inject(1))
  // check("nqueens.scm", abs.inject(92))
  // check("church.scm", abs.inject(true))
  // check("boyer.scm", abs.inject(true))
  // check("dderiv.scm", abs.inject(true))
  // check("takl.scm", abs.inject(true))
}

abstract class AACBenchmarks[Abs : AbstractValue, Addr : Address]
    extends Benchmarks[SchemeExp, Abs, Addr] {
  val sem = new SchemeSemantics[Abs, Addr]
  val machine = new AAC[SchemeExp, Abs, Addr]
}

abstract class AAMBenchmarks[Abs : AbstractValue, Addr : Address]
    extends Benchmarks[SchemeExp, Abs, Addr] {
  val sem = new SchemeSemantics[Abs, Addr]
  val machine = new AAM[SchemeExp, Abs, Addr]
}

abstract class FreeBenchmarks[Abs : AbstractValue, Addr : Address]
    extends Benchmarks[SchemeExp, Abs, Addr] {
  val sem = new SchemeSemantics[Abs, Addr]
  val machine = new Free[SchemeExp, Abs, Addr]
}

abstract class ConcurrentAAMBenchmarks[Abs : AbstractValue, Addr : Address]
    extends Benchmarks[SchemeExp, Abs, Addr] {
  val sem = new SchemeSemantics[Abs, Addr]
  val machine = new ConcurrentAAM[SchemeExp, Abs, Addr]
}

/* Concrete tests are disabled because of cpstak takes too much time to compute since it requires more than 75k recursive calls */
/* Type tests are disabled because they fail due to their inability to support a join between a closure and other abstract values */
//class AACConcreteBenchmarks extends AACBenchmarks[AbstractConcrete, ConcreteAddress]
//class AACTypeBenchmarks extends AACBenchmarks[AbstractType, ClassicalAddress]
class AACTypeSetBenchmarks extends AACBenchmarks[AbstractTypeSet, ClassicalAddress]

//class AAMConcreteBenchmarks extends AAMBenchmarks[AbstractConcrete, ConcreteAddress]
//class AAMTypeBenchmarks extends AAMBenchmarks[AbstractType, ClassicalAddress]
class AAMTypeSetBenchmarks extends AAMBenchmarks[AbstractTypeSet, ClassicalAddress]

//class FreeConcreteBenchmarks extends FreeBenchmarks[AbstractConcrete, ConcreteAddress]
//class FreeTypeBenchmarks extends FreeBenchmarks[AbstractType, ClassicalAddress]
class FreeTypeSetBenchmarks extends FreeBenchmarks[AbstractTypeSet, ClassicalAddress]

class ConcurrentAAMTypeSetBenchmarks extends ConcurrentAAMBenchmarks[AbstractTypeSet, ClassicalAddress]

class AACOneResultTests extends OneResultTests[SchemeExp, AbstractConcrete, ConcreteAddress] {
  val sem = new SchemeSemantics[AbstractConcrete, ConcreteAddress]
  val machine = new AAC[SchemeExp, AbstractConcrete, ConcreteAddress]
}

class AAMOneResultTests extends OneResultTests[SchemeExp, AbstractConcrete, ConcreteAddress] {
  val sem = new SchemeSemantics[AbstractConcrete, ConcreteAddress]
  val machine = new AAM[SchemeExp, AbstractConcrete, ConcreteAddress]
}

class FreeOneResultTests extends OneResultTests[SchemeExp, AbstractConcrete, ConcreteAddress] {
  val sem = new SchemeSemantics[AbstractConcrete, ConcreteAddress]
  val machine = new Free[SchemeExp, AbstractConcrete, ConcreteAddress]
}

class ConcurrentAAMOneResultTests extends OneResultTests[SchemeExp, AbstractConcrete, ConcreteAddress] {
  val sem = new SchemeSemantics[AbstractConcrete, ConcreteAddress]
  val machine = new ConcurrentAAM[SchemeExp, AbstractConcrete, ConcreteAddress]
}
