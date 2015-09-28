import org.scalatest._
import org.scalatest.prop._

abstract class Benchmarks[Abs, Addr](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
  addr: Address[Addr], addri: AddressInjection[Addr])
    extends FlatSpec with Matchers {
  def checkResult(file: String, expected: Abs): Unit

  "blur.scm" should "eval to #t" in { checkResult("blur.scm", absi.inject(true)) }
  "count.scm" should "eval to \"done\"" in { checkResult("count.scm", absi.inject("done")) }
  "cpstak.scm" should "eval to 6" in { checkResult("cpstak.scm", absi.inject(6)) }
  "fib.scm" should "eval to 3" in { checkResult("fib.scm", absi.inject(3)) }
  "eta.scm" should "eval to #f" in { checkResult("eta.scm", absi.inject(false)) }
  "fact.scm" should "eval to 120" in { checkResult("fact.scm", absi.inject(120)) }
  "gcipd.scm" should "eval to 36" in { checkResult("gcipd.scm", absi.inject(36)) }
  "inc.scm" should "eval to 4" in { checkResult("inc.scm", absi.inject(4)) }
  "kcfa2.scm" should "eval to #f" in { checkResult("kcfa2.scm", absi.inject(false)) }
  "kcfa3.scm" should "eval to #f" in { checkResult("kcfa3.scm", absi.inject(false)) }
  "loop2.scm" should "eval to 550" in { checkResult("loop2.scm", absi.inject(550)) }
  "mj09.scm" should "eval to 2" in { checkResult("mj09.scm", absi.inject(2)) }
  "mut-rec.scm" should "eval to #t" in { checkResult("mut-rec.scm", absi.inject(true)) }
  "rotate.scm" should "eval to \"hallo\"" in { checkResult("rotate.scm", absi.inject("hallo")) }
  "sq.scm" should "eval to 9" in { checkResult("sq.scm", absi.inject(9)) }
  "sym.scm" should "eval to 'foo" in { checkResult("sym.scm", absi.injectSymbol("foo")) }
  "rsa.scm" should "eval to #t" in { checkResult("rsa.scm", absi.inject(true)) }
  "sat.scm" should "eval to #t" in { checkResult("sat.scm", absi.inject(true)) }
  "primtest.scm" should "eval to 1" in { checkResult("primtest.scm", absi.inject(true)) }
}

abstract class AACBenchmarks[Abs, Addr](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
  addr: Address[Addr], addri: AddressInjection[Addr])
    extends Benchmarks[Abs, Addr] {
  val machine = new AAC[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])

  def checkResult(file: String, expected: Abs) = {
    println(s"Testing $file")
    val result = machine.eval(Scheme.parse(s"test/$file"), None)
    assert(result.exists((st: machine.State) => st.control match {
      case machine.ControlKont(v) => abs.subsumes(v, expected)
      case _ => false
    }))
  }
}

abstract class AAMBenchmarks[Abs, Addr](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
  addr: Address[Addr], addri: AddressInjection[Addr])
    extends Benchmarks[Abs, Addr] {
  val machine = new AAM[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])

  def checkResult(file: String, expected: Abs) = {
    println(s"Testing $file")
    val result = machine.eval(Scheme.parse(s"test/$file"), None)
    assert(result.exists((st: machine.State) => st.control match {
      case machine.ControlKont(v) => abs.subsumes(v, expected)
      case _ => false
    }))
  }
}

abstract class FreeBenchmarks[Abs, Addr](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
  addr: Address[Addr], addri: AddressInjection[Addr])
    extends Benchmarks[Abs, Addr] {
  val machine = new Free[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])

  def checkResult(file: String, expected: Abs) = {
    println(s"Testing $file")
    val result = machine.eval(Scheme.parse(s"test/$file"), None)
    assert(result.exists((st: machine.State) => st.control match {
      case machine.ControlKont(v) => abs.subsumes(v, expected)
      case _ => false
    }))
  }

}

/* Concrete tests are disabled because of cpstak takes too much time to compute since it requires more than 75k recursive calls */
/* Type tests are disabled because they fail due to their inability to support a join between a closure and other abstract values */
//class AACConcreteTest extends AACFlatSpec[AbstractConcrete, ConcreteAddress]
//class AACTypeTest extends AACFlatSpec[AbstractType, ClassicalAddress]
class AACTypeSetTest extends AACBenchmarks[AbstractTypeSet, ClassicalAddress]

//class AAMConcreteTest extends AAMFlatSpec[AbstractConcrete, ConcreteAddress]
//class AAMTypeTest extends AAMFlatSpec[AbstractType, ClassicalAddress]
class AAMTypeSetTest extends AAMBenchmarks[AbstractTypeSet, ClassicalAddress]

//class FreeConcreteTest extends FreeFlatSpec[AbstractConcrete, ConcreteAddress]
//class FreeTypeTest extends FreeFlatSpec[AbstractType, ClassicalAddress]
class FreeTypeSetTest extends FreeBenchmarks[AbstractTypeSet, ClassicalAddress]
