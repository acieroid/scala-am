import org.scalatest._
import org.scalatest.prop._

abstract class AACFlatSpec[Abs, Addr](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
                                      addr: Address[Addr], addri: AddressInjection[Addr])
         extends FlatSpec with Matchers {
  val machine = new AAC[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])

  def loadScheme(path: String): SchemeExp = {
    val f = scala.io.Source.fromFile(path)
    val content = f.getLines.mkString("\n")
    f.close()
    return Scheme.compile(SExpParser.parse(content))
  }

  def checkResult(file: String, expected: Abs) = {
    val result = machine.eval(loadScheme(s"test/$file"), None)
    assert(result.exists((st: machine.State) => st.control match {
      case machine.ControlKont(v) => abs.subsumes(v, expected)
      case _ => false
    }))
  }

  "blur.scm" should "eval to #t" in { checkResult("blur.scm", absi.inject(true)) }
  "count.scm" should "eval to \"done\"" in { checkResult("count.scm", absi.inject("done")) }
  "cpstak.scm" should "eval to 6" in { checkResult("cpstak.scm", absi.inject(6)) }
  "fib.scm" should "eval to 3" in { checkResult("fib.scm", absi.inject(3)) }
  "eta.scm" should "eval to #f" in { checkResult("eta.scm", absi.inject(false)) }
  "fact.scm" should "eval to 120" in { checkResult("fact.scm", absi.inject(120)) }
  "gcipd.scm" should "eval to 36" in { checkResult("gcipd.scm", absi.inject(36)) }
  "inc.scm" should "eval to 2" in { checkResult("inc.scm", absi.inject(2)) }
  "kcfa2.scm" should "eval to #f" in { checkResult("kcfa2.scm", absi.inject(false)) }
  "kcfa3.scm" should "eval to #f" in { checkResult("kcfa3.scm", absi.inject(false)) }
  "loop2.scm" should "eval to 550" in { checkResult("loop2.scm", absi.inject(550)) }
  "mj09.scm" should "eval to 2" in { checkResult("mj09.scm", absi.inject(2)) }
  "mut-rec.scm" should "eval to #t" in { checkResult("mut-rec.scm", absi.inject(true)) }
  "rotate.scm" should "eval to \"hallo\"" in { checkResult("rotate.scm", absi.inject("hallo")) }
  "sq.scm" should "eval to 9" in { checkResult("sq.scm", absi.inject(9)) }
  "sym.scm" should "eval to 'foo" in { checkResult("sym.scm", absi.injectSymbol("foo")) }
}

abstract class AAMFlatSpec[Abs, Addr](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
                                      addr: Address[Addr], addri: AddressInjection[Addr])
         extends FlatSpec with Matchers {
  val machine = new AAM[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])

  def loadScheme(path: String): SchemeExp = {
    val f = scala.io.Source.fromFile(path)
    val content = f.getLines.mkString("\n")
    f.close()
    return Scheme.compile(SExpParser.parse(content))
  }

  def checkResult(file: String, expected: Abs) = {
    val result = machine.eval(loadScheme(s"test/$file"), None)
    assert(result.exists((st: machine.State) => st.control match {
      case machine.ControlKont(v) => abs.subsumes(v, expected)
      case _ => false
    }))
  }

  "blur.scm" should "eval to #t" in { checkResult("blur.scm", absi.inject(true)) }
  "count.scm" should "eval to \"done\"" in { checkResult("count.scm", absi.inject("done")) }
  "cpstak.scm" should "eval to 6" in { checkResult("cpstak.scm", absi.inject(6)) }
  "fib.scm" should "eval to 3" in { checkResult("fib.scm", absi.inject(3)) }
  "eta.scm" should "eval to #f" in { checkResult("eta.scm", absi.inject(false)) }
  "fact.scm" should "eval to 120" in { checkResult("fact.scm", absi.inject(120)) }
  "gcipd.scm" should "eval to 36" in { checkResult("gcipd.scm", absi.inject(36)) }
  "inc.scm" should "eval to 2" in { checkResult("inc.scm", absi.inject(2)) }
  "kcfa2.scm" should "eval to #f" in { checkResult("kcfa2.scm", absi.inject(false)) }
  "kcfa3.scm" should "eval to #f" in { checkResult("kcfa3.scm", absi.inject(false)) }
  "loop2.scm" should "eval to 550" in { checkResult("loop2.scm", absi.inject(550)) }
  "mj09.scm" should "eval to 2" in { checkResult("mj09.scm", absi.inject(2)) }
  "mut-rec.scm" should "eval to #t" in { checkResult("mut-rec.scm", absi.inject(true)) }
  "rotate.scm" should "eval to \"hallo\"" in { checkResult("rotate.scm", absi.inject("hallo")) }
  "sq.scm" should "eval to 9" in { checkResult("sq.scm", absi.inject(9)) }
  "sym.scm" should "eval to 'foo" in { checkResult("sym.scm", absi.injectSymbol("foo")) }
}

abstract class FreeFlatSpec[Abs, Addr](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
                                      addr: Address[Addr], addri: AddressInjection[Addr])
         extends FlatSpec with Matchers {
  val machine = new Free[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])

  def loadScheme(path: String): SchemeExp = {
    val f = scala.io.Source.fromFile(path)
    val content = f.getLines.mkString("\n")
    f.close()
    return Scheme.compile(SExpParser.parse(content))
  }

  def checkResult(file: String, expected: Abs) = {
    val result = machine.eval(loadScheme(s"test/$file"), None)
    assert(result.R.exists((conf: machine.Configuration) => conf.control match {
      case machine.ControlKont(v) => abs.subsumes(v, expected)
      case _ => false
    }))
  }

  "blur.scm" should "eval to #t" in { checkResult("blur.scm", absi.inject(true)) }
  "count.scm" should "eval to \"done\"" in { checkResult("count.scm", absi.inject("done")) }
  "cpstak.scm" should "eval to 6" in { checkResult("cpstak.scm", absi.inject(6)) }
  "fib.scm" should "eval to 3" in { checkResult("fib.scm", absi.inject(3)) }
  "eta.scm" should "eval to #f" in { checkResult("eta.scm", absi.inject(false)) }
  "fact.scm" should "eval to 120" in { checkResult("fact.scm", absi.inject(120)) }
  "gcipd.scm" should "eval to 36" in { checkResult("gcipd.scm", absi.inject(36)) }
  "inc.scm" should "eval to 2" in { checkResult("inc.scm", absi.inject(2)) }
  "kcfa2.scm" should "eval to #f" in { checkResult("kcfa2.scm", absi.inject(false)) }
  "kcfa3.scm" should "eval to #f" in { checkResult("kcfa3.scm", absi.inject(false)) }
  "loop2.scm" should "eval to 550" in { checkResult("loop2.scm", absi.inject(550)) }
  "mj09.scm" should "eval to 2" in { checkResult("mj09.scm", absi.inject(2)) }
  "mut-rec.scm" should "eval to #t" in { checkResult("mut-rec.scm", absi.inject(true)) }
  "rotate.scm" should "eval to \"hallo\"" in { checkResult("rotate.scm", absi.inject("hallo")) }
  "sq.scm" should "eval to 9" in { checkResult("sq.scm", absi.inject(9)) }
  "sym.scm" should "eval to 'foo" in { checkResult("sym.scm", absi.injectSymbol("foo")) }
}

//class AACConcreteTest extends AACFlatSpec[AbstractConcrete, ConcreteAddress]
//class AACTypeTest extends AACFlatSpec[AbstractType, ClassicalAddress]
class AACTypeSetTest extends AACFlatSpec[AbstractTypeSet, ClassicalAddress]

//class AAMConcreteTest extends AAMFlatSpec[AbstractConcrete, ConcreteAddress]
//class AAMTypeTest extends AAMFlatSpec[AbstractType, ClassicalAddress]
class AAMTypeSetTest extends AAMFlatSpec[AbstractTypeSet, ClassicalAddress]

//class FreeConcreteTest extends FreeFlatSpec[AbstractConcrete, ConcreteAddress]
//class FreeTypeTest extends FreeFlatSpec[AbstractType, ClassicalAddress]
class FreeTypeSetTest extends FreeFlatSpec[AbstractTypeSet, ClassicalAddress]
