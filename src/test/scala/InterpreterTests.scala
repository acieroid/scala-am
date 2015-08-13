import org.scalatest._
import org.scalatest.prop._

abstract class AACFlatSpec[Abs, Addr](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
                                      addr: Address[Addr], addri: AddressInjection[Addr])
         extends FlatSpec with Matchers {
  val aacScheme = new AAC[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])
  val aacANF = new AAC[Abs, Addr, ANFExp](new ANFSemantics[Abs, Addr])

  def loadScheme(path: String): SchemeExp = {
    val f = scala.io.Source.fromFile(path)
    val content = f.getLines.mkString("\n")
    f.close()
    return Scheme.compile(SExpParser.parse(content))
  }

  def checkResult(file: String, expected: Abs) = {
    val result = aacScheme.eval(loadScheme(s"test/$file"), None)
    assert(result.exists((st: aacScheme.State) => st.control match { case aacScheme.ControlKont(v) => abs.subsumes(v, expected) }))
  }

  "blur.scm" should "eval to #t" in {
    checkResult("blur.scm", absi.inject(true))
  }
  "count.scm" should "eval to \"done\"" in {
    checkResult("count.scm", absi.inject("done"))
  }
  "fib.scm" should "eval to 3" in {
    checkResult("fib.scm", absi.inject(3))
  }
}

abstract class AAMFlatSpec[Abs, Addr](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
                                      addr: Address[Addr], addri: AddressInjection[Addr])
         extends FlatSpec with Matchers {
  val aamScheme = new AAM[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])
  val aamANF = new AAM[Abs, Addr, ANFExp](new ANFSemantics[Abs, Addr])

  def loadScheme(path: String): SchemeExp = {
    val f = scala.io.Source.fromFile(path)
    val content = f.getLines.mkString("\n")
    f.close()
    return Scheme.compile(SExpParser.parse(content))
  }

  "fib.scm" should "eval to 3" in {
    val result = aamScheme.eval(loadScheme("test/fib.scm"), None)
    assert(result.exists((st: aamScheme.State) => st.control match { case aamScheme.ControlKont(v) => abs.subsumes(v, absi.inject(3)) }))
  }
}

class AACConcreteTest extends AACFlatSpec[AbstractConcrete, ConcreteAddress]
class AACTypeTest extends AACFlatSpec[AbstractType, ClassicalAddress]
class AACTypeSetTest extends AACFlatSpec[AbstractTypeSet, ClassicalAddress]

class AAMConcreteTest extends AAMFlatSpec[AbstractConcrete, ConcreteAddress]
class AAMTypeTest extends AAMFlatSpec[AbstractType, ClassicalAddress]
class AAMTypeSetTest extends AAMFlatSpec[AbstractTypeSet, ClassicalAddress]
