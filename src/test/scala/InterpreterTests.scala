import org.scalatest._
import org.scalatest.prop._
import Address._

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

  "fib.scm" should "eval to 3" in {
    val result = aacScheme.eval(loadScheme("test/fib.scm"), None)
    println(result)
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
    println(result)
  }
}

class AACConcreteTest extends AACFlatSpec[AbstractConcrete, ClassicalAddress]
//class AACTypeTest extends AACFlatSpec[AbstractType, ClassicalAddress]
//class AACTypeSetTest extends AACFlatSpec[AbstractTypeSet, ClassicalAddress]


//class AAMConcreteTest extends AAMFlatSpec[AbstractConcrete, ClassicalAddress]
//class AAMTypeTest extends AAMFlatSpec[AbstractType, ClassicalAddress]
//class AAMTypeSetTest extends AAMFlatSpec[AbstractTypeSet, ClassicalAddress]
