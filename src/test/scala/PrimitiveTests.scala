import org.scalatest._
import org.scalatest.prop._
import org.scalatest.prop.TableDrivenPropertyChecks._

/** Tests that encodes Chapter 6 of R5RS */
abstract class Tests[Abs, Addr](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
  addr: Address[Addr], addri: AddressInjection[Addr])
    extends PropSpec with TableDrivenPropertyChecks with Matchers {
  def checkResult(program: SchemeExp, answer: Abs): Unit
  def check(table: TableFor2[String, Abs]) =
    forAll (table) { (program: String, answer: Abs) =>
      checkResult(Scheme.parseString(program), answer)
    }

  val t = absi.inject(true)
  val f = absi.inject(false)

  /* 6.1 Equivalence predicates */
  // eqv? is not implemented

  val eq = Table(
    ("program", "answer"),
    ("(eq? 'a 'a)", t),
    ("(eq? (list 'a) (list 'a))", f),
    ("(eq? '() '())", t),
    ("(eq? car car)", t),
    ("(let ((x '(a))) (eq? x x))", t),
    ("(let ((p (lambda (x) x))) (eq? p p)", t)
  )
  property("eq? satisfies R5RS") { check(eq) }
}

abstract class AAMTests[Abs, Addr](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
  addr: Address[Addr], addri: AddressInjection[Addr])
    extends Tests[Abs, Addr] {
  val machine = new AAM[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])

  def checkResult(program: SchemeExp, answer: Abs) = {
    val result = machine.eval(program, None)
    assert(result.exists((st: machine.State) => st.control match {
      case machine.ControlKont(v) => abs.subsumes(v, answer)
      case _ => false
    }))
  }
}

/* Since these tests are small, they can be performed in concrete mode */
class AAMConcreteTests extends AAMTests[AbstractConcrete, ConcreteAddress]
