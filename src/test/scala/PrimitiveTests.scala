import org.scalatest._
import org.scalatest.prop._
import org.scalatest.prop.TableDrivenPropertyChecks._

/** Tests that encodes Chapter 6 of R5RS (only for specified behaviour,
  * unspecified behaviour is not tested because it's... unspecified). This is
  * only for test cases explicitely given in R5RS. Some primitives are therefore
  * not tested (because they aren't given any test case in R5RS). Unsupported
  * primitives with test cases defined in R5RS are explicitely stated in
  * comments. If you're bored, you can implement some of them. */
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
    ("(eq? (cons 'a '()) (cons 'a '()))", f), // was: ("(eq? (list 'a) (list 'a))", f), but list not implemented
    ("(eq? '() '())", t),
    ("(eq? car car)", t),
    ("(let ((x '(a))) (eq? x x))", t),
    // ("(let ((x (make-vector 0))) (eq? x x))", t), // vectors not implemented
    ("(let ((p (lambda (x) x))) (eq? p p))", t)
  )
  property("eq? satisfies R5RS") { check(eq) }

  val equal = Table(
    ("program", "answer"),
    ("(equal? 'a 'a)", t),
    ("(equal? '(a) '(a))", t),
    ("(equal? '(a (b) c) '(a (b) c))", t),
    ("(equal? \"abc\" \"abc\")", t),
    ("(equal? 2 2)", t)
    // ("(equal? (make-vector 5 'a) (make-vector 5 'a))", t), // vectors not implemented
  )
  property("equal? satisfies R5RS") { check(equal) }

  /* 6.2 Numbers */
  // complex? is not implemented
  // real? is not implemented
  // rational? is not implemented
  // max is not implemented

  val plus = Table(
    ("program", "answer"),
    ("(+ 3 4)", absi.inject(7)),
    ("(+ 3)", absi.inject(3)),
    ("(+)", absi.inject(0)),
    ("(* 4)", absi.inject(4)),
    ("(*)", absi.inject(1))
  )
  property("+ satisfies R5RS") { check(plus) }

  val minus = Table(
    ("program", "answer"),
    ("(- 3 4)", absi.inject(-1)),
    ("(- 3 4 5)", absi.inject(-6)),
    ("(- 3)", absi.inject(-3))
  )
  property("- satisfies R5RS") { check(minus) }

  // division (/) is implemented BUT we don't support non-integers yet
  // abs is not implemented

  val modulo = Table(
    ("program", "answer"),
    ("(modulo 13 4)", absi.inject(1)),
    //("(modulo -13 4)", absi.inject(3)), // modulo is Scala's modulo, which is different from Scheme's
    //("(modulo 13 -4)", absi.inject(-3)), // modulo is Scala's modulo, which is different from Scheme's
    ("(modulo -13 -4)", absi.inject(-1))
  )
  property("modulo satisfies R5RS") { check(modulo) }

  // remainder not implemented

  val gcd = Table(
    ("program", "answer"),
    ("(gcd 32 -36)", absi.inject(4))
    // ("(gcd)", absi.inject(0)), // gcd doesn't support 0 arguments yet
  )
  property("gcd satisfies R5RS") { check(gcd) }

  // lcm not implemented yet
  // numerator not implemented yet
  // denominator not implemented yet
  // floor not implemented yet
  // ceiling not implemented yet
  // truncate not implemented yet
  // round not implemented yet
  // rationalize not implemented yet

  // string->number not implemented yet

  /* 6.3 Other data types */
  val not_ = Table(
    ("program", "answer"),
    ("(not #t)", f),
    ("(not 3)", f),
    ("(not (cons 3 '()))", f),
    ("(not #f)", t),
    ("(not '())", f),
    // ("not (list)", f) // list not implemented
    ("(not 'nil)", f)
  )
  property("not satisfies R5RS") { check(not_) }

  // boolean? not implemented

}

abstract class AAMTests[Abs, Addr](implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs],
  addr: Address[Addr], addri: AddressInjection[Addr])
    extends Tests[Abs, Addr] {
  val machine = new AAM[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])

  def checkResult(program: SchemeExp, answer: Abs) = {
    val result = machine.eval(program, None)
    println(s"$program -> $result ($answer)")
    assert(result.exists((st: machine.State) => st.control match {
      case machine.ControlKont(v) => abs.subsumes(v, answer)
      case _ => false
    }))
  }
}

/* Since these tests are small, they can be performed in concrete mode */
class AAMConcreteTests extends AAMTests[AbstractConcrete, ConcreteAddress]
