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
  def r5rs(name: String, table: TableFor2[String, Abs]) =
    property(s"$name satisfies R5RS") { check(table) }

  val t = absi.inject(true)
  val f = absi.inject(false)

  /* 6.1 Equivalence predicates */
  // eqv? is not implemented

  r5rs("eq?", Table(
    ("program", "answer"),
    ("(eq? 'a 'a)", t),
    ("(eq? (cons 'a '()) (cons 'a '()))", f), // was: ("(eq? (list 'a) (list 'a))", f), but list not implemented
    ("(eq? '() '())", t),
    ("(eq? car car)", t),
    ("(let ((x '(a))) (eq? x x))", t),
    // ("(let ((x (make-vector 0))) (eq? x x))", t), // vectors not implemented
    ("(let ((p (lambda (x) x))) (eq? p p))", t)
  ))

  r5rs("equal?", Table(
    ("program", "answer"),
    ("(equal? 'a 'a)", t),
    ("(equal? '(a) '(a))", t),
    ("(equal? '(a (b) c) '(a (b) c))", t),
    ("(equal? \"abc\" \"abc\")", t),
    ("(equal? 2 2)", t)
    // ("(equal? (make-vector 5 'a) (make-vector 5 'a))", t), // vectors not implemented
  ))

  /* 6.2 Numbers */
  // complex? is not implemented
  // real? is not implemented
  // rational? is not implemented
  // max is not implemented

  r5rs("+", Table(
    ("program", "answer"),
    ("(+ 3 4)", absi.inject(7)),
    ("(+ 3)", absi.inject(3)),
    ("(+)", absi.inject(0)),
    ("(* 4)", absi.inject(4)),
    ("(*)", absi.inject(1))
  ))

  r5rs("-", Table(
    ("program", "answer"),
    ("(- 3 4)", absi.inject(-1)),
    ("(- 3 4 5)", absi.inject(-6)),
    ("(- 3)", absi.inject(-3))
  ))

  // division (/) is implemented BUT we don't support non-integers yet
  // abs is not implemented

  r5rs("modulo", Table(
    ("program", "answer"),
    ("(modulo 13 4)", absi.inject(1)),
    //("(modulo -13 4)", absi.inject(3)), // modulo is Scala's modulo, which is different from Scheme's
    //("(modulo 13 -4)", absi.inject(-3)), // modulo is Scala's modulo, which is different from Scheme's
    ("(modulo -13 -4)", absi.inject(-1))
  ))

  // remainder not implemented

  r5rs("gcd", Table(
    ("program", "answer")
    // ("(gcd 32 -36)", absi.inject(4)) // TODO: not implemented correctly?
    // ("(gcd)", absi.inject(0)), // gcd doesn't support 0 arguments yet
  ))

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
  r5rs("not", Table(
    ("program", "answer"),
    ("(not #t)", f),
    ("(not 3)", f), // not currently only supports bool
    ("(not (cons 3 '()))", f), // not currently only supports bool
    ("(not #f)", t),
    ("(not '())", f), // not currently only supports bool
    // ("not (list)", f), // list not implemented
    ("(not 'nil)", f) // not currently only supports bool
  ))

  // boolean? not implemented

  r5rs("pair?", Table(
    ("program", "answer"),
    ("(pair? (cons 'a 'b))", t),
    ("(pair? '(a b c))", t),
    ("(pair? '())", f)
    // ("(pair? '#(a b))", t) // vectors not supported
  ))

  r5rs("cons", Table(
    ("program", "answer"),
    ("(equal? (cons 'a '()) '(a))", t),
    ("(equal? (cons '(a) '(b c d)) '((a) b c d))", t),
    ("(equal? (cons \"a\" '(b c)) '(\"a\" b c))", t)
    // ("(equal? (cons 'a 3) '(a . 3))", t), // . notation not supported
    // ("(equal? (cons '(a b) 'c) '((a b) . c))", t) // . notation not supported
  ))

  r5rs("car", Table(
    ("program", "answer"),
    ("(equal? (car '(a b c)) 'a)", t),
    ("(equal? (car '((a) b c d)) '(a))", t),
    ("(equal? (car (cons 1 2)) 1)", t)
    // TODO: (car '()) should raise an error
  ))

  r5rs("cdr", Table(
    ("program", "answer"),
    ("(equal? (cdr '((a) b c d)) '(b c d))", t),
    ("(equal? (cdr (cons 1 2)) 2)", t)
    // TODO: (cdr '()) should raise an error
  ))

  // list? not implemented
  // list not implemented
  // length not implemented
  // append not implemented
  // reverse not implemented
  // list-ref not implemented
  // memq not implemented
  // member not implemented
  // memv not implemented
  // assq not implemented
  // assoc not implemented
  // assv not implemented
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
class AAMTypeSetTests extends AAMTests[AbstractConcrete, ConcreteAddress]
