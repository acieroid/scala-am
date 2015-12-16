import org.scalatest._
import org.scalatest.prop._
import org.scalatest.prop.TableDrivenPropertyChecks._
import Timestamps._

/** Tests that encodes Chapter 6 of R5RS (only for specified behaviour,
  * unspecified behaviour is not tested because it's... unspecified). This is
  * only for test cases explicitely given in R5RS. Some primitives are therefore
  * not tested (because they aren't given any test case in R5RS). Unsupported
  * primitives with test cases defined in R5RS are explicitely stated in
  * comments. If you're bored, you can implement some of them. */
abstract class Tests[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val abs = implicitly[AbstractValue[Abs]]
  val sem: Semantics[Exp, Abs, Addr, Time]
  val machine: AbstractMachine[Exp, Abs, Addr, Time]

  def checkResult(program: String, answer: Abs) = {
    val result = machine.eval(sem.parse(program), sem, false)
    assert(result.containsFinalValue(answer))
  }
  def check(table: TableFor2[String, Abs]) =
    forAll (table) { (program: String, answer: Abs) =>
      checkResult(program, answer)
    }
  def r5rs(name: String, table: TableFor2[String, Abs]) =
    property(s"$name satisfies R5RS") { check(table) }

  val t = abs.inject(true)
  val f = abs.inject(false)

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
    ("(+ 3 4)", abs.inject(7)),
    ("(+ 3)", abs.inject(3)),
    ("(+)", abs.inject(0)),
    ("(* 4)", abs.inject(4)),
    ("(*)", abs.inject(1))
  ))

  r5rs("-", Table(
    ("program", "answer"),
    ("(- 3 4)", abs.inject(-1)),
    ("(- 3 4 5)", abs.inject(-6)),
    ("(- 3)", abs.inject(-3))
  ))

  // division (/) is implemented BUT we don't support non-integers yet
  // abs is not implemented

  r5rs("modulo", Table(
    ("program", "answer"),
    ("(modulo 13 4)", abs.inject(1)),
    //("(modulo -13 4)", abs.inject(3)), // modulo is Scala's modulo, which is different from Scheme's
    //("(modulo 13 -4)", abs.inject(-3)), // modulo is Scala's modulo, which is different from Scheme's
    ("(modulo -13 -4)", abs.inject(-1))
  ))

  // remainder not implemented

  r5rs("gcd", Table(
    ("program", "answer")
    // ("(gcd 32 -36)", abs.inject(4)) // TODO: not implemented correctly?
    // ("(gcd)", abs.inject(0)), // gcd doesn't support 0 arguments yet
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

  r5rs("symbol?", Table(
    ("program", "answer"),
    ("(symbol? 'foo)", t),
    ("(symbol? (car '(a b)))", t),
    ("(symbol? \"bar\")", f),
    ("(symbol? 'nil)", t),
    ("(symbol? '())", f),
    ("(symbol? #f)", f)
  ))

  // symbol->string not implemented
  // string->symbol not implemented

  // char->integer not implemented
  // integer->char not implemented
  // char<=? not implemented

  // 6.3.6: vectors are not supported

  /* 6.4 Control features */
  // procedure not implemented
  // apply not implemented
  // map not implemented
  // for-each not implemented
  // force not implemented
  // call/cc not implemented
  // call-with-values not implemented

  /* 6.5 Eval */
  // eval not implemented

  /* 6.6 Input and output */
}

abstract class AAMPrimitiveTests[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Tests[SchemeExp, Abs, Addr, Time] {
  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
}

abstract class AACPrimitiveTests[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Tests[SchemeExp, Abs, Addr, Time] {
  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAC[SchemeExp, Abs, Addr, Time]
}

abstract class FreePrimitiveTests[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Tests[SchemeExp, Abs, Addr, Time] {
  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new Free[SchemeExp, Abs, Addr, Time]
}

abstract class ConcurrentAAMPrimitiveTests[Abs : AbstractValue, Addr : Address, Time : Timestamp, TID : ThreadIdentifier]
    extends Tests[SchemeExp, Abs, Addr, Time] {
  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new ConcurrentAAM[SchemeExp, Abs, Addr, Time, TID]
}

/* Since these tests are small, they can be performed in concrete mode */
class AAMConcretePrimitiveTests extends AAMPrimitiveTests[ConcreteLattice.L, ConcreteAddress, ZeroCFA]
class AAMTypeSetPrimitiveTests extends AAMPrimitiveTests[TypeSetLattice.L, ClassicalAddress, ZeroCFA]
class AACConcretePrimitiveTests extends AACPrimitiveTests[ConcreteLattice.L, ConcreteAddress, ZeroCFA]
class AACTypeSetPrimitiveTests extends AACPrimitiveTests[TypeSetLattice.L, ClassicalAddress, ZeroCFA]
class FreeConcretePrimitiveTests extends FreePrimitiveTests[ConcreteLattice.L, ClassicalAddress, ZeroCFA]
class FreeTypeSetPrimitiveTests extends FreePrimitiveTests[TypeSetLattice.L, ClassicalAddress, ZeroCFA]
class ConcurrentAAMConcretePrimitiveTests extends ConcurrentAAMPrimitiveTests[ConcreteLattice.L, ClassicalAddress, ZeroCFA, ConcreteTID]
class ConcurrentAAMTypeSetPrimitiveTests extends ConcurrentAAMPrimitiveTests[TypeSetLattice.L, ClassicalAddress, ZeroCFA, ContextSensitiveTID]
