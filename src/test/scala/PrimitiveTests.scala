import org.scalatest._
import org.scalatest.prop._
import org.scalatest.prop.TableDrivenPropertyChecks._

/** Tests that encodes Chapter 6 of R5RS (only for specified behaviour,
  * unspecified behaviour is not tested because it's... unspecified). This is
  * only for test cases explicitly given in R5RS. Some primitives are therefore
  * not tested (because they aren't given any test case in R5RS). Unsupported
  * primitives with test cases defined in R5RS are explicitly stated in
  * comments. If you're bored, you can implement some of them. */
abstract class Tests[Exp : Expression, Addr : Address, Time : Timestamp](val lattice: SchemeLattice)
    extends PropSpec with TableDrivenPropertyChecks with Matchers {
  type Abs = lattice.L
  implicit val abs = lattice.isSchemeLattice
  val sem: Semantics[Exp, Abs, Addr, Time]
  val machine: AbstractMachine[Exp, Abs, Addr, Time]

  def checkResult(program: String, answer: Abs) = {
    val result = machine.eval(sem.parse(program), sem, false, Timeout.none)
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
    ("(eq? (cons 'a '()) (cons 'a '()))", f), // equivalent to following benchmark
    ("(eq? (list 'a) (list 'a))", f),
    ("(eq? '() '())", t),
    ("(eq? car car)", t),
    ("(let ((x '(a))) (eq? x x))", t),
    ("(let ((x (make-vector 0 1))) (eq? x x))", t),
    ("(let ((p (lambda (x) x))) (eq? p p))", t)
  ))

  r5rs("equal?", Table(
    ("program", "answer"),
    ("(equal? 'a 'a)", t),
    ("(equal? '(a) '(a))", t),
    ("(equal? '(a (b) c) '(a (b) c))", t),
    ("(equal? \"abc\" \"abc\")", t),
    ("(equal? 2 2)", t),
    ("(equal? (make-vector 5 'a) (make-vector 5 'a))", t),
    ("(equal? 1 2)", f),
    ("(equal? #\\a #\\b)", f),
    ("(equal? '(a b c) '(a b))", f),
    ("(equal? (cons 'a (cons 'b (cons 'c '()))) '(a b c))", t),
    ("(equal? '(a b c) '(a c b))", f)
  ))

  /* 6.2 Numbers */
  // complex? is not implemented
  r5rs("real?", Table(
    ("program", "answer"),
    ("(real? 3)", t),
    ("(real? 1.5)", t)))
  // rational? is not implemented

  r5rs("integer?", Table(
    ("program", "answer"),
    // ("(integer? 3+0i)", t), // notation not supported
    // ("(integer? 3.0)", t), // conversion not supported
    // ("(integer? 8/4)", t), // notation not supported
    ("(integer? 0)", t),
    ("(integer? '())", f)
  ))

  r5rs("number?", Table(
    ("program", "answer"),
    ("(number? 0)", t),
    ("(number? -1)", t),
    ("(number? 0.5)", t),
    ("(number? '())", f)
  ))

  r5rs("odd?", Table(
    ("program", "answer"),
    ("(odd? 0)", f),
    ("(odd? 1)", t),
    ("(odd? 101)", t)
  ))

  r5rs("max", Table(
    ("program", "answer"),
    ("(max 3 4)", abs.inject(4)),
    ("(max 3.9 4)", abs.inject(4)), /* TODO: Does not exactly follow spec (should be 4.0) */
    ("(max 1)", abs.inject(1)),
    ("(max 1 2 3 4 5 4 3 2 1)", abs.inject(5))))

  r5rs("min", Table(
    ("program", "answer"),
    ("(min 3 4)", abs.inject(3)),
    ("(min 3 4.9)", abs.inject(3)), /* TODO: should be 3.0 */
    ("(min 1)", abs.inject(1)),
    ("(min 5 4 3 2 1 2 3 4 5)", abs.inject(1))))

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

  // division (/) is implemented BUT we don't support fractions yet
  r5rs("abs", Table(
    ("program", "answer"),
    ("(abs -7)", abs.inject(7)),
    ("(abs 7)", abs.inject(7)),
    ("(abs 0)", abs.inject(0))))

  r5rs("modulo", Table(
    ("program", "answer"),
    ("(modulo 13 4)", abs.inject(1)),
    ("(modulo -13 4)", abs.inject(3)),
    ("(modulo 13 -4)", abs.inject(-3)),
    ("(modulo -13 -4)", abs.inject(-1))
  ))


  r5rs("quotient", Table(
    ("program", "answer"),
    ("(quotient 3 5)", abs.inject(0)),
    ("(quotient 4 2)", abs.inject(2)),
    ("(quotient -6 2)", abs.inject(-3))
  ))

  r5rs("remainder", Table(
    ("program", "answer"),
    ("(remainder 13 4)", abs.inject(1)),
    ("(remainder -13 4)", abs.inject(-1)),
    ("(remainder 13 -4)", abs.inject(1)),
    ("(remainder -13 -4)", abs.inject(-1))
    // ("(remainder -13 -4.0)", -1.0)
  ))


  r5rs("gcd", Table(
    ("program", "answer")
    // ("(gcd 32 -36)", abs.inject(4)) // TODO: not implemented correctly?
    // ("(gcd)", abs.inject(0)), // gcd doesn't support 0 arguments yet
  ))

  // lcm not implemented yet
  // numerator not implemented yet
  // denominator not implemented yet
  // floor not implemented yet
  r5rs("ceiling", Table(
    ("program", "answer"),
    ("(ceiling -4.3)", abs.inject(-4.toFloat)),
    ("(ceiling 3.5)", abs.inject(4.toFloat))))
  // truncate not implemented yet
  r5rs("round", Table(
    ("program", "anwser"),
    ("(round -4.3)", abs.inject(-4.toFloat)),
    ("(round 3.5)", abs.inject(4.toFloat)),
    //("(round 7/2)", abs.inject(4)),
    ("(round 7)", abs.inject(7))
  ))

  r5rs("sin", Table(
    ("program", "answer"),
    ("(sin 0)", abs.inject(0.toFloat))
  ))

  r5rs("asin", Table(
    ("program", "answer"),
    ("(asin 0)", abs.inject(0.toFloat))
  ))

  r5rs("cos", Table(
    ("program", "answer"),
    ("(cos 0)", abs.inject(1.toFloat))
  ))

  r5rs("acos", Table(
    ("program", "answer"),
    ("(acos 1)", abs.inject(0.toFloat))
  ))

  r5rs("tan", Table(
    ("program", "answer"),
    ("(tan 0)", abs.inject(0.toFloat)),
    ("(= (tan 4) (/ (sin 4) (cos 4)))", abs.inject(true)) // Test whether this mathematical relationship holds.
  ))

  r5rs("atan", Table(
    ("program", "answer"),
    ("(atan 0)", abs.inject(0.toFloat))
  ))

  r5rs("sqrt", Table(
    ("program", "answer"),
    ("(sqrt 16)", abs.inject(4.toFloat))
  ))
  // rationalize not implemented yet

  r5rs("log", Table(
    ("program", "answer"),
    ("(log 1)", abs.inject(0.toFloat))
  )) // TODO: (log 0) should raise an error

  r5rs("negative?", Table(
    ("program", "answer"),
    ("(negative? 0)", f),
    ("(negative? -1)", t),
    ("(negative? 1)", f)
  ))

  r5rs("positive?", Table(
    ("program", "answer"),
    ("(positive? 0)", f),
    ("(positive? -1)", f),
    ("(positive? 1)", t)
  ))

  r5rs("zero?", Table(
    ("program", "answer"),
    ("(zero? 0)", t),
    ("(zero? 1)", f),
    ("(zero? -1)", f)
  ))

  // string->number not implemented yet

  r5rs("number->string", Table(
    ("program", "answer"),
    ("(number->string 0)", abs.inject("0")),
    ("(number->string .5)", abs.inject("0.5")),
    ("(number->string -123.456)", abs.inject("-123.456"))
  ))

  /* 6.3 Other data types */
  r5rs("not", Table(
    ("program", "answer"),
    ("(not #t)", f),
    ("(not 3)", f),
    ("(not (cons 3 '()))", f),
    ("(not #f)", t),
    ("(not '())", f),
    ("(not (list))", f),
    ("(not 'nil)", f)
  ))

  r5rs("boolean?", Table(
    ("program", "answer"),
    ("(boolean? #f)", t),
    ("(boolean? 0)", f),
    ("(boolean? '())", f)
  ))

  r5rs("pair?", Table(
    ("program", "answer"),
    ("(pair? (cons 'a 'b))", t),
    ("(pair? '(a b c))", t),
    ("(pair? '())", f)
    // ("(pair? '#(a b))", t) // # notation not supported
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

  r5rs("null?", Table(
    ("program", "answer"),
    ("(null? '())", t),
    ("(null? (list))", t),
    ("(null? '(1 2 3))", f)
  ))

  r5rs("list?", Table(
    ("program", "answer"),
    ("(list? '(a b c))", t),
    ("(list? '((a b) c d))", t),
    ("(list? '())", t),
    ("(list? (cons 'a 'b))", f),
    ("(list? 'a)", f),
    ("(let ((x '(a))) (set-cdr! x x) (list? x))", f)
  ))

  r5rs("list", Table(
    ("program", "answer"),
    ("(equal? (list 'a (+ 3 4) 'c) '(a 7 c))", t),
    ("(list)", abs.nil)
  ))

  r5rs("length", Table(
    ("program", "answer"),
    ("(length '(a b c))", abs.inject(3)),
    ("(length '(a (b) (c d e)))", abs.inject(3)),
    ("(length '())", abs.inject(0))
  ))

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

  r5rs("symbol->string", Table(
    ("program", "answer"),
    ("(symbol->string 'flying-fish)", abs.inject("flying-fish"))
  ))

  // string->symbol not implemented

  r5rs("char?", Table(
    ("program", "answer"),
    ("(char? #\\a)", t),
    ("(char? 0)", f),
    ("(char? '())", f)
  ))
  // char->integer not implemented
  // integer->char not implemented
  // char<=? not implemented

  r5rs("string-append", Table(
    ("program", "answer"),
    ("(string-append \"foo\" \"bar\")", abs.inject("foobar"))
  ))

  r5rs("string-length", Table(
    ("program", "answer"),
    ("(string-length \"foobar\")", abs.inject(6))
  ))

  // 6.3.6: vector notation (#(1 2)) not supported
  r5rs("vector", Table(
    ("program", "answer"),
    ("(let ((vec (vector 'a 'b 'c))) (and (equal? (vector-ref vec 0) 'a) (equal? (vector-ref vec 1) 'b) (equal? (vector-ref vec 2) 'c)))", t),
    ("(let ((vec (vector 0 '(2 2 2 2) \"Anna\"))) (vector-set! vec 1 '(\"Sue\" \"Sue\")) (and (equal? (vector-ref vec 0) 0) (equal? (vector-ref vec 1) '(\"Sue\" \"Sue\")) (equal? (vector-ref vec 2) \"Anna\")))", t)
  ))

  r5rs("vector?", Table(
    ("program", "answer"),
    ("(vector? (vector 'a 'b 'c))", t),
    ("(vector? 'a)", f)
  ))

  r5rs("vector-length", Table(
    ("program", "answer"),
    ("(vector-length (vector))", abs.inject(0)),
    ("(vector-length (vector 0 1 0))", abs.inject(3))
  ))

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

abstract class AAMPrimitiveTests[Addr : Address, Time : Timestamp](override val lattice: SchemeLattice)
    extends Tests[SchemeExp, Addr, Time](lattice) {
  val sem = new SchemeSemantics[lattice.L, Addr, Time](new SchemePrimitives[Addr, lattice.L])
  val machine = new AAM[SchemeExp, lattice.L, Addr, Time]
}

abstract class AAMGlobalStorePrimitiveTests[Addr : Address, Time : Timestamp](override val lattice: SchemeLattice)
    extends Tests[SchemeExp, Addr, Time](lattice) {
  val sem = new SchemeSemantics[lattice.L, Addr, Time](new SchemePrimitives[Addr, lattice.L])
  val machine = new AAMAACP4F[SchemeExp, lattice.L, Addr, Time](AAMKAlloc)
}

abstract class AACPrimitiveTests[Addr : Address, Time : Timestamp](override val lattice: SchemeLattice)
    extends Tests[SchemeExp, Addr, Time](lattice) {
  val sem = new SchemeSemantics[lattice.L, Addr, Time](new SchemePrimitives[Addr, lattice.L])
  val machine = new AAMAACP4F[SchemeExp, lattice.L, Addr, Time](AACKAlloc)
}

abstract class FreePrimitiveTests[Addr : Address, Time : Timestamp](override val lattice: SchemeLattice)
    extends Tests[SchemeExp, Addr, Time](lattice) {
  val sem = new SchemeSemantics[lattice.L, Addr, Time](new SchemePrimitives[Addr, lattice.L])
  val machine = new AAMAACP4F[SchemeExp, lattice.L, Addr, Time](P4FKAlloc)
}

abstract class ConcreteMachinePrimitiveTests(override val lattice: SchemeLattice)
    extends Tests[SchemeExp, ClassicalAddress.A, ConcreteTimestamp.T](lattice) {
  val sem = new SchemeSemantics[lattice.L, ClassicalAddress.A, ConcreteTimestamp.T](new SchemePrimitives[ClassicalAddress.A, lattice.L])
  val machine = new ConcreteMachine[SchemeExp, lattice.L, ClassicalAddress.A, ConcreteTimestamp.T]
}

/* Since these tests are small, they can be performed in concrete mode */
class AAMConcretePrimitiveTests extends AAMPrimitiveTests[ClassicalAddress.A, ConcreteTimestamp.T](Lattices.ConcreteLattice)
class AAMTypeSetPrimitiveTests extends AAMPrimitiveTests[ClassicalAddress.A, ZeroCFA.T](Lattices.TypeLattice)
class AAMBoundedIntPrimitiveTests extends AAMPrimitiveTests[ClassicalAddress.A, ZeroCFA.T](Lattices.BoundedIntLattice)
class AAMGlobalStoreConcretePrimitiveTests extends AAMGlobalStorePrimitiveTests[ClassicalAddress.A, ZeroCFA.T](Lattices.ConcreteLattice)
class AAMGlobalStoreTypeSetPrimitiveTests extends AAMGlobalStorePrimitiveTests[ClassicalAddress.A, ZeroCFA.T](Lattices.TypeLattice)
class AAMGlobalStoreBoundedIntPrimitiveTests extends AAMGlobalStorePrimitiveTests[ClassicalAddress.A, ZeroCFA.T](Lattices.BoundedIntLattice)
class AACConcretePrimitiveTests extends AACPrimitiveTests[ClassicalAddress.A, ConcreteTimestamp.T](Lattices.ConcreteLattice)
class AACTypeSetPrimitiveTests extends AACPrimitiveTests[ClassicalAddress.A, ZeroCFA.T](Lattices.TypeLattice)
class AACBoundedIntPrimitiveTests extends AACPrimitiveTests[ClassicalAddress.A, ZeroCFA.T](Lattices.BoundedIntLattice)
class FreeConcretePrimitiveTests extends FreePrimitiveTests[ClassicalAddress.A, ConcreteTimestamp.T](Lattices.ConcreteLattice)
class FreeTypeSetPrimitiveTests extends FreePrimitiveTests[ClassicalAddress.A, ZeroCFA.T](Lattices.TypeLattice)
class FreeBoundedIntPrimitiveTests extends FreePrimitiveTests[ClassicalAddress.A, ZeroCFA.T](Lattices.BoundedIntLattice)
class ConcreteMachineConcretePrimitiveTests extends ConcreteMachinePrimitiveTests(Lattices.ConcreteLattice)
