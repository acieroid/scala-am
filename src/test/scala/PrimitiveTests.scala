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
    ("(eq? (cons 'a '()) (cons 'a '()))", f), // was: ("(eq? (list 'a) (list 'a))", f), but list not implemented
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
    ("(equal? (make-vector 5 'a) (make-vector 5 'a))", t)
  ))

  /* 6.2 Numbers */
  // complex? is not implemented
  r5rs("real?", Table(
    ("program", "answer"),
    ("(real? 3)", t),
    ("(real? 1.5)", t)))
  // rational? is not implemented

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

  r5rs("list?", Table(
    ("program", "answer"),
    ("(list? '(a b c))", t),
    ("(list? '((a b) c d))", t),
    ("(list? '())", t),
    ("(list? (cons 'a 'b))", f),
    ("(list? 'a)", f),
    ("(let ((x '(a))) (set-cdr! x x) (list? x))", f)
  ))

  // list not implemented
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

  // symbol->string not implemented
  // string->symbol not implemented

  // char->integer not implemented
  // integer->char not implemented
  // char<=? not implemented

  // 6.3.6: vector notation (#(1 2)) not supported
  r5rs("vector", Table(
    ("program", "answer"),
    ("(let ((vec (vector 'a 'b 'c))) (and (equal? (vector-ref vec 0) 'a) (equal? (vector-ref vec 1) 'b) (equal? (vector-ref vec 2) 'c)))", t),
    ("(let ((vec (vector 0 '(2 2 2 2) \"Anna\"))) (vector-set! vec 1 '(\"Sue\" \"Sue\")) (and (equal? (vector-ref vec 0) 0) (equal? (vector-ref vec 1) '(\"Sue\" \"Sue\")) (equal? (vector-ref vec 2) \"Anna\")))", t)
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
