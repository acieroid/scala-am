import org.scalatest._
import org.scalatest.prop._
// import org.scalatest.prop.TableDrivenPropertyChecks._

import scalaam.core._
import scalaam.language.scheme._
import scalaam.graph._
import Graph._
import scalaam.machine._
import scalaam.lattice._

/** Tests that encodes Chapter 6 of R5RS (only for specified behaviour,
  * unspecified behaviour is not tested because it's... unspecified). This is
  * only for test cases explicitly given in R5RS. Some primitives are therefore
  * not tested (because they aren't given any test case in R5RS). Unsupported
  * primitives with test cases defined in R5RS are explicitly stated in
  * comments. If you're bored, you can implement some of them. */
abstract class SchemeTests[A <: Address, V, T, C](
  implicit val timestamp: Timestamp[T, C],
  implicit val lat: SchemeLattice[V, SchemeExp, A])
    extends PropSpec with TableDrivenPropertyChecks with Matchers {
  import lat._
  val sem: Semantics[SchemeExp, A, V, T, C]
  val machine: MachineAbstraction[SchemeExp, A, V, T, C]
  val graph = ReachableStatesConditionGraph[machine.State, machine.Transition](n => n.metadata.find("halted") == Some(GraphMetadataBool(true)))

  def checkResult(program: String, answer: V) = {
    val result = machine.run[graph.G](SchemeParser.parse(program), Timeout.Infinity)
    assert(!result.findNodes(n => n.metadata.find("type") == Some(GraphMetadataString("kont")) && {
      //println(n)
       //println(n.metadata.find("value"))
       //println(Some(GraphMetadataValue[V](answer)))
       //println(n.metadata.find("value") == Some(GraphMetadataValue[V](answer)))
      n.metadata.find("value") == Some(GraphMetadataValue[V](answer))
    }
    ).isEmpty)
  }
  def check(table: TableFor2[String, V]) =
    forAll (table) { (program: String, answer: V) =>
      checkResult(program, answer)
    }
  def r5rs(name: String, table: TableFor2[String, V]) =
    property(s"$name satisfies R5RS") { check(table) }

  val t = bool(true)
  val f = bool(false)

  /* 6.1 Equivalence predicates */
  // eqv? is not implemented
  r5rs("eq?", Table(
    ("program", "answer"),
    ("(eq? 'a 'a)", t),
//    ("(eq? (cons 'a '()) (cons 'a '()))", f), // equivalent to following benchmark
// TODO    ("(eq? (list 'a) (list 'a))", f),
    ("(eq? '() '())", t),
//    ("(eq? car car)", t),
//    ("(let ((x '(a))) (eq? x x))", t),
//    ("(let ((x (make-vector 0 1))) (eq? x x))", t),
//    ("(let ((p (lambda (x) x))) (eq? p p))", t)
  ))
  // TODO reimplement
  r5rs("equal?", Table(
    ("program", "answer"),
    ("(equal? 'a 'a)", t),
    ("(equal? '(a) '(a))", t),
    ("(equal? '(a (b) c) '(a (b) c))", t),
    ("(equal? \"abc\" \"abc\")", t),
    ("(equal? 2 2)", t),
///    ("(equal? (make-vector 5 'a) (make-vector 5 'a))", t),
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
    ("(max 3 4)", number(4)),
    ("(max 3.9 4)", number(4)), /* TODO: Does not exactly follow spec (should be 4.0) */
    ("(max 1)", number(1)),
    ("(max 1 2 3 4 5 4 3 2 1)", number(5))))

  r5rs("min", Table(
    ("program", "answer"),
    ("(min 3 4)", number(3)),
    ("(min 3 4.9)", number(3)), /* TODO: should be 3.0 */
    ("(min 1)", number(1)),
    ("(min 5 4 3 2 1 2 3 4 5)", number(1))))

  r5rs("+", Table(
    ("program", "answer"),
    ("(+ 3 4)", number(7)),
    ("(+ 3)", number(3)),
    ("(+)", number(0)),
  ))

  r5rs("*", Table(
    ("program", "answer"),
    ("(* 3 4)", number(12)),
    ("(* 4)", number(4)),
    ("(*)", number(1))
  ))

  r5rs("-", Table(
    ("program", "answer"),
    ("(- 3 4)", number(-1)),
    ("(- 3 4 5)", number(-6)),
    ("(- 3)", number(-3))
  ))

  // division (/) is implemented BUT we don't support fractions yet
  r5rs("/", Table(
    ("program", "answer"),
    ("(/ 4 2)", number(2)),
    ("(/ 1 2)", real(0.5)), /* should be 1/2 */
    ("(/ 1.0 1)", real(1.0)),
    ("(/ 1 1.0)", real(1.0)),
    ("(/ 4 2.0)", real(2.0))
  ))

  r5rs("<", Table(
    ("program", "answer"),
//    ("(<)", t),
//    ("(< 1)", t),
    ("(< 1 2)", t),
    ("(< 2 1)", f),
    ("(< 1 1)", f),
    ("(< 2.0 2.1)", t),
    ("(< 2.1 2.0)", f)
  ))

  r5rs("<=", Table(
    ("program", "answer"),
//    ("(<=)", t),
//    ("(<= 1)", t),
    ("(<= 1 2)", t),
    ("(<= 2 1)", f),
    ("(<= 1 1)", t),
    ("(<= 2.0 2.1)", t),
    ("(<= 2.1 2.0)", f)
  ))

  r5rs(">", Table(
    ("program", "answer"),
//    ("(>)", f),
//    ("(> 1)", f),
    ("(> 1 2)", f),
    ("(> 2 1)", t),
    ("(> 1 1)", f),
    ("(> 2.0 2.1)", f),
    ("(> 2.1 2.0)", t)
  ))

  r5rs(">=", Table(
    ("program", "answer"),
//    ("(>=)", f),
//    ("(>= 1)", f),
    ("(>= 1 2)", f),
    ("(>= 2 1)", t),
    ("(>= 1 1)", t),
    ("(>= 2.0 2.1)", f),
    ("(>= 2.1 2.0)", t)
  ))

  r5rs("=", Table(
    ("program", "answer"),
    ("(= 1 1)", t),
    ("(= 2 1)", f)
  ))


  r5rs("abs", Table(
    ("program", "answer"),
    ("(abs -7)", number(7)),
    ("(abs 7)", number(7)),
    ("(abs 0)", number(0))))

  r5rs("modulo", Table(
    ("program", "answer"),
    ("(modulo 13 4)", number(1)),
    ("(modulo -13 4)", number(3)),
    ("(modulo 13 -4)", number(-3)),
    ("(modulo -13 -4)", number(-1))
  ))


  r5rs("quotient", Table(
    ("program", "answer"),
    ("(quotient 3 5)", number(0)),
    ("(quotient 4 2)", number(2)),
    ("(quotient -6 2)", number(-3))
  ))

  r5rs("remainder", Table(
    ("program", "answer"),
    ("(remainder 13 4)", number(1)),
    ("(remainder -13 4)", number(-1)),
    ("(remainder 13 -4)", number(1)),
    ("(remainder -13 -4)", number(-1))
    // ("(remainder -13 -4.0)", -1.0)
  ))


  r5rs("gcd", Table(
    ("program", "answer")
    // ("(gcd 32 -36)", abs.inject(4)) // TODO: not implemented correctly?
    // ("(gcd)", abs.inject(0)), // gcd doesn't support 0 arguments yet
  ))

  // lcm not implemented yet
  r5rs("expt", Table(
    ("program", "answer"),
    ("(expt 5 2)", number(25)),
    ("(expt 1 0)", number(1)),
    ("(expt 0 0)", number(1))
  ))
  // numerator not implemented yet
  // denominator not implemented yet

  r5rs("ceiling", Table(
    ("program", "answer"),
    ("(ceiling -4.3)", real(-4.0)),
    ("(ceiling 3.5)", real(4.0))))
  // truncate not implemented yet
//TODO  r5rs("round", Table(
//    ("program", "anwser"),
//    ("(round -4.3)", real(-4.0)),
//    ("(round 3.5)", real(4.0)),
//    //("(round 7/2)", abs.inject(4)), // TODO: rounding an exact returns an exact (but we don't support fractions)
//    ("(round 7)", real(7))
//  ))

  r5rs("floor", Table(
    ("program", "answer"),
    ("(floor -4.3)", real(-5.0)),
    ("(floor 3.5)", real(3.0)),
    ("(floor 7)", number(7))
  ))

  r5rs("sin", Table(
    ("program", "answer"),
    ("(sin 0)", real(0.0))
  ))

  r5rs("asin", Table(
    ("program", "answer"),
    ("(asin 0)", real(0.0))
  ))

  r5rs("cos", Table(
    ("program", "answer"),
    ("(cos 0)", real(1.0))
  ))

  r5rs("acos", Table(
    ("program", "answer"),
    ("(acos 1)", real(0.0))
  ))

  r5rs("tan", Table(
    ("program", "answer"),
    ("(tan 0)", real(0)),
    ("(= (tan 4) (/ (sin 4) (cos 4)))", t) // Test whether this mathematical relationship holds.
  ))

  r5rs("atan", Table(
    ("program", "answer"),
    ("(atan 0)", real(0.0))
  ))

//TODO  r5rs("sqrt", Table(
//    ("program", "answer"),
//    ("(sqrt 4)", number(2)), // R5RS: It is desirable (but not required) for potentially inexact operations such as `sqrt', when applied to exact arguments, to produce exact answers whenever possible (for example the square root of an exact 4 ought to be an exact 2).
//    ("(sqrt 16)", number(4)),
//    ("(sqrt 4.0)", real(2.0))
//  ))
  // rationalize not implemented yet

  r5rs("log", Table(
    ("program", "answer"),
    ("(log 1)", real(0.0))
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

  r5rs("even?", Table(
    ("program", "answer"),
    ("(even? 0)", t),
    ("(even? 1)", f),
    ("(even? -1)", f),
    ("(even? -2)", t)))

  r5rs("exact->inexact", Table(
    ("program", "answer"),
    ("(exact->inexact 5)", real(5.0)),
    ("(exact->inexact 0)", real(0.0))
  ))

  r5rs("inexact->exact", Table(
    ("program", "answer"),
    ("(inexact->exact 5.0)", number(5)),
    ("(inexact->exact 0.000)", number(0))
  ))

  // string->number not implemented yet

  r5rs("number->string", Table(
    ("program", "answer"),
    ("(number->string 0)", string("0")),
    ("(number->string .5)", string("0.5")),
    ("(number->string -123.456)", string("-123.456"))
  ))

  /* 6.3 Other data types */
  r5rs("not", Table(
    ("program", "answer"),
    ("(not #t)", f),
    ("(not 3)", f),
// TODO    ("(not (cons 3 '()))", f),
    ("(not #f)", t),
// TODO   ("(not '())", f),
// TODO   ("(not (list))", f),
    ("(not 'nil)", f)
  ))

  r5rs("boolean?", Table(
    ("program", "answer"),
    ("(boolean? #f)", t),
    ("(boolean? 0)", f),
    ("(boolean? '())", f)
  ))

//  r5rs("pair?", Table(
//    ("program", "answer"),
//    ("(pair? (cons 'a 'b))", t),
//    ("(pair? '(a b c))", t),
//    ("(pair? '())", f)
//    // ("(pair? '#(a b))", t) // # notation not supported
//  ))

 r5rs("cons", Table(
    ("program", "answer"),
//    ("(equal? (cons 'a '()) '(a))", t),
//    ("(equal? (cons '(a) '(b c d)) '((a) b c d))", t),
//    ("(equal? (cons \"a\" '(b c)) '(\"a\" b c))", t)
//    // ("(equal? (cons 'a 3) '(a . 3))", t), // . notation not supported
//    // ("(equal? (cons '(a b) 'c) '((a b) . c))", t) // . notation not supported
))

  r5rs("car", Table(
    ("program", "answer"),
//TODO    ("(equal? (car '(a b c)) 'a)", t),
//    ("(equal? (car '((a) b c d)) '(a))", t),
//    ("(equal? (car (cons 1 2)) 1)", t)
      // TODO: (car '()) should raise an error
  ))

  r5rs("cdr", Table(
    ("program", "answer"),
//TODO    ("(equal? (cdr '((a) b c d)) '(b c d))", t),
//    ("(equal? (cdr (cons 1 2)) 2)", t)
      // TODO: (cdr '()) should raise an error
  ))

  r5rs("null?", Table(
    ("program", "answer"),
//TODO    ("(null? '())", t),
//    ("(null? (list))", t),
//    ("(null? '(1 2 3))", f)
  ))

  r5rs("list?", Table(
    ("program", "answer"),
//TODO    ("(list? '(a b c))", t),
//    ("(list? '((a b) c d))", t),
//    ("(list? '())", t),
//    ("(list? (cons 'a 'b))", f),
//    ("(list? 'a)", f),
//    ("(let ((x '(a))) (set-cdr! x x) (list? x))", f)
  ))

//  r5rs("list", Table(
//    ("program", "answer"),
//    ("(equal? (list 'a (+ 3 4) 'c) '(a 7 c))", t),
//    ("(list)", nil)
//  ))

  r5rs("length", Table(
    ("program", "answer"),
//TODO    ("(length '(a b c))", number(3)),
//    ("(length '(a (b) (c d e)))", number(3)),
//    ("(length '())", number(0))
  ))

  // append not implemented
  // reverse not implemented
//  r5rs("list-ref", Table(
//    ("program", "answer"),
//    ("(list-ref '(a b c d) 2)", symbol("c")),
//    ("(list-ref '(a b c d) (inexact->exact (round 1.8)))", symbol("c"))
//  ))
//  r5rs("memq", Table(
//    ("program", "answer"),
//    ("(equal? (memq 'a '(a b c)) '(a b c))", t),
//    ("(equal? (memq 'b '(a b c)) '(b c))", t),
//    ("(memq 'a '(b c d))", f),
//    ("(memq (list 'a) '(b (a) c))", f)
//  ))
//  r5rs("member", Table(
//    ("program", "answer"),
//    ("(equal? (member (list 'a) '(b (a) c)) '((a) c))", t),
//    ("(member 'd '(a b c))", f)
//  ))
  // memv not implemented
//  r5rs("assq", Table(
//    ("program", "answer"),
//    ("(equal? (assq 'a '((a 1) (b 2) (c 3))) '(a 1))", t),
//    ("(equal? (assq 'b '((a 1) (b 2) (c 3))) '(b 2))", t),
//    ("(equal? (assq 'c '((a 1) (b 2) (c 3))) '(c 3))", t),
//    ("(assq 'd '((a 1) (b 2) (c 3)))", f),
//    ("(assq (list 'a) '(((a)) ((b)) ((c))))", f)
//  ))
//  r5rs("assoc", Table(
//    ("program", "answer"),
//    ("(equal? (assoc (list 'a) '(((a)) ((b)) ((c)))) '((a)))", t)
//  ))
  // assv not implemented

  r5rs("symbol?", Table(
    ("program", "answer"),
//TODO    ("(symbol? 'foo)", t),
//    ("(symbol? (car '(a b)))", t),
//    ("(symbol? \"bar\")", f),
//    ("(symbol? 'nil)", t),
//    ("(symbol? '())", f),
//    ("(symbol? #f)", f)
  ))
  r5rs("symbol->string", Table(
    ("program", "answer"),
    ("(symbol->string 'flying-fish)", string("flying-fish"))
  ))

  r5rs("string->symbol", Table(
    ("program", "answer"),
    ("(string->symbol \"flying-fish\")", symbol("flying-fish"))
  ))
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
    ("(string-append \"foo\" \"bar\")", string("foobar"))
  ))

  r5rs("string-length", Table(
    ("program", "answer"),
    ("(string-length \"foobar\")", number(6))
  ))

  r5rs("string?", Table(
    ("program", "answer"),
    ("(string? 'foo)", f),
    ("(string? 1)", f),
    ("(string? \"\")", t),
    ("(string? \"foo\")", t)
  ))

  r5rs("string<?", Table(
    ("program", "answer"),
    ("(string<? \"foo\" \"bar\")", f),
    ("(string<? \"bar\" \"foo\")", t),
    ("(string<? \"foo\" \"foo\")", f),
    ("(string<? \"f\" \"foo\")", t)
  ))

  // 6.3.6: vector notation (#(1 2)) not supported
  // TODO: reimplement vectors
//  r5rs("vector", Table(
//    ("program", "answer"),
//    ("(let ((vec (vector 'a 'b 'c))) (and (equal? (vector-ref vec 0) 'a) (equal? (vector-ref vec 1) 'b) (equal? (vector-ref vec 2) 'c)))", t),
//    ("(let ((vec (vector 0 '(2 2 2 2) \"Anna\"))) (vector-set! vec 1 '(\"Sue\" \"Sue\")) (and (equal? (vector-ref vec 0) 0) (equal? (vector-ref vec 1) '(\"Sue\" \"Sue\")) (equal? (vector-ref vec 2) \"Anna\")))", t)
//  ))
//
//  r5rs("vector?", Table(
//    ("program", "answer"),
//    ("(vector? (vector 'a 'b 'c))", t),
//    ("(vector? 'a)", f)
//  ))
//
//  r5rs("vector-length", Table(
//    ("program", "answer"),
//    ("(vector-length (vector))", number(0)),
//    ("(vector-length (vector 0 1 0))", number(3))
//  ))

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

abstract class SchemePrimitiveAAMTests[A <: Address, T, V](
  allocator: Allocator[A, T, SchemeExp])(
  implicit val time: Timestamp[T, SchemeExp],
  implicit val l: SchemeLattice[V, SchemeExp, A]) extends SchemeTests[A, V, T, SchemeExp] {
  val sem = new BaseSchemeSemantics[A, V, T, SchemeExp](allocator)
  val machine = new AAM[SchemeExp, A, V, T](sem)
}

object ConcreteSchemeLattice extends MakeSchemeLattice[SchemeExp, NameAddress.A, Concrete.S, Concrete.B, Concrete.I, Concrete.R, Concrete.C, Concrete.Sym]
object ConcreteSchemeTimestamp extends ConcreteTimestamp[SchemeExp]

class ConcreteSchemePrimitiveAAMTests extends SchemePrimitiveAAMTests[NameAddress.A, ConcreteSchemeTimestamp.T, ConcreteSchemeLattice.L](NameAddress.Alloc[ConcreteSchemeTimestamp.T, SchemeExp])
