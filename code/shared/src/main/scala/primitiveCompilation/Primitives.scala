package scalaam.primitiveCompilation

import scalaam.core.{Expression, Identifier, Identity}
import scalaam.language.scheme.{SchemeBegin, SchemeExp, SchemeParser, SchemeVar}
import util.FileUtil

object Primitives {
  val primitives = Map(
    // "*" is a primop
    // "+" is a primop
    // "-" is a primop
    // "/" is a primop
    "abs" -> "(define (abs x) (if (< x 0) (- 0 x) x))",
    // "acos" is a primop
    "append" -> """(define (append l1 l2)
          (if (null? l1)
              l2
              (cons (car l1)
                    (append (cdr l1) l2))))""",
    // "asin" is a primop
    "assoc" -> """(define (assoc k l)
        (if (null? l)
          #f
         (if (equal? (caar l) k)
           (car l)
           (assoc k (cdr l)))))""",
    "assq" -> """(define (assq k l)
        (if (null? l)
          #f
         (if (equal? (caar l) k)
           (car l)
           (assq k (cdr l)))))""",
    "assv" -> """(define (assq k l)
        (if (null? l)
          #f
         (if (eqv? (caar l) k)
           (car l)
           (assq k (cdr l)))))""",
    // "atan" is a primop
    // "boolean?" is a primop
    // "car" is a primop
    // "cdr" is a primop
    // "ceiling" is a primop
    // "char->integer" is a primop
    // "char?" is a primop
    // "cons" is a primop? TODO maybe not exactly (could define in terms of vectors of size 2)
    // "cos" is a primop
    "display" -> "(define (display x) x)", // undefined behavior in R5RS
    // "eq?" is a primop
    // TODO: vectors in equal? (requires a loop, see SchemePrimitives.scala)
    "equal?" -> """(define (equal? a b)
          (or (eq? a b)
            (and (null? a) (null? b))
            (and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))""",
    "even?" -> "(define (even? x) (= 0 (modulo x 2)))",
    // "exact->inexact" is a primop
    // TODO: expt // TODO isn't this a primot (easier to handle real exponents).
    // TODO: exp
    // "floor" is a primop
    "gcd" -> "(define (gcd a b) (if (= b 0) a (gcd b (modulo a b))))",
    // "inexact->exact" is a primop
    // "integer?" is a primop
    "lcm" -> "(define (lcm m n) (/ (abs (* m n)) (gcd m n)))",
    /*
    (define (lcm m n)
      (define (generate cur prim)
        (let ((flag #f))
          (for-each (lambda (p) (set! flag (or flag (= 0 (modulo cur p))))) prim)
          (if (not flag) ; Found a prime.
              (cons cur (delay (generate (+ cur 1) (cons cur prim))))
              (generate (+ cur 1) prim))))
      (define primes (generate 2 '()))
      (define (next-prime primes)
        (force (cdr primes)))
      (define (factor n)
        (define (f n div p)
          (if (= n 1)
              div
              (if (= 0 (modulo n (car p)))
                  (f (/ n (car p)) (cons (car p) div) p)
                  (f n div (next-prime p)))))
        (f n '() primes))
      (let loop ((f1 (factor m))
                 (f2 (factor n))
                 (cur 1))
        (cond ((or (null? f1)(null? f2)) cur)
              ((= (car f1)(car f2))(loop (cdr f1)(cdr f2)(* cur (car f1))))
              ((> (car f1)(car f2))(loop (cdr f1) f2 (* cur (car f1))))
              (else (loop f1 (cdr f2) (* cur (car f2)))))))
     */
    "length" -> """(define (length l)
          (if (null? l)
              0
              (+ 1 (length (cdr l)))))""",
    // "list" is a primop of variadic arity
    "list-ref" -> """(define (list-ref l index)
            (if (= index 0)
              (car l)
              (list-ref (cdr l) (- index 1))))""",
    "list-tail" -> """(define list-tail
                     |  (lambda (x k)
                     |    (if (zero? k)
                     |        x
                     |        (list-tail (cdr x) (- k 1)))))""".stripMargin, // Source: R5RS specification
    "list?" -> "(define (list? l) (or (and (pair? l) (list? (cdr l))) (null? l)))",
    // "log" is a primop
    "max" -> "(define (max a b) (if (< a b) b a)", // variadic
    "member" -> """(define (member e l)
          (if (null? l)
            #f
            (if (equal? (car l) e)
              l
              (member e (cdr l)))))""",
    "memq" -> """(define (memq e l)
          (if (null? l)
            #f
            (if (eq? (car l) e)
              l
              (memq e (cdr l)))))""",
    "min" -> "(define (min a b) (if (< a b) a b)", // variadic
    // "modulo" is a primop
    "negative?" -> "(define (negative? x) (< x 0))",
    "newline" -> "(define (newline) #f)", // undefined
    "not" -> "(define (not x) (if x #f #t))",
    // "null?" is a primop
    // "number->string" is a primop
    // "number?" is a primop
    "odd?" -> "(define (odd? x) (= 1 (modulo x 2)))",
    // "pairp?" is a primop, or is it? TODO
    "positive?" -> "(define (positive? x) (> x 0))",
    // "quotient" is a primop
    // "real?" is a primop
    // "remainder" is a primop
    // "round" is a primop
    // "set-car!" is a primop
    // "set-cdr!" is a primop
    // "sin" is a primop
    // TODO: sqrt
    // "string->symbol" is a primop
    // "string-append" is a primop
    // "string-length" is a primop
    // "string-ref" is a primop
    // "string-lt" is a primop
    // "string?" is a primop
    // "symbol->string" is a primop
    // "tan" is a primop
    // TODO: make-vector
    // TODO: vector
    // TODO: vector-length
    // TODO: vector-ref
    // TODO: vector-set
    // TODO: vector?
    "zero?" -> "(define (zero? x) (= x 0))",
    // "<" is a primop
    "<=" -> "(define (<= x y) (or (< x y) (= x y)))",
    // "=" is a primop
    // ">" is a primop
    ">=" -> "(define (>= x y) (or (> x y) (= x y)))",

    "caar" -> "(define (caar x) (car (car x)))",
    "cadr" -> "(define (cadr x) (car (cdr x)))",
    "cdar" -> "(define (cdar x) (cdr (car x)))",
    "cddr" -> "(define (cddr x) (cdr (cdr x)))",
    "caaar" -> "(define (caaar x) (car (car (car x))))",
    "caadr" -> "(define (caadr x) (car (car (cdr x))))",
    "cadar" -> "(define (cadar x) (car (cdr (car x))))",
    "caddr" -> "(define (caddr x) (car (cdr (cdr x))))",
    "cdaar" -> "(define (cdaar x) (cdr (car (car x))))",
    "cdadr" -> "(define (cdadr x) (cdr (car (cdr x))))",
    "cddar" -> "(define (cddar x) (cdr (cdr (car x))))",
    "cdddr" -> "(define (cdddr x) (cdr (cdr (cdr x))))",
    "caaaar" -> "(define (caaaar x) (car (car (car (car x)))))",
    "caaadr" -> "(define (caaadr x) (car (car (car (cdr x)))))",
    "caadar" -> "(define (caadar x) (car (car (cdr (car x)))))",
    "caaddr" -> "(define (caaddr x) (car (car (cdr (cdr x)))))",
    "cadaar" -> "(define (cadaar x) (car (cdr (car (car x)))))",
    "cadadr" -> "(define (cadadr x) (car (cdr (car (cdr x)))))",
    "caddar" -> "(define (caddar x) (car (cdr (cdr (car x)))))",
    "cadddr" -> "(define (cadddr x) (car (cdr (cdr (cdr x)))))",
    "cdaaar" -> "(define (cdaaar x) (cdr (car (car (car x)))))",
    "cdaadr" -> "(define (cdaadr x) (cdr (car (car (cdr x)))))",
    "cdadar" -> "(define (cdadar x) (cdr (car (cdr (car x)))))",
    "cdaddr" -> "(define (cdaddr x) (cdr (car (cdr (cdr x)))))",
    "cddaar" -> "(define (cddaar x) (cdr (cdr (car (car x)))))",
    "cddadr" -> "(define (cddadr x) (cdr (cdr (car (cdr x)))))",
    "cdddar" -> "(define (cdddar x) (cdr (cdr (cdr (car x)))))",
    "cddddr" -> "(define (cddddr x) (cdr (cdr (cdr (cdr x)))))",
    )

  val names: Set[String] = primitives.keySet

  def scalaSource: String = primitives.values.map(src => PrimCompiler.compile(SchemeParser.parse(src))).mkString("\n\n")

  // Chooses which functions to append to the beginning of a file by over-approximation. Avoids having to attach the entire prelude.
  def addPrelude(exp: SchemeExp): SchemeExp = {
    var prelude: Set[SchemeExp] = Set()
    var work: List[Expression] = List(exp)

    while (work.nonEmpty) {
      val hd :: tl = work
      work = tl
      hd match {
          case Identifier(name, _) if names.contains(name) =>
            val exp = SchemeParser.parse(primitives(name))
            prelude = prelude + exp
            work = exp :: work // If a primitive depends on other primitives, make sure to also inline them.
          case e => work = e.subexpressions ::: work
      }
    }
    SchemeBegin(prelude.toList ::: List(exp), Identity.none)
  }

  // Parses a file and automatically adds the required prelude (over-approximated).
  def parseWithPrelude(path: String): SchemeExp = addPrelude(SchemeParser.parse(FileUtil.loadFile(path)))
}

object CompileTest extends App {
  print(Primitives.scalaSource)
}