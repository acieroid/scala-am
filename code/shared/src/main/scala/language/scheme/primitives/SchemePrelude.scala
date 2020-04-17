package scalaam.language.scheme.primitives

import scalaam.core._
import scalaam.language.scheme._

object SchemePrelude {

  val primDefs = Map(
    "abs" -> "(define (abs x) (if (< x 0) (- 0 x) x))",
    // TODO: disabled, this will be defined manually
//    "append" -> """(define (append l1 l2)
//          (if (null? l1)
//              l2
//              (cons (car l1)
//                    (append (cdr l1) l2))))""",
    "assoc" -> """(define (assoc k l)
        (if (null? l)
          #f
         (if (equal? (caar l) k)
           (car l)
           (assoc k (cdr l)))))""",
    "assq" -> """(define (assq k l)
        (if (null? l)
          #f
         (if (eq? (caar l) k)
           (car l)
           (assq k (cdr l)))))""",
    "assv" -> """(define (assv k l)
        (if (null? l)
          #f
         (if (eqv? (caar l) k)
           (car l)
           (assq k (cdr l)))))""",

    // "cons" is a primop? TODO maybe not exactly (could define in terms of vectors of size 2)
    "display" -> "(define (display x) x)", // undefined behavior in R5RS
    // TODO: vectors in equal? (requires a loop, see SchemePrimitives.scala)
    "equal?" -> """(define (equal? a b)
          (or (eq? a b)
            (and (null? a) (null? b))
            (and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))""",
    "eqv?" -> "(define (eqv? x y) (eq? x y))",
    "even?" -> "(define (even? x) (= 0 (modulo x 2)))",
    // TODO: expt // TODO isn't this a primop (easier to handle real exponents).
    // TODO: exp
    "gcd" -> "(define (gcd a b) (if (= b 0) a (gcd b (modulo a b))))",
    "lcm" -> "(define (lcm m n) (/ (abs (* m n)) (gcd m n)))",
    "length" -> """(define (length l)
          (if (null? l)
              0
              (+ 1 (length (cdr l)))))""",
    "list-ref" -> """(define (list-ref l index)
            (if (= index 0)
              (car l)
              (list-ref (cdr l) (- index 1))))""",
    "list-tail" -> """(define (list-tail x k)
                     |    (if (zero? k)
                     |        x
                     |        (list-tail (cdr x) (- k 1))))""".stripMargin, // Based on definition in R5RS specification.
    "list?" -> "(define (list? l) (or (and (pair? l) (list? (cdr l))) (null? l)))",
    //"max" -> "(define (max a b) (if (< a b) b a))", // Variadic => implemented manually.
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
    //"min" -> "(define (min a b) (if (< a b) a b))", // Variadic => implemented manually.
    "negative?" -> "(define (negative? x) (< x 0))",
    "newline" -> "(define (newline) #f)", // undefined
    "not" -> "(define (not x) (if x #f #t))",
    "odd?" -> "(define (odd? x) (= 1 (modulo x 2)))",
    // "pairp?" is a primop, or is it? TODO
    "positive?" -> "(define (positive? x) (> x 0))",
    // TODO: sqrt
    // TODO: make-vector
    // TODO: vector
    // TODO: vector-length
    // TODO: vector-ref
    // TODO: vector-set
    // TODO: vector?
    "zero?" -> "(define (zero? x) (= x 0))",
    "<=" -> "(define (<= x y) (or (< x y) (= x y)))",
    ">" -> "(define (> x y) (not (<= x y)))",
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
    "reverse" -> """(define (reverse l)
      (if (null? l)
          ()
          (append (reverse (cdr l))
                  (list (car l)))))""",
    "map" ->  """(define (map f l)
      (if (null? l)
          l
          (if (pair? l)
              (cons (f (car l)) (map f (cdr l)))
              (error "Cannot map over a non-list."))))""",
    "for-each" -> """(define (for-each f l)
      (if (null? l)
          #t
          (if (pair? l)
              (begin (f (car l)) (for-each f (cdr l)))
              (error "Cannot for-each over a non-list."))))""",
    //    "foldr" -> """(define (foldr f base lst) (foldr-aux f base lst))""",
    //    "foldr-aux" -> """(define (foldr-aux f base lst)
    //        (if (null? lst)
    //            base
    //            (f (car lst) (foldr-aux f base (cdr lst)))))""",
    //    "foldl" -> """(define (foldl f base lst) (foldl-aux f base lst))""",
    //    "foldl-aux" -> """(define (foldl-aux f base lst)
    //        (if (null? lst)
    //            base
    //            (foldl-aux f (f base (car lst)) (cdr lst))))"""
  )

  val primDefsParsed = primDefs.map { 
    case (nam,str) => 
      val exp = SchemeParser.parse(str,Position.newTag(nam))
      (nam,exp)
  }

  val primNames: Set[String] = primDefs.keySet

  def addPrelude(exp: SchemeExp): SchemeExp = {
    var prelude: List[ SchemeExp] = List()
    var work:    List[Expression] = List(exp)
    var visited:  Set[    String] = Set()

    while (work.nonEmpty) {
      work.head match {
        case Identifier(name, _) if primNames.contains(name) && !visited.contains(name) =>
            val exp = primDefsParsed(name)
            prelude = exp :: prelude
            work = exp :: work.tail // If a primitive depends on other primitives, make sure to also inline them.
            visited = visited + name
        case e => work = e.subexpressions ::: work.tail // There will be no subexpressions if e is an Identifier for which the conditions do not hold.
      }
    }
    SchemeBody(prelude ::: List(exp))
  }
}
