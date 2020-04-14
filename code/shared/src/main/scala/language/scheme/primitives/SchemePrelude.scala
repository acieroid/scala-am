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
    "max" -> "(define (max a b) (if (< a b) b a))", // variadic
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
    "min" -> "(define (min a b) (if (< a b) a b))", // variadic
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

  val primNames: Set[String] = primDefs.keySet
  val primPrecision: Set[String] = primDefs.keySet ++ Set(
    "foldr", "foldr-aux", "foldl", "foldl-aux",
    // mceval.scm
    "self-evaluating?", "variable?", "tagged-list?", "quoted?", "text-of-quotation", "assignment?",
    "assignment-variable", "assignment-value", "definition?", "definition-variable", "make-lambda",
    "definition-value", "lambda?", "lambda-parameters", "lambda-body", "if?", "if-predicate", "if-consequent",
    "if-alternative", "make-if", "begin?", "begin-actions", "last-exp?", "first-exp", "rest-exps", "mk-begin",
    "sequence->exp", "application?", "operator", "operands", "no-operands?", "rest-operands",
    "cond?", "cond-clauses", "cond-predicate", "cond-else-clause?", "cond-actions", "cond->if",
    "true?", "false?", "make-procedure", "compound-procedure?", "procedure-parameters", "procedure-body",
    "procedure-environment", "enclosing-environment", "first-frame", "make-frame", "frame-variables", "frame-values",
    "add-binding-to-frame!", "extend-environment", "primitive-procedure", "primitive-implementation",
    "expand-clauses", "lookup-variable-value", "set-variable-value!", "define-variable!",
    // gambit/peval.scm
    "every?", "some?", "map2", "get-last-pair", "alphabetize", "const-expr?", "const-value", "quot", "not-constant?", "remove-constant", "extract-constant", "beta-subst", "binding-frame", "bound-expr", "add-binding", "for-each!", "arg-pattern", "sum", "product", "reduce-global", "constant-fold-global",
    // scp1/9.12.scm
    "find-last", "flatten!", "atom?", "flatten2!",
    // gabriel/browse.scm
    "lookup", "get", "put", "append-to-tail!", "tree-copy",
    // scp1/8.15.scm
    "maak-buffer", "newValue", "returnSum", "flush", "value",
    "maak-verkeersteller", "newCar", "newHour", "newDay", "loop",
    // gambit/mazefun.scm
    "for", "concat", "list-read", "list-write", "list-remove-pos", "duplicates?", "make-matrix", "matrix-read", "matrix-write", "matrix-size", "matrix-map", "shuffle", "shuffle-aux", "make-maze", "cave-to-maze", "pierce", "pierce-randomly", "try-to-pierce", "change-cavity", "change-cavity-aux", "neighboring-cavities",
    // gabriel/diviter.scm
    "create-n", "iterate-div2",
    // gabriel/divrec.scm
    "recursive-div2",
    // scp1/9.18.scm
    "first-el", "smaller?", "same?", "merge", "merge-in",
    // scp1/5.14.3.scm
    "super-merge-n", "geef-n+rest",
    // scp1/7.16.scm"
    "omzetcijfer", "heeft-omzetcijfer", "deel-categorien", "hoofdcategorie", "bereken", "omzet", "omzet-in", "collect-pairs", "collect-pairs-in", "verdeel-democratisch", "verdeel", "verdeel-in",
    // scp1/9.16.scm"
    "insert-aux!", "insert!",
    // gambit/destruc.scm
    "destructive",
    // gabriel/dderiv.scm
    "dderiv", "my+dderiv", "my-dderiv", "*dderiv", "/dderiv",
    // scp1/7.11.scm
    "baas", "sub-organigrammen", "hierarchisch?", "hierarchisch?-in", "collegas", "collegas-in", "werknemers-in", "werknemers",
    // scp1/7.17.scm
    "familiehoofd", "kinderen", "laatste-nakomeling?", "verdeel-democratisch", "verdeel", "verdeel-in", "budget", "budget-hulp", "budget-hulp-in",
    // scp1/9.14.scm
    "schuif-in!",
    // scp1/7.9.scm
    "blad?", "appel?", "type", "leafs", "all-apples", "conditional-append", "apple-types", "bewerk-boom", "leafs-dmv-bewerk", "all-apples-dmv-bewerk", "apple-types-dmv-bewerk",
    // scp1/7.15
    "maak-blad", "geef-type", "maak-knop", "geef-deelbomen", "maak-hybride-tak", "geef-knopen", "leeg?", "knoop?", "blad?", "tel", "combine-results", "tel-hulp", "tel-hulp-in", "member?", "normaal?", "check-normaal", "check-normaal-in", 
  )

  def addPrelude(exp: SchemeExp): SchemeExp = {
    var prelude: Set[SchemeExp] = Set()
    var work: List[Expression] = List(exp)
    var visited: List[String] = List()
    var calls = 0

    while (work.nonEmpty) {
      val hd :: tl = work
      work = tl
      hd match {
        case Identifier(name, _) if primNames.contains(name) =>
          calls = calls+1
          if (!visited.contains(name)) {
            // println(s"Found primitive: $name")
            val exp = SchemeParser.parse(primDefs(name))
            prelude = prelude + exp
            work = exp :: work // If a primitive depends on other primitives, make sure to also inline them.
            visited = name :: visited
          }
        case e => work = e.subexpressions ::: work
      }
    }
    // println(s"Distinct primitive calls: $calls")
    SchemeBegin(prelude.toList ::: List(exp), Identity.none)
  }
}
