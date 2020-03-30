package scalaam.primitiveCompilation

import scalaam.core.{Expression, Identifier, Identity}
import scalaam.io.Reader
import scalaam.language.scheme.{SchemeBegin, SchemeExp, SchemeParser}

object PrimitiveDefinitions {
  val definitions = Map(
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
         (if (eq? (caar l) k)
           (car l)
           (assq k (cdr l)))))""",
    "assv" -> """(define (assv k l)
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
    "list-tail" -> """(define (list-tail x k)
                     |    (if (zero? k)
                     |        x
                     |        (list-tail (cdr x) (- k 1))))""".stripMargin, // Based on definition in R5RS specification.
    "list?" -> "(define (list? l) (or (and (pair? l) (list? (cdr l))) (null? l)))",
    // "log" is a primop
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
//      "map" ->  """(define (map f l)
//  (if (null? l)
//      l
//      (if (pair? l)
//          (cons (f (car l)) (map f (cdr l)))
//          (error "Cannot map over a non-list"))))""",
//    "for-each" -> """(define (for-each f l)
//  (if (null? l)
//      #t
//      (if (pair? l)
//          (begin (f (car l)) (for-each f (cdr l)))
//          (error "Cannot for-each over a non-list"))))""",
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

  val names: Set[String] = definitions.keySet
  val primitivePrecision: Set[String] = names ++ Set(
    "map", "for-each", "foldr", "foldr-aux", "foldl", "foldl-aux",
    "foo", "bar", "baz",
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

  //def scalaSource: String = primitives.values.map(src => PrimCompiler.compile(ANFCompiler.toANF(SchemeParser.parse(src)))).mkString("\n\n")

  // Chooses which functions to append to the beginning of a file by over-approximation. Avoids having to attach the entire prelude.
  def addPrelude(exp: SchemeExp): SchemeExp = {
    var prelude: Set[SchemeExp] = Set()
    var work: List[Expression] = List(exp)
    var visited: List[String] = List()
    var calls = 0

    while (work.nonEmpty) {
      val hd :: tl = work
      work = tl
      hd match {
        case Identifier(name, _) if names.contains(name) =>
          calls = calls+1
          if (!visited.contains(name)) {
            // println(s"Found primitive: $name")
            val exp = SchemeParser.parse(definitions(name))
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

  def computeUsage(benchmarks: List[String]): Unit = {

    def primitiveUsage(exp: SchemeExp, prev: Map[String, Int]): Map[String, Int] = {
      var work: List[Expression] = List(exp)
      var visited: List[String] = List()
      var primitives: Map[String, Int] = prev // Map().withDefaultValue(0)

      while (work.nonEmpty) {
        val hd :: tl = work
        work = tl
        hd match {
          case Identifier(name, _) if PrimitiveDefinitions.names.contains(name) =>
            if (!visited.contains(name)) {
              primitives = primitives + (name -> (primitives(name) + 1))
              work = exp :: work // If a primitive depends on other primitives, make sure to also inline them.
              visited = name :: visited
            }
          case e => work = e.subexpressions ::: work
        }
      }
      primitives
    }

    benchmarks.foldLeft(Map[String, Int]().withDefaultValue(0)){case(curr, path) =>
      try {
        primitiveUsage(SchemeParser.parse(Reader.loadFile(path)), curr)
      } catch {
        case _: Throwable =>
          System.err.println(s"Error when investigating $path.")
          curr
      }
    }.toList.sortBy(_._2).reverse.foreach(println)
  }

  // Parses a file and automatically adds the required prelude (over-approximated).
  def parseWithPrelude(path: String): SchemeExp = addPrelude(SchemeParser.parse(Reader.loadFile(path)))
  def parseWithoutPrelude(path: String): SchemeExp = SchemeParser.parse(Reader.loadFile(path))
}

object SchemeBenchmarks {

  val ad: Set[String] = Set(
    "test/ad/abstrct.scm",
    //"test/ad/bfirst.scm", // VARARG
    // "test/ad/bst.scm", // VARARG
    //"test/ad/btree.scm", // TODO add a body
    "test/ad/bubsort.scm",
    "test/ad/dict.scm",
    // "test/ad/dictExamples.scm", // EMPTY
    //"test/ad/heap.scm", // PARSER ERROR TODO
    "test/ad/inssort.scm",
    //"test/ad/linear.scm", // VARARG
    //"test/ad/list.scm", // VARARG
    "test/ad/mesort.scm",
    "test/ad/prioq.scm",
    "test/ad/qsort.scm",
    "test/ad/qstand.scm",
    //"test/ad/queue.scm", // VARARG
    "test/ad/quick.scm",
    //"test/ad/RBtreeADT.scm", // VARARG
    //"test/ad/selsort.scm", // PARSER ERROR TODO
    "test/ad/stack.scm",
    //"test/ad/stspaceCODE.scm", // VARARG
  )

  val gabriel: Set[String] = Set(
//    "test/gabriel/boyer.scm",
    "test/gabriel/browse.scm",
    "test/gabriel/cpstak.scm",
    "test/gabriel/dderiv.scm",
    "test/gabriel/deriv.scm",
    "test/gabriel/destruc.scm",
    "test/gabriel/diviter.scm",
    "test/gabriel/divrec.scm",
//    "test/gabriel/puzzle.scm",
    "test/gabriel/takl.scm",
//    "test/gabriel/triangl.scm",
  )

  val gambit: Set[String] = Set(
    "test/gambit/array1.scm",
    "test/gambit/browse.scm",
    "test/gambit/cat.scm",
    //"test/gambit/compiler.scm", // PARSER ERROR TODO
    "test/gambit/ctak.scm",
    "test/gambit/deriv.scm",
    "test/gambit/destruc.scm",
    "test/gambit/diviter.scm",
    "test/gambit/earley.scm",
    "test/gambit/fibc.scm",
    "test/gambit/graphs.scm",
    "test/gambit/lattice.scm",
    "test/gambit/matrix.scm",
    "test/gambit/mazefun.scm",
    //"test/gambit/nboyer.scm", // VARARG
    "test/gambit/nqueens.scm",
    "test/gambit/paraffins.scm",
    "test/gambit/perm9.scm",
    //"test/gambit/peval.scm", // VARARG
    "test/gambit/primes.scm",
    "test/gambit/puzzle.scm",
    //"test/gambit/sboyer.scm", // VARARG
    //"test/gambit/scheme.scm", // VARARG
    //"test/gambit/slatex.scm", // PARSER LIMITATION TODO
    "test/gambit/string.scm",
    "test/gambit/sum.scm",
    "test/gambit/sumloop.scm",
    "test/gambit/tail.scm",
    "test/gambit/tak.scm",
    //"test/gambit/trav1.scm", // PARSER ERROR TODO
    "test/gambit/triangl.scm",
    "test/gambit/wc.scm",
  )

  val rosetta: Set[String] = Set(
    "test/rosetta/easter.scm",
    "test/rosetta/quadratic.scm",
  )

  val scp1: Set[String] = Set(
    "test/scp1/2.1.scm",
    "test/scp1/2.4.scm",

    "test/scp1/3.1.scm",
    "test/scp1/3.2.1.scm",
    "test/scp1/3.2.scm",
    "test/scp1/3.3.scm",
    "test/scp1/3.4.scm",
    "test/scp1/3.6.scm",
    "test/scp1/3.8.scm",
    //"test/scp1/3.9.scm", // LOOPS, EVEN WITH FINER TIMEOUT TODO

    "test/scp1/4.1.scm", // LOOPS, EVEN WITH FINER TIMEOUT TODO
    "test/scp1/4.8.scm",

    "test/scp1/5.6.scm",
    "test/scp1/5.7.scm",
    "test/scp1/5.14.3.scm",
    "test/scp1/5.19.scm",
    "test/scp1/5.20.4.scm",
    "test/scp1/5.21.scm",
    "test/scp1/5.22.scm",

    "test/scp1/7.2.scm",
    "test/scp1/7.3.scm",
    "test/scp1/7.4.scm",
    // "test/scp1/7.5.scm", // DOT NOTATION
    // "test/scp1/7.6.scm", // DOT NOTATION
    //"test/scp1/7.9.scm",
    "test/scp1/7.11.scm",
    "test/scp1/7.12.scm",
    //"test/scp1/7.13.scm", // SOMETIMES LOOPS TODO
    "test/scp1/7.14.scm",
    "test/scp1/7.15.scm",
    "test/scp1/7.16.scm",
    "test/scp1/7.17.scm",
    "test/scp1/7.18.scm",

    "test/scp1/8.1.1.scm",
    "test/scp1/8.1.3.scm",
    //"test/scp1/8.5.scm", // VARARG
    "test/scp1/8.6.scm",
    //"test/scp1/8.10.scm", // SMALLSTEP LOOPS, EVEN WITH FINER TIMEOUT TODO
    //"test/scp1/8.11.scm", // VARARG
    "test/scp1/8.12.scm",
    "test/scp1/8.13.scm",
    "test/scp1/8.14.scm",
    "test/scp1/8.15.scm",
    //"test/scp1/8.16.scm", // VARARG

    "test/scp1/9.2.scm",
    "test/scp1/9.3.scm",
    "test/scp1/9.5.scm",
    "test/scp1/9.6.scm",
    "test/scp1/9.7.scm",
    "test/scp1/9.8.scm",
    "test/scp1/9.9.scm",
    "test/scp1/9.12.scm",
    "test/scp1/9.13.scm",
    "test/scp1/9.14.scm",
    "test/scp1/9.15.scm",
    "test/scp1/9.16.scm",
    "test/scp1/9.17.scm",
    "test/scp1/9.18.scm",
  )

  val sigscheme: Set[String] = Set(
    "test/sigscheme/arithint.scm",
    "test/sigscheme/case.scm",
    "test/sigscheme/let-loop.scm",
    "test/sigscheme/loop.scm",
    "test/sigscheme/mem.scm",
    "test/sigscheme/rec.scm",
    "test/sigscheme/takr.scm",
  )

  val theLittleSchemer: Set[String] = Set(
    "test/WeiChenRompf2019/the-little-schemer/ch1.scm",
    "test/WeiChenRompf2019/the-little-schemer/ch2.scm",
    "test/WeiChenRompf2019/the-little-schemer/ch3.scm",
    "test/WeiChenRompf2019/the-little-schemer/ch4.scm",
    "test/WeiChenRompf2019/the-little-schemer/ch5.scm",
    //"test/WeiChenRompf2019/the-little-schemer/ch6.scm", // PARSER LIMITATION TODO check whether needed
    "test/WeiChenRompf2019/the-little-schemer/ch7.scm",
    //"test/WeiChenRompf2019/the-little-schemer/ch8.scm", // PARSER LIMITATION TODO check whether needed
    //"test/WeiChenRompf2019/the-little-schemer/ch9.scm", // UNSUPPORTED FEATURE? (lambda args body)
    "test/WeiChenRompf2019/the-little-schemer/ch10.scm",
  )

  val toplas98: Set[String] = Set(
    //"test/WeiChenRompf2019/toplas98/boyer.sch", // USES SQUARE BRACKETS
    //"test/WeiChenRompf2019/toplas98/dynamic.sch", // PARSER LIMITATION TODO
    //"test/WeiChenRompf2019/toplas98/graphs.sch", // MISSING PRIMITIVE open-input-file
    //"test/WeiChenRompf2019/toplas98/handle.scm", // MAYBE INVALID SCHEME PROGRAM TODO check
    //"test/WeiChenRompf2019/toplas98/lattice.scm", // PARSER ERROR TODO
    //"test/WeiChenRompf2019/toplas98/lattice-processed.scm", // PARSER ERROR TODO
    //"test/WeiChenRompf2019/toplas98/maze.sch", // PARSER ERROR: #\space is interpreted as #\s pace
    //"test/WeiChenRompf2019/toplas98/nbody.sch", // PARSER LIMITATION TODO
    //"test/WeiChenRompf2019/toplas98/nbody-processed.sch", // PARSER LIMITATION TODO
    //"test/WeiChenRompf2019/toplas98/nucleic.sch", // PARSER ERROR TODO
    //"test/WeiChenRompf2019/toplas98/nucleic2.sch", // USES MACROS: define-syntax
    //"test/WeiChenRompf2019/toplas98/splay.scm", // PARSER ERROR
  )

  val WCR2019: Set[String] = Set(
    //"test/WeiChenRompf2019/church_exp.sch", // PARSER LIMITATION TODO // Unknown (void) function
    "test/WeiChenRompf2019/church_simple.sch",
    //"test/WeiChenRompf2019/earley.sch", // MISSING PRIMITIVE read TODO
    "test/WeiChenRompf2019/fermat.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-16.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-32.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-64.scm",
    "test/WeiChenRompf2019/kcfa-worst-case-256.scm",
    "test/WeiChenRompf2019/kcfa3.scm",
    //"test/WeiChenRompf2019/mbrotZ.sch", // PARSER ERROR TODO
    //"test/WeiChenRompf2019/meta-circ.scm", // UNSUPPORTED FEATURE? (lambda args body)
    //"test/WeiChenRompf2019/omega.scm", // STACKOVERFLOW CONCRETE MACHINE
    "test/WeiChenRompf2019/regex-derivative.scm",
    "test/WeiChenRompf2019/rsa.scm",
    "test/WeiChenRompf2019/scheme2java.scm",
    "test/WeiChenRompf2019/solovay-strassen.scm",
  )

  val other: Set[String] = Set(
    "test/blur.scm",
    "test/bound-precision.scm",
    "test/church-2-num.scm",
    "test/church-6.scm",
    "test/church.scm",
    "test/collatz.scm",
    "test/count.scm",
    "test/eta.scm",
    "test/fact.scm",
    "test/fib.scm",
    "test/gcipd.scm",
    "test/grid.scm",
    "test/inc.scm",
    "test/infinite-1.scm",
    "test/infinite-2.scm",
    "test/infinite-3.scm",
    "test/kcfa2.scm",
    "test/kcfa3.scm",
    "test/kernighanvanwyk/ack.scm",
    "test/letrec-begin.scm",
    "test/loop2.scm",
    "test/mceval.scm",
    "test/mj09.scm",
    "test/mut-rec.scm",
    "test/my-list.scm",
    "test/nested-defines.scm",
    "test/primtest.scm",
    "test/quasiquoting-simple.scm",
    "test/quasiquoting.scm",
    "test/regex.scm",
    "test/rotate.scm",
    "test/rsa.scm",
    "test/sat.scm",
    //"test/scm2c.scm",     // various unsupported primitives
    //"test/scm2java.scm",  // various unsupported primitives
    "test/sq.scm",
    //"test/Streams.scm",   // define-macro
    "test/sym.scm",
    "test/widen.scm",
    "test/work.scm",
  )

  val WeiChenRompf2019: Set[String] = WCR2019 ++ theLittleSchemer ++ toplas98
  val    allBenchmarks: Set[String] = ad ++ gabriel ++ gambit ++ rosetta ++ scp1 ++ sigscheme ++ WeiChenRompf2019 ++ other
}
