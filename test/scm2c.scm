(define (void) (if #f #t))

(define (tagged-list? tag l)
  (and (pair? l)
       (eq? tag (car l))))

(define (char->natural c)
  (let ((i (char->integer c)))
    (if (< i 0)
        (* -2 i)
        (+ (* 2 i) 1))))

(define (integer->char-list n)
  (string->list (number->string n)))

(define gensym-count 0)

(define gensym (lambda (param)
                 (set! gensym-count (+ gensym-count 1))
                 (string->symbol (string-append
                                  (if (symbol? param)
                                      (symbol->string param)
                                      param)
                                  "$"
                                  (number->string gensym-count)))))

(define (member sym S)
  (if (not (pair? S))
      #f
      (if (eq? sym (car S))
          #t
          (member sym (cdr S)))))

(define (symbol<? sym1 sym2)
  (string<? (symbol->string sym1)
            (symbol->string sym2)))

(define (insert sym S)
  (if (not (pair? S))
      (list sym)
      (cond
        ((eq? sym (car S))       S)
        ((symbol<? sym (car S))  (cons sym S))
        (else (cons (car S) (insert sym (cdr S)))))))

(define (remove sym S)
  (if (not (pair? S))
      '()
      (if (eq? (car S) sym)
          (cdr S)
          (cons (car S) (remove sym (cdr S))))))

(define (union set1 set2)
  (if (not (pair? set1))
      set2
      (insert (car set1) (union (cdr set1) set2))))

(define (difference set1 set2)
  (if (not (pair? set2))
      set1
      (difference (remove (car set2) set1) (cdr set2))))

(define (reduce f lst init)
  (if (not (pair? lst))
      init
      (reduce f (cdr lst) (f (car lst) init))))

(define (azip list1 list2)
  (if (and (pair? list1) (pair? list2))
      (cons (list (car list1) (car list2))
            (azip (cdr list1) (cdr list2)))
      '()))

(define (assq-remove-key env key)
  (if (not (pair? env))
      '()
      (if (eq? (car (car env)) key)
          (assq-remove-key (cdr env) key)
          (cons (car env) (assq-remove-key (cdr env) key)))))

(define (assq-remove-keys env keys)
  (if (not (pair? keys))
      env
      (assq-remove-keys (assq-remove-key env (car keys)) (cdr keys))))

(define (const? exp)
  (or (integer? exp)
      (boolean? exp)))

(define (ref? exp)
  (symbol? exp))

(define (let? exp)
  (tagged-list? 'let exp))

(define (let->bindings exp)
  (cadr exp))

(define (let->exp exp)
  (caddr exp))

(define (let->bound-vars exp)
  (map car (cadr exp)))

(define (let->args exp)
  (map cadr (cadr exp)))

(define (letrec? exp)
  (tagged-list? 'letrec exp))

(define (letrec->bindings exp)
  (cadr exp))

(define (letrec->exp exp)
  (caddr exp))

(define (letrec->bound-vars exp)
  (map car (cadr exp)))

(define (letrec->args exp)
  (map cadr (cadr exp)))

(define (lambda? exp)
  (tagged-list? 'lambda exp))

(define (lambda->formals exp)
  (cadr exp))

(define (lambda->exp exp)
  (caddr exp))

(define (if? exp)
  (tagged-list? 'if exp))

(define (if->condition exp)
  (cadr exp))

(define (if->then exp)
  (caddr exp))

(define (if->else exp)
  (cadddr exp))

(define (app? exp)
  (pair? exp))

(define (app->fun exp)
  (car exp))

(define (app->args exp)
  (cdr exp))

(define (prim? exp)
  (or (eq? exp '+)
      (eq? exp '-)
      (eq? exp '*)
      (eq? exp '=)
      (eq? exp 'display)))

(define (begin? exp)
  (tagged-list? 'begin exp))

; begin->exps : begin-exp -> list[exp]
(define (begin->exps exp)
  (cdr exp))

; set! : exp -> boolean
(define (set!? exp)
  (tagged-list? 'set! exp))

; set!->var : set!-exp -> var
(define (set!->var exp)
  (cadr exp))

; set!->exp : set!-exp -> exp
(define (set!->exp exp)
  (caddr exp))

; closure? : exp -> boolean
(define (closure? exp)
  (tagged-list? 'closure exp))

; closure->lam : closure-exp -> exp
(define (closure->lam exp)
  (cadr exp))

; closure->env : closure-exp -> exp
(define (closure->env exp)
  (caddr exp))

; env-make? : exp -> boolean
(define (env-make? exp)
  (tagged-list? 'env-make exp))

; env-make->id : env-make-exp -> env-id
(define (env-make->id exp)
  (cadr exp))

; env-make->fields : env-make-exp -> list[symbol]
(define (env-make->fields exp)
  (map car (cddr exp)))

; env-make->values : env-make-exp -> list[exp]
(define (env-make->values exp)
  (map cadr (cddr exp)))

; env-get? : exp -> boolen
(define (env-get? exp)
  (tagged-list? 'env-get exp))

; env-get->id : env-get-exp -> env-id
(define (env-get->id exp)
  (cadr exp))

; env-get->field : env-get-exp -> symbol
(define (env-get->field exp)
  (caddr exp))

; env-get->env : env-get-exp -> exp
(define (env-get->env exp)
  (cadddr exp))

; set-cell!? : set-cell!-exp -> boolean
(define (set-cell!? exp)
  (tagged-list? 'set-cell! exp))

; set-cell!->cell : set-cell!-exp -> exp
(define (set-cell!->cell exp)
  (cadr exp))

; set-cell!->value : set-cell!-exp -> exp
(define (set-cell!->value exp)
  (caddr exp))

; cell? : exp -> boolean
(define (cell? exp)
  (tagged-list? 'cell exp))

; cell->value : cell-exp -> exp
(define (cell->value exp)
  (cadr exp))

; cell-get? : exp -> boolean
(define (cell-get? exp)
  (tagged-list? 'cell-get exp))

; cell-get->cell : cell-exp -> exp
(define (cell-get->cell exp)
  (cadr exp))

(define (substitute-var env var)
  (let ((sub (assq var env)))
    (if sub
        (cadr sub)
        var)))

(define (substitute env exp)

  (define (substitute-with env)
    (lambda (exp)
      (substitute env exp)))

  (cond
    ((null? env)        exp)
    ((const? exp)       exp)
    ((prim? exp)        exp)
    ((ref? exp)         (substitute-var env exp))
    ((lambda? exp)      `(lambda ,(lambda->formals exp)
                           ,(substitute (assq-remove-keys env (lambda->formals exp))
                                        (lambda->exp exp))))
    ((set!? exp)        `(set! ,(substitute-var env (set!->var exp))
                               ,(substitute env (set!->exp exp))))
    ((if? exp)          `(if ,(substitute env (if->condition exp))
                             ,(substitute env (if->then exp))
                             ,(substitute env (if->else exp))))

    ((let? exp)         `(let ,(azip (let->bound-vars exp)
                                     (map (substitute-with env) (let->args exp)))
                           ,(substitute (assq-remove-keys env (let->bound-vars exp))
                                        (let->exp exp))))
    ((letrec? exp)      (let ((new-env (assq-remove-keys env (letrec->bound-vars exp))))
                          `(letrec ,(azip (letrec->bound-vars exp)
                                          (map (substitute-with new-env)
                                               (letrec->args exp)))
                             ,(substitute new-env (letrec->exp exp)))))
    ((begin? exp)       (cons 'begin (map (substitute-with env) (begin->exps exp))))

    ((cell? exp)        `(cell ,(substitute env (cell->value exp))))
    ((cell-get? exp)    `(cell-get ,(substitute env (cell-get->cell exp))))
    ((set-cell!? exp)   `(set-cell! ,(substitute env (set-cell!->cell exp))
                                    ,(substitute env (set-cell!->value exp))))

    ((closure? exp)     `(closure ,(substitute env (closure->lam exp))
                                  ,(substitute env (closure->env exp))))
    ((env-make? exp)    `(env-make ,(env-make->id exp)
                                   ,@(azip (env-make->fields exp)
                                           (map (substitute-with env)
                                                (env-make->values exp)))))
    ((env-get? exp)     `(env-get ,(env-get->id exp)
                                  ,(env-get->field exp)
                                  ,(substitute env (env-get->env exp))))

    ((app? exp)         (map (substitute-with env) exp))
    (else               (error "unhandled expression type in substitution: " exp))))

(define (let=>lambda exp)
  (if (let? exp)
      (let ((vars (map car (let->bindings exp)))
            (args (map cadr (let->bindings exp))))
        `((lambda (,@vars) ,(let->exp exp)) ,@args))
      exp))

(define (letrec=>lets+sets exp)
  (if (letrec? exp)
      (let* ((bindings  (letrec->bindings exp))
             (namings   (map (lambda (b) (list (car b) #f)) bindings))
             (names     (letrec->bound-vars exp))
             (sets      (map (lambda (binding)
                               (cons 'set! binding))
                             bindings))
             (args      (letrec->args exp)))
        `(let ,namings
           (begin ,@(append sets (list (letrec->exp exp))))))))

(define (begin=>let exp)
  (define (singlet? l)
    (and (list? l)
         (= (length l) 1)))

  (define (dummy-bind exps)
    (cond
      ((singlet? exps)  (car exps))

      ((pair? exps)     `(let (($_ ,(car exps)))
                          ,(dummy-bind (cdr exps))))))
  (dummy-bind (begin->exps exp)))

(define (desugar exp)
  (cond
    ((const? exp)      exp)
    ((prim? exp)       exp)
    ((ref? exp)        exp)
    ((lambda? exp)     `(lambda ,(lambda->formals exp)
                          ,(desugar (lambda->exp exp))))
    ((set!? exp)       `(set! ,(set!->var exp) ,(set!->exp exp)))
    ((if? exp)         `(if ,(if->condition exp)
                            ,(if->then exp)
                            ,(if->else exp)))

    ((let? exp)        (desugar (let=>lambda exp)))
    ((letrec? exp)     (desugar (letrec=>lets+sets exp)))
    ((begin? exp)      (desugar (begin=>let exp)))

    ((cell? exp)       `(cell ,(desugar (cell->value exp))))
    ((cell-get? exp)   `(cell-get ,(desugar (cell-get->cell exp))))
    ((set-cell!? exp)  `(set-cell! ,(desugar (set-cell!->cell exp))
                                   ,(desugar (set-cell!->value exp))))

    ((closure? exp)    `(closure ,(desugar (closure->lam exp))
                                 ,(desugar (closure->env exp))))
    ((env-make? exp)   `(env-make ,(env-make->id exp)
                                  ,@(azip (env-make->fields exp)
                                          (map desugar (env-make->values exp)))))
    ((env-get? exp)    `(env-get ,(env-get->id exp)
                                 ,(env-get->field exp)
                                 ,(env-get->env exp)))

    ((app? exp)        (map desugar exp))
    (else              (error "unknown exp: " exp))))

(define (free-vars exp)
  (cond
    ((const? exp)    '())
    ((prim? exp)     '())
    ((ref? exp)      (list exp))
    ((lambda? exp)   (difference (free-vars (lambda->exp exp))
                                 (lambda->formals exp)))
    ((if? exp)       (union (free-vars (if->condition exp))
                            (union (free-vars (if->then exp))
                                   (free-vars (if->else exp)))))
    ((set!? exp)     (union (list (set!->var exp))
                            (free-vars (set!->exp exp))))

    ((let? exp)      (free-vars (let=>lambda exp)))
    ((letrec? exp)   not-handled)
    ((begin? exp)    (reduce union (map free-vars (begin->exps exp)) '()))

    ((cell-get? exp)  (free-vars (cell-get->cell exp)))
    ((cell? exp)      (free-vars (cell->value exp)))
    ((set-cell!? exp) (union (free-vars (set-cell!->cell exp))
                             (free-vars (set-cell!->value exp))))

    ((closure? exp)   (union (free-vars (closure->lam exp))
                             (free-vars (closure->env exp))))
    ((env-make? exp)  (reduce union (map free-vars (env-make->values exp)) '()))
    ((env-get? exp)   (free-vars (env-get->env exp)))

    ((app? exp)       (reduce union (map free-vars exp) '()))
    (else             (error "unknown expression: " exp))))

(define mutable-variables '())

(define (mark-mutable symbol)
  (set! mutable-variables (cons symbol mutable-variables)))

(define (is-mutable? symbol)
  (define (is-in? S)
    (if (not (pair? S))
        #f
        (if (eq? (car S) symbol)
            #t
            (is-in? (cdr S)))))
  (is-in? mutable-variables))

(define (analyze-mutable-variables exp)
  (cond
    ((const? exp)    (void))
    ((prim? exp)     (void))
    ((ref? exp)      (void))
    ((lambda? exp)   (analyze-mutable-variables (lambda->exp exp)))
    ((set!? exp)     (begin (mark-mutable (set!->var exp))
                            (analyze-mutable-variables (set!->exp exp))))
    ((if? exp)       (begin
                       (analyze-mutable-variables (if->condition exp))
                       (analyze-mutable-variables (if->then exp))
                       (analyze-mutable-variables (if->else exp))))

    ((let? exp)      (begin
                       (map analyze-mutable-variables (map cadr (let->bindings exp)))
                       (analyze-mutable-variables (let->exp exp))))
    ((letrec? exp)   (begin
                       (map analyze-mutable-variables (map cadr (letrec->bindings exp)))
                       (analyze-mutable-variables (letrec->exp exp))))
    ((begin? exp)    (begin
                       (map analyze-mutable-variables (begin->exps exp))
                       (void)))

    ((app? exp)      (begin
                       (map analyze-mutable-variables exp)
                       (void)))
    (else            (error "unknown expression type: " exp))))


(define (wrap-mutables exp)

  (define (wrap-mutable-formals formals body-exp)
    (if (not (pair? formals))
        body-exp
        (if (is-mutable? (car formals))
            `(let ((,(car formals) (cell ,(car formals))))
               ,(wrap-mutable-formals (cdr formals) body-exp))
            (wrap-mutable-formals (cdr formals) body-exp))))

  (cond
    ((const? exp)    exp)
    ((ref? exp)      (if (is-mutable? exp)
                         `(cell-get ,exp)
                         exp))
    ((prim? exp)     exp)
    ((lambda? exp)   `(lambda ,(lambda->formals exp)
                        ,(wrap-mutable-formals (lambda->formals exp)
                                               (wrap-mutables (lambda->exp exp)))))
    ((set!? exp)     `(set-cell! ,(set!->var exp) ,(wrap-mutables (set!->exp exp))))
    ((if? exp)       `(if ,(wrap-mutables (if->condition exp))
                          ,(wrap-mutables (if->then exp))
                          ,(wrap-mutables (if->else exp))))

    ((app? exp)      (map wrap-mutables exp))
    (else            (error "unknown expression type: " exp))))

(define (mangle symbol)
  (define (m chars)
    (if (null? chars)
        '()
        (if (or (and (char-alphabetic? (car chars)) (not (char=? (car chars) #\_)))
                (char-numeric? (car chars)))
            (cons (car chars) (m (cdr chars)))
            (cons #\_ (append (integer->char-list (char->natural (car chars)))
                              (m (cdr chars)))))))
  (list->string (m (string->list (symbol->string symbol)))))

(define num-environments 0)

(define environments '())

(define (allocate-environment fields)
  (let ((id num-environments))
    (set! num-environments (+ 1 num-environments))
    (set! environments (cons (cons id fields) environments))
    id))

(define (get-environment id)
  (cdr (assv id environments)))


(define (closure-convert exp)
  (cond
    ((const? exp)        exp)
    ((prim? exp)         exp)
    ((ref? exp)          exp)
    ((lambda? exp)       (let* (($env (gensym 'env))
                                (body  (closure-convert (lambda->exp exp)))
                                (fv    (difference (free-vars body) (lambda->formals exp)))
                                (id    (allocate-environment fv))
                                (sub  (map (lambda (v)
                                             (list v `(env-get ,id ,v ,$env)))
                                           fv)))
                           `(closure (lambda (,$env ,@(lambda->formals exp))
                                       ,(substitute sub body))
                                     (env-make ,id ,@(azip fv fv)))))
    ((if? exp)           `(if ,(closure-convert (if->condition exp))
                              ,(closure-convert (if->then exp))
                              ,(closure-convert (if->else exp))))
    ((set!? exp)         `(set! ,(set!->var exp)
                                ,(closure-convert (set!->exp exp))))

    ((cell? exp)         `(cell ,(closure-convert (cell->value exp))))
    ((cell-get? exp)     `(cell-get ,(closure-convert (cell-get->cell exp))))
    ((set-cell!? exp)    `(set-cell! ,(closure-convert (set-cell!->cell exp))
                                     ,(closure-convert (set-cell!->value exp))))

    ((app? exp)          (map closure-convert exp))
    (else                (error "unhandled exp: " exp))))

(define (c-compile-program exp)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n"))))
         (body (c-compile-exp exp append-preamble)))
    (string-append
     "int main (int argc, char* argv[]) {\n"
     preamble
     "  __sum         = MakePrimitive(__prim_sum) ;\n"
     "  __product     = MakePrimitive(__prim_product) ;\n"
     "  __difference  = MakePrimitive(__prim_difference) ;\n"
     "  __display     = MakePrimitive(__prim_display) ;\n"
     "  __numEqual    = MakePrimitive(__prim_numEqual) ;\n"
     "  " body " ;\n"
     "  return 0;\n"
     " }\n")))

(define (c-compile-exp exp append-preamble)
  (cond
    ((const? exp)       (c-compile-const exp))
    ((prim?  exp)       (c-compile-prim exp))
    ((ref?   exp)       (c-compile-ref exp))
    ((if? exp)          (c-compile-if exp append-preamble))

    ((cell? exp)        (c-compile-cell exp append-preamble))
    ((cell-get? exp)    (c-compile-cell-get exp append-preamble))
    ((set-cell!? exp)   (c-compile-set-cell! exp append-preamble))

    ((closure? exp)     (c-compile-closure exp append-preamble))
    ((env-make? exp)    (c-compile-env-make exp append-preamble))
    ((env-get? exp)     (c-compile-env-get exp append-preamble))
    ((app? exp)         (c-compile-app exp append-preamble))
    (else               (error "unknown exp in c-compile-exp: " exp))))

(define (c-compile-const exp)
  (cond
    ((integer? exp) (string-append
                     "MakeInt(" (number->string exp) ")"))
    ((boolean? exp) (string-append
                     "MakeBoolean(" (if exp "1" "0") ")"))
    (else           (error "unknown constant: " exp))))

(define (c-compile-prim p)
  (cond
    ((eq? '+ p)       "__sum")
    ((eq? '- p)       "__difference")
    ((eq? '* p)       "__product")
    ((eq? '= p)       "__numEqual")
    ((eq? 'display p) "__display")
    (else             (error "unhandled primitive: " p))))

(define (c-compile-ref exp)
  (mangle exp))

(define (c-compile-args args append-preamble)
  (if (not (pair? args))
      ""
      (string-append
       (c-compile-exp (car args) append-preamble)
       (if (pair? (cdr args))
           (string-append ", " (c-compile-args (cdr args) append-preamble))
           ""))))

(define (c-compile-app exp append-preamble)
  (let (($tmp (mangle (gensym 'tmp))))
    (append-preamble (string-append
                      "Value " $tmp " ; "))
    (let* ((args     (app->args exp))
           (fun      (app->fun exp)))
      (string-append
       "("  $tmp " = " (c-compile-exp fun append-preamble)
       ","
       $tmp ".clo.lam("
       "MakeEnv(" $tmp ".clo.env)"
       (if (null? args) "" ",")
       (c-compile-args args append-preamble) "))"))))

(define (c-compile-if exp append-preamble)
  (string-append
   "(" (c-compile-exp (if->condition exp) append-preamble) ").b.value ? "
   "(" (c-compile-exp (if->then exp) append-preamble)      ") : "
   "(" (c-compile-exp (if->else exp) append-preamble)      ")"))

(define (c-compile-set-cell! exp append-preamble)
  (string-append
   "(*"
   "(" (c-compile-exp (set-cell!->cell exp) append-preamble) ".cell.addr)" " = "
   (c-compile-exp (set-cell!->value exp) append-preamble)
   ")"))

(define (c-compile-cell-get exp append-preamble)
  (string-append
   "(*("
   (c-compile-exp (cell-get->cell exp) append-preamble)
   ".cell.addr"
   "))"))

(define (c-compile-cell exp append-preamble)
  (string-append
   "NewCell(" (c-compile-exp (cell->value exp) append-preamble) ")"))

(define (c-compile-env-make exp append-preamble)
  (string-append
   "MakeEnv(__alloc_env" (number->string (env-make->id exp))
   "("
   (c-compile-args (env-make->values exp) append-preamble)
   "))"))

(define (c-compile-env-get exp append-preamble)
  (string-append
   "((struct __env_"
   (number->string (env-get->id exp)) "*)"
   (c-compile-exp (env-get->env exp) append-preamble) ".env.env)->"
   (mangle (env-get->field exp))))

(define num-lambdas 0)

(define lambdas '())

(define (allocate-lambda lam)
  (let ((id num-lambdas))
    (set! num-lambdas (+ 1 num-lambdas))
    (set! lambdas (cons (list id lam) lambdas))
    id))

(define (get-lambda id)
  (cdr (assv id lambdas)))

(define (c-compile-closure exp append-preamble)
  (let* ((lam (closure->lam exp))
         (env (closure->env exp))
         (lid (allocate-lambda (c-compile-lambda lam))))
    (string-append
     "MakeClosure("
     "__lambda_" (number->string lid)
     ","
     (c-compile-exp env append-preamble)
     ")")))

(define (c-compile-formals formals)
  (if (not (pair? formals))
      ""
      (string-append
       "Value "
       (mangle (car formals))
       (if (pair? (cdr formals))
           (string-append ", " (c-compile-formals (cdr formals)))
           ""))))

(define (c-compile-lambda exp)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n")))))
    (let ((formals (c-compile-formals (lambda->formals exp)))
          (body    (c-compile-exp     (lambda->exp exp) append-preamble)))
      (lambda (name)
        (string-append "Value " name "(" formals ") {\n"
                       preamble
                       "  return " body " ;\n"
                       "}\n")))))

(define (c-compile-env-struct env)
  (let* ((id     (car env))
         (fields (cdr env))
         (sid    (number->string id))
         (tyname (string-append "struct __env_" sid)))
    (string-append
     "struct __env_" (number->string id) " {\n"
     (apply string-append (map (lambda (f)
                                 (string-append
                                  " Value "
                                  (mangle f)
                                  " ; \n"))
                               fields))
     "} ;\n\n"
     tyname "*" " __alloc_env" sid
     "(" (c-compile-formals fields) ")" "{\n"
     "  " tyname "*" " t = malloc(sizeof(" tyname "))" ";\n"
     (apply string-append
            (map (lambda (f)
                   (string-append "  t->" (mangle f) " = " (mangle f) ";\n"))
                 fields))
     "  return t;\n"
     "}\n\n"
     )))

(define (emit line)
  (display line)
  (newline))

(define (c-compile-and-emit emit input-program)

  (define compiled-program "")

  (set! input-program (desugar input-program))

  (analyze-mutable-variables input-program)

  (set! input-program (desugar (wrap-mutables input-program)))

  (set! input-program (closure-convert input-program))



  (emit "#include <stdlib.h>")
  (emit "#include <stdio.h>")
  (emit "#include \"scheme.h\"")

  (emit "")

  (emit "
Value __sum ;
Value __difference ;
Value __product ;
Value __display ;
Value __numEqual ;
")

  (for-each
   (lambda (env)
     (emit (c-compile-env-struct env)))
   environments)

  (set! compiled-program  (c-compile-program input-program))

  (emit
   "Value __prim_sum(Value e, Value a, Value b) {
  return MakeInt(a.z.value + b.z.value) ;
}")

  (emit
   "Value __prim_product(Value e, Value a, Value b) {
  return MakeInt(a.z.value * b.z.value) ;
}")

  (emit
   "Value __prim_difference(Value e, Value a, Value b) {
  return MakeInt(a.z.value - b.z.value) ;
}")

  (emit
   "Value __prim_display(Value e, Value v) {
  printf(\"%i\\n\",v.z.value) ;
  return v ;
}")

  (emit
   "Value __prim_numEqual(Value e, Value a, Value b) {
  return MakeBoolean(a.z.value == b.z.value) ;
}")


  (for-each
   (lambda (l)
     (emit (string-append "Value __lambda_" (number->string (car l)) "() ;")))
   lambdas)

  (emit "")

  (for-each
   (lambda (l)
     (emit ((cadr l) (string-append "__lambda_" (number->string (car l))))))
   lambdas)

  (emit compiled-program))


(define the-program 3)

(c-compile-and-emit emit the-program)
