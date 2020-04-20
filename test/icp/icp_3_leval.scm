;;
;;toegevoegd
;;
(define true #t)
(define false #f)

;;
;; zie deel 3 p6
;;
(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

;;
;; zie deel 3 p7
;;

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define nonmemo-force-it force-it) ;;toegevoegd voor demonstratie onderaan

;;
;; ;; zie deel 3 p16
;;
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj) ;; herdefinitie van bovenstaande voor memoization
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk) ; change tag of list
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define memo-force-it force-it) ;;toegevoegd voor timing demonstratie onderaan

;;
;; zie deel 1.1 p52
;;
(define (apply-in-underlying-scheme proc args)
  (cond
   ((null? args) (proc))
   ((null? (cdr args)) (proc (car args)))
   ((null? (cddr args)) (proc (car args) (cadr args)))
   (else (error "Unsupported call"))))

;;
;; zie deel 3 p9 en p24 
;;
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ;; origineel uit boek
        ;; ((quoted? exp) (text-of-quotation exp))
        ;; aangepast voor demonstratie onderaan
        ((or (quoted? exp)
             (quasiquoted? exp))
         (eval-quotation-args (text-of-quotation exp) env)) 
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) ;;aangepast
         (apply (actual-value (operator exp) env) 
                (operands exp)
                env)) 
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;
;; zie deel 3 p7
;;
(define (actual-value exp env)
  (force-it (eval exp env)))

;;
;; zie deel 3 p10
;;

(define (apply procedure arguments env) ;; aangepast
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ;; aangepast
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ;; aangepast
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;;
;; zie deel 3 p11
;;
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

;;
;; zie deel 1.1 p21
;;
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;;
;; zie deel 1.1 p22
;;
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;
;; zie deel 1.1 p24
;;
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;;
;; zie deel 3 p12
;;
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env)) ;;aangepast
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;
;; zie deel 1.1 p36
;;
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;
;; zie deel 1.1 p18
;;
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;;
;; zie deel 1.1 p19
;;
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

;; toegevoegd voor demonstratie onderaan
(define (quasiquoted? exp)
  (tagged-list? exp 'quasiquote))

(define (unquote? exp)
  (tagged-list? exp 'unquote))

;;
;; zie deel 1.1 p18
;;
(define (variable? exp) (symbol? exp))

;;
;; zie deel 1.1 p21
;;
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;
;; zie deel 1.1 p22
;;
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;;
;; zie deel 1.1 p24
;;
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;
;; zie deel 1.1 p28
;;
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;
;; zie deel 1.1 p38/40
;;
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;
;; zie deel 1.1 p42
;;
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;;
;; zie deel 1.1 p32
;;
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;
;; zie deel 1.1 p29
;;
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;;
;; zie deel 1.1 p44
;;
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;;
;; zie deel 1.1 p45
;;
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;;
;; zie deel 1.1 p44
;;
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;;
;; zie deel 1.1 p46
;;
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;
;; zie deel 1.1 p48
;;
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;
;; zie deel 1.1 p49
;;
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;
;; zie deel 1.1 p50
;;
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;;
;; zie deel 1.1 p51
;;
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list
        ;zie onderaan voor een definitie van cons/car/cdr als compound procedures ipv primitives
        ;(list 'car car)
        ;(list 'cdr cdr)
        ;(list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '<= <=)
        (list '>= >=)
        (list '< <)
        (list '> >)
        (list 'newline newline)
        (list 'display display)
;;      more primitives
        ))

;;
;; zie deel 1.1 p52
;;
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;;
;; zie deel 3 p8
;;
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

;;(define (driver-loop)
;;  (prompt-for-input input-prompt)
;;  (let ((input (read)))
;;    (let ((output
;;           (actual-value input the-global-environment))) ;; aangepast
;;      (announce-output output-prompt)
;;      (user-print output)))
;;  (driver-loop))

;;
;; zie deel 1a p. 38
;;
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

;;
;; Oplossing voor oefening 4.33
;; zodat expressies '(1 2 (+ 4 5) 3) resulteren in een lazy list.
;; Ondersteunt eveneens quasiquoted expressies
;; `(1 2 ,(+ 4 5) 3)
;; zoals uitgelegd onderaan pagina 575 van het boek.

(define (make-list xs)
  (if (null? xs)
      '()
      (let ((x (car xs)))
        (list 'cons  
              (if (unquote? x)
                  (cadr x)
                  (list 'quote x))
              (make-list (cdr xs))))))

;zelfde effect als origineel uit boek, zal lijst teruggeven zoals gekregen van Racket reader
(define (unaware-eval-quotation-args qargs env)
  qargs)

;zal lijst aanmaken met cons van l-eval ipv Racket
(define (aware-eval-quotation-args qargs env)
  (if (pair? qargs)
      (eval (make-list qargs) env)
      qargs))

(define eval-quotation-args unaware-eval-quotation-args)


(display "Loading definitions of cons, car, cdr as compound procedures. ")
(eval '(begin
         ;zie pagina 92
         (define (cons x y)
           ;;oproepen van cons resulteren in een nieuwe procedure 
           ;;die de car x en de cdr y van de oproep onthoudt via zijn definitie-omgeving
           (lambda (m)
             ;;de teruggeven procedure neemt een andere procedure als argument,
             ;;en past dit argument toe op x en y
             (m x y)))
         
         (define (car z)
           ;;toepassing van de procedure die een cons voorstelt
           (z (lambda (p q) p)))
         
         (define (cdr z)
            ;;toepassing van de procedure die een cons voorstelt
           (z (lambda (p q) q)))
         
         (define (list-ref items n)
           (if (= n 0)
               (car items)
               (list-ref (cdr items) (- n 1))))
         
         (define (map proc items)
           (if (null? items)
               '()
               (cons (proc (car items)) (map proc (cdr items))))))
      the-global-environment)

(eval '(begin
  (define (try a b)
    (if (= a 0)
        1
        b))
  (try 0 (/ 1 0))) the-global-environment)

(eval '(begin
  (define (unless condition usual-value exceptional-value)
    (if condition
        exceptional-value
        usual-value))
  
  (define (factorial n)
    (unless (= n 1) (* (factorial (- n 1)) n) 1))
  
  (factorial 5)) the-global-environment)

(eval '(begin 
  (define (scale-list factor items)
    (map (lambda (x) (* x factor))
         items))
  
  (define l (scale-list 5 (cons 0 (cons 1 (cons 2 (cons 3 '()))))))
  
  (list-ref l 2)
  
  (define (powers x)
    (cons 1 (scale-list x (powers x))))

  (list-ref (powers 2) 0)
  (list-ref (powers 2) 3)
  (list-ref (powers 2) 8)) the-global-environment)

(eval '(begin
  (define (add-lists list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else (cons (+ (car list1) (car list2))
                      (add-lists (cdr list1)
                                 (cdr list2))))))
  
  (define ones (cons 1 ones))
  
  (define integers (cons 1 (add-lists ones integers)))
  
  (list-ref integers 17)) the-global-environment)

(eval '(begin
  (define count 0)
  
  (define (id x)
    (set! count (+ count 1))
    x)

  (define w (id (id 10)))

  ;;voor de definitie van w is alleen de buitenste toepassing van id nodig 
  (display "count before display of w: ") (display count) (newline)
  ;;de evaluator resulteert een thunk als opgezochte waarde van w 
  (display "w: ") (display w) (newline)
  ;;het printen van die thunk via display geeft aanleiding tot de binnenste toepassing van id
  (display "count after display of w: ") (display count) (newline)
  'ok) the-global-environment)

(define (square x) (* x x))
(square (square (+ 2 3)))

