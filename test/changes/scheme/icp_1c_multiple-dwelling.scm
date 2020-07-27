;;
;;toegevoegd
;;
(define true #t)
(define false #f)

;;
;; zie deel 1a p. 37
;;
(define (apply-in-underlying-scheme proc args)
  (cond
    ((null? args) (proc))
    ((null? (cdr args)) (proc (car args)))
    ((null? (cddr args)) (proc (car args) (cadr args)))
    ((null? (cdddr args)) (proc (car args) (cadr args) (caddr args)))
    ((null? (cddddr args)) (proc (car args) (cadr args) (caddr args) (cadddr args)))
    ((null? (cdr (cddddr args))) (proc (car args) (cadr args) (caddr args) (cadddr args) (car (cddddr args))))
    (else (error "Unsupported call."))))

;;
;; zie deel 1a p. 13
;;
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;;
;; zie deel 1a p. 15
;;
(define (self-evaluating? exp)
  (cond ((number? exp) true)
    ((string? exp) true)
    (else false)))

;;
;; zie deel 1a p. 16
;;
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

;;
;; zie deel 1a p. 17
;;
(define (variable? exp) (symbol? exp))

;;
;; zie deel 1a p. 18
;;
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;
;; zie deel 1a p. 19/20
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
;; zie deel 1a p. 21
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
;; zie deel 1a p. 22
;;
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;
;; zie deel 1a p. 23/24
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
          (error "ELSE clause isn't last -- COND->IF"))
        (make-if (cond-predicate first)
          (sequence->exp (cond-actions first))
          (expand-clauses rest))))))

;;
;; zie deel 1a p. 25
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
;; zie deel 1a p. 26
;;
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;
;; zie deel 1a p. 27
;;
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;;
;; zie deel 1a p. 29
;;
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;;
;; zie deel 1a p. 30
;;
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied")
      (error "Too few arguments supplied"))))

;;
;; zie deel 1a p. 31
;;
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;;
;; zie deel 1a p. 32
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
      (error "Unbound variable")
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
          (frame-values frame)))))
  (env-loop env))

;;
;; zie deel 1a p. 33
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
      (error "Unbound variable -- SET!")
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
          (frame-values frame)))))
  (env-loop env))

;;
;; zie deel 1a p. 34
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
;; zie deel 1a p. 35
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
;; zie deel 1a p. 36
;;
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;;
;; zie deel 1c p. 24
;;
(define primitive-procedures
  (list (list 'car car)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'null? null?)
    (list 'list list)
    (list 'memq memq)
    (list 'member member)
    (list 'not not)
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '= =)
    (list '> >)
    (list '< <)
    (list '>= >=)
    (list 'abs abs)
    (list 'remainder remainder)
    (list 'integer? integer?)
    (list 'sqrt sqrt)
    (list 'eq? eq?)
    ;;      more primitives
  ))

(define (primitive-procedure-names)
  (map car
    primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
    primitive-procedures))

;;
;; zie deel 1a p. 37
;;
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

;;
;; zie deel 1c p. 22
;;
(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

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

;;
;; zie deel 1c p. XX
;;
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;;
;; zie deel 1c p. 12
;;
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
          (analyze-self-evaluating exp))
    ((quoted? exp) (analyze-quoted exp))
    ((variable? exp) (analyze-variable exp))
    ((assignment? exp) (analyze-assignment exp))
    ((definition? exp) (analyze-definition exp))
    ((if? exp) (analyze-if exp))
    ((lambda? exp) (analyze-lambda exp))
    ((begin? exp) (analyze-sequence (begin-actions exp)))
    ((cond? exp) (analyze (cond->if exp)))
    ((let? exp) (analyze (let->combination exp)))
    ((amb? exp) (analyze-amb exp))
    ((application? exp) (analyze-application exp))
    (else
      (error "Unknown expression type -- ANALYZE"))))

;;
;; zie deel 1c p. 13
;;
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
      fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
         (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
        fail))))

;;
;; zie deel 1c p. 14
;;
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
         (cproc (analyze (if-consequent exp)))
         (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
        (lambda (pred-value fail2)
          (if (true? pred-value)
            (cproc env succeed fail2)
            (aproc env succeed fail2)))
        fail))))

;;
;; zie deel 1c p. 15
;;
(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
        (lambda (a-value fail2)
          (b env succeed fail2))
        fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
        (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;;
;; zie deel 1c p. 16
;;
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
         (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
        (lambda (val fail2)
          (define-variable! var val env)
          (succeed 'ok fail2))
        fail))))

;;
;; zie deel 1c p. 17
;;
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
         (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
        (lambda (val fail2)
          (let ((old-value
                  (lookup-variable-value var env)))
            (set-variable-value! var val env)
            (succeed 'ok
              (lambda ()
                (set-variable-value! var
                  old-value
                  env)
                (fail2)))))
        fail))))

;;
;; zie deel 1c p. 18
;;
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
         (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
        (lambda (proc fail2)
          (get-args aprocs
            env
            (lambda (args fail3)
              (execute-application
                proc args succeed fail3))
            fail2))
        fail))))

;;
;; zie deel 1c p. 19
;;
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs) env
      (lambda (arg fail2)
        (get-args (cdr aprocs)
          env
          (lambda (args fail3)
            (succeed (cons arg args)
              fail3))
          fail2))
      fail)))

;;
;; zie deel 1c p. 20
;;
(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
          (succeed (apply-primitive-procedure proc args)
            fail))
    ((compound-procedure? proc)
      ((procedure-body proc)
        (extend-environment (procedure-parameters proc)
          args
          (procedure-environment proc))
        succeed
        fail))
    (else
      (error "Unknown procedure type -- EXECUTE-APPLICATION"))))

;;
;; zie deel 1c p. 21
;;
(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices) env
            succeed
            (lambda ()
              (try-next (cdr choices))))))
      (try-next cprocs))))

;;
;; zie deel 1c p. 23
;;
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-var binding) (car binding))
(define (let-val binding) (cadr binding))

(define (make-combination operator operands)
  (cons operator operands))

(define (let->combination exp)
  (let ((bindings (let-bindings exp)))
    (make-combination (make-lambda (map let-var bindings)
                        (let-body exp))
      (map let-val bindings))))

(define the-global-environment (setup-environment))
(define input '(begin
                 (define (require p)
                   (if (not p) (amb)))
                 (define (distinct? items)
                   (cond ((null? items) true)
                     ((null? (cdr items)) true)
                     ((member (car items) (cdr items)) false)
                     (else (distinct? (cdr items)))))

                 (define (multiple-dwelling)
                   (let ((baker (amb 1 2 3 4 5))
                          (cooper (amb 1 2 3 4 5))
                          (fletcher (amb 1 2 3 4 5))
                          (miller (amb 1 2 3 4 5))
                          (smith (amb 1 2 3 4 5)))
                     (require (distinct?
                                (list baker cooper fletcher miller smith)))
                     (require (not (= baker (<change> 5 4)))) ; <=======================================================
                     (require (not (= cooper 1)))
                     (require (not (= fletcher 5)))
                     (require (not (= fletcher 1)))
                     (require (> miller cooper))
                     (require (not (= (abs (- smith fletcher)) (<change> 1 2)))) ; <====================================
                     (require (not (= (abs (- fletcher cooper)) 1)))
                     (<change> #t (require (< (abs (- miller smith)) 3))) ; <===========================================
                     (list (list 'baker baker)
                       (list 'cooper cooper)
                       (list 'fletcher fletcher)
                       (list 'miller miller)
                       (list 'smith smith))))

                 (multiple-dwelling)))

(define next-alternative (lambda () #f))
(define (try)
  (ambeval input
    the-global-environment
    (lambda (val next-alt)
      (announce-output output-prompt)
      (user-print val)
      (set! next-alternative next-alt))
    (lambda ()
      (announce-output ";;; There are no more values of")
      (user-print input)
      (set! next-alternative (lambda () #f)))))

(try)
(and (next-alternative)
     (next-alternative)
     (next-alternative)
     (next-alternative)
     (next-alternative))