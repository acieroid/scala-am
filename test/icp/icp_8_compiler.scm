(#%require (only racket/base
                 time error))

;;
;;toegevoegd
;;
(define true #t)
(define false #f)

;;
;; zie deel 1.1 p52
;;
(define apply-in-underlying-scheme apply)

;;
;; zie deel 1.1 p24
;;
(define (true? x)
  (not (eq? x false)))

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

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

;;
;; zie deel 1.1 p28
;;
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;
;; zie deel 1.1 p42
;;
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

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
;; zie deel 1.1 p52
;;
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))



;; =======================
;; begin van compiler code
;; =======================



;;
;; zie deel 8 p3
;;
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) 
         (compile-if exp target linkage))
        ((lambda? exp)
         (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

;;
;; zie deel 8 p6
;;
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;;
;; zie deel 8 p4
;;
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence
          '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence
          '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

;;
;; zie deel 8 p13
;;
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '() (list target)
                     `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence 
                     '() (list target)
                     `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '(env) (list target)
                     `((assign ,target (op lookup-variable-value)
                               (const ,exp) (reg env))))))

;;
;; zie deel 8 p15
;;
(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence 
                                   '(env val) (list target)
                                   `((perform (op set-variable-value!)
                                              (const ,var)
                                              (reg val)
                                              (reg env))
                                     (assign ,target (const ok))))))))

;;
;; zie deel 8 p17
;;
(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence 
                                   '(env val) (list target)
                                   `((perform (op define-variable!)
                                              (const ,var)
                                              (reg val)
                                              (reg env))
                                     (assign ,target (const ok))))))))

;;
;; zie deel 8 p21
;;
(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))
;;
;; zie deel 8 p19
;;
(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence
                      '(val) '()
                      `((test (op false?) (reg val))
                        (branch (label ,f-branch))))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))

;;
;; zie deel 8 p24
;;
(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next)
                  (compile-sequence (rest-exps seq) target linkage))))

;;
;; zie deel 8 p26
;;
(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
                          (make-instruction-sequence
                           '(env) (list target)
                           `((assign ,target (op make-compiled-procedure)
                                     (label ,proc-entry) (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

;;
;; zie deel 8 p28
;;
(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence 
      '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env (op extend-environment)
                (const ,formals) (reg argl) (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))

;;
;; zie deel 8 p30
;;
(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

;;
;; zie deel 8 p31
;;
(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
                                   '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))
;;
;; zie deel 8 p32
;;
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence
                      '(val argl) '(argl)
                      '((assign argl
                                (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

;;
;; zie deel 8 p34
;;
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
                           (make-instruction-sequence
                            '(proc argl)
                            (list target)
                            `((assign ,target (op apply-primitive-procedure)
                                      (reg proc) (reg argl)))))))
       after-call))))

;;
;; zie deel 8 p36
;;
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry) (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence 
          '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))

(define all-regs '(env proc val argl continue))

;;
;; zie deel 8 p7
;;
(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

;;
;; zie deel 8 p10
;;
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

;;
;; zie deel 8 p11
;;
(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

;;
;; zie deel 8 p8
;;
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

;;
;; zie deel 8 p22
;;
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))


(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc)
  (cadr c-proc))
(define (compiled-procedure-env c-proc)
  (caddr c-proc))


;; =======================
;; einde van compiler code
;; =======================

;(compile
; '(begin (define (id x)
;           x)
;         (id 2))
; 'val
; 'next)

;(compile
; '(begin (define (fac n)
;           (if (= n 0)
;               1
;               (* n (fac (- n 1)))))
;         (fac 5))
; 'val
; 'next)


