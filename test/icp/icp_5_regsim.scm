;; Getest in DrRacket 7.2 met R5RS in het Language menu geselecteerd
(define false #f)
(define true #t)

;;
;; zie deel 5 p47
;;
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;;
;; zie deel 5 p51
;;
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;;
;; zie deel 5 p53
;;
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;;
;; zie deel 5 p48-50
;;
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond
          ((eq? message 'start)
           (set-contents! pc the-instruction-sequence)
           (execute))
          ((eq? message 'install-instruction-sequence)
           (lambda (seq) (set! the-instruction-sequence seq)))
          ((eq? message 'allocate-register) allocate-register)
          ((eq? message 'get-register) lookup-register)
          ((eq? message 'install-operations)
           (lambda (ops) (set! the-ops (append the-ops ops))))
          ((eq? message 'stack) stack)
          ((eq? message 'operations) the-ops)
          (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;;
;; zie deel 5 p52
;;
(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;;
;; zie deel 5 p54
;;

(define (assemble controller-text machine)
  (let ((result (extract-labels controller-text)))
    (let ((insts (car result)) (labels (cdr result)))
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text)
  (if (null? text)
      (cons '() '())
      (let ((result (extract-labels (cdr text))))
        (let ((insts (car result))
              (labels (cdr result)))
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (cons insts
                      (cons (make-label-entry next-inst insts) labels))
                (cons (cons (make-instruction next-inst) insts)
                      labels)))))))


; ;; continuatie-gebaseerd alternatief uit het boek (niet te kennen)
; (define (assemble controller-text machine)
;   (extract-labels controller-text
;     (lambda (insts labels)
;       (update-insts! insts labels machine)
;       insts)))
; 
; (define (extract-labels text receive)
;   (if (null? text)
;       (receive '() '())
;       (extract-labels (cdr text)
;        (lambda (insts labels)
;          (let ((next-inst (car text)))
;            (if (symbol? next-inst)
;                (receive insts
;                         (cons (make-label-entry next-inst
;                                                 insts)
;                               labels))
;                (receive (cons (make-instruction next-inst)
;                               insts)
;                         labels)))))))


;;
;; zie deel 5 p56
;;
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

;;
;; zie deel 5 p55
;;
(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

;;
;; zie deel 5 p57
;;
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

;;
;; zie deel 5 p58-59
;;
(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;;
;; zie deel 5 p70-71
;;
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;;
;; zie deel 5 p72-73
;;
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;;
;; zie deel 5 p68-69
;;
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;;
;; zie deel 5 p66-67
;;
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;;
;; zie deel 5 p64-65
;;
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

;;
;; zie deel 5 p61
;;
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

;;
;; zie deel 5 p60
;;
(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

;;
;; zie deel 5 p62-63
;;
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (cond
     ((null? aprocs) (op))
     ((null? (cdr aprocs)) (op (car aprocs)))
     ((null? (cddr aprocs)) (op (car aprocs) (cadr aprocs)))
     (else (error "apply")))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;
;; lijst van vaak benodigde primitieve operaties
;;

(define ops `((+ ,+)
              (* ,*)
              (- ,-)
              (= ,=)
              (< ,<)
              (> ,>)
              (display ,(lambda (x)(display x)))))

;;
;; registermachine voor faculteit
;; 
;; (define (fac n)
;;  (if (= n 1)
;;      1
;;      (* n (fac (- n 1)))))
(let ((fac-machine
       (make-machine '(continue
                       ;; getal om faculteit van te berekenen
                       n 
                       ;; waarde van faculteit
                       val) 
                     ops
                     '(;; label om naartoe te springen na afhandelen van base-case
                       start
                          (assign continue (label fact-done))
                       fact-loop
                          (test (op =) (reg n) (const 1))
                          (branch (label base-case))
                          ;; opslaan van continue en n voor "recursieve oproep"
                          ;; n is na afhandeling van de oproep nog nodig voor de vermenigvuldiging
                          ;; als resultaat van de oproep krijgt val een nieuwe waarde 
                          (save continue)
                          (save n)
                          (assign n (op -) (reg n) (const 1))
                          ;; label om naartoe te springen na de "recursieve oproep"
                          (assign continue (label after-fact))
                          (goto (label fact-loop))
                       after-fact
                          ;; terugplaatsen van met deze oproep gassocieerde continue en n
                          (restore n)
                          (restore continue)
                          (assign val (op *) (reg n) (reg val))
                          (goto (reg continue))
                       base-case
                          (assign val (const 1))
                          (goto (reg continue))
                       fact-done
                       ;; na fact-done bevat val het resultaat
                       ))))
  (display "(fac 5): " )
  (set-register-contents! fac-machine 'n 5)
  (start fac-machine)
  (display (get-register-contents fac-machine 'val))
  (newline))


;;
;; registermachine voor fibonacci
;; 
;; (define (fib n)
;;  (if (< n 2)
;;      n
;;      (+ (fib (- n 1))
;;         (fib (- n 2)))))
(let ((fib-machine
       (make-machine '(continue n val) 
                     ops
                     '(start
                           (assign continue (label fib-done))
                       fib-loop
                           (test (op <) (reg n) (const 2))
                           (branch (label immediate-answer))
                           ;; oproep van (fib (- n 1))
                           (save continue)
                           (save n) 
                           (assign continue (label afterfib-n-1))
                           (assign n (op -) (reg n) (const 1))
                           (goto (label fib-loop))
                        afterfib-n-1
                           ;; herstel oude, opgeslagen waarde van n
                           (restore n)
                           (restore continue)
                           ;; voorbereiding voor oproep (fib (- n 2))
                           (assign n (op -) (reg n) (const 2))
                           (save continue)
                           (assign continue (label afterfib-n-2))
                           ;; sla de net berekende waarde van (fib (- n 1)) op
                           (save val)
                           (goto (label fib-loop))
                        afterfib-n-2
                           ;; onthoud de net berekende waarde van (fib (- n 2)) 
                           (assign n (reg val))
                           ;; herstel de eerder berekende waarde van (fib (- n 1))
                           (restore val)
                           (restore continue)
                           ;; som van resultaten
                           (assign val (op +) (reg val) (reg n))
                           ;; ga terug naar oproeper
                           (goto (reg continue))
                        immediate-answer
                           ;; base case: (fib n) = n 
                           (assign val (reg n))
                           (goto (reg continue))
                        fib-done))))
  (display "(fib 5): " )
  (set-register-contents! fib-machine 'n 5)
  (start fib-machine)
  (display (get-register-contents fib-machine 'val))
  (newline))
