;; Hulpprocedures
;; --------------
;; Getest in DrRacket 7.2 met R5RS in het Language menu geselecteerd
(#%require (only racket/base error current-print void?))

(current-print (lambda (v)
                 (if (not (void? v))
                     (display v))))

(#%require (rename r5rs apply-in-underlying-scheme apply))
(#%require (rename r5rs eval-in-underlying-scheme eval))

(define true #t)
(define false #f)

(define the-empty-stream '())

(define (stream-null? stream)
  (null? stream))

(define-syntax cons-stream 
  (syntax-rules () ((cons-stream head tail)
                    (cons head (delay tail)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (list->stream list)
  (if (pair? list)
      (cons-stream (car list) (list->stream (cdr list)))
      the-empty-stream))


(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

; Implementatie evaluator
; -----------------------

;;
;; zie deel 4 p44
;;
(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                           (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed ;ongebruikelijk
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

;;
;; zie deel 4 p19
;;
(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define NIL (singleton-stream '()))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q NIL)))
           (query-driver-loop)))))

;;
;; zie deel 4 p15
;;
(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

;;
;; zie deel 4 p17
;;
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?" 
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

;;
;; zie deel 4 p20/p86
;;
(define THE-ASSERTIONS the-empty-stream)

(define (get-all-assertions) THE-ASSERTIONS)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (rule? statement)
  (tagged-list? statement 'rule))

;;
;; zie deel 4 p62
;;
(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

;;
;; zie deel 4 p24/p63
;;
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;;
;; zie deel 4 p25
;;
(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

;;
;; zie deel 4 p34
;;
(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

;;
;; zie deel 4 p35
;;
(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

;;
;; zie deel 4 p22
;;
(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

;;
;; zie deel 4 p21
;;
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

;;
;; zie deel 4 p55
;;
(define PROPERTY-LIST '())

(define (put symb qual obj)
  (define entry (assoc symb PROPERTY-LIST))
  (if entry
      (let ((qual-obj (assoc qual (cdr entry))))
        (if qual-obj
            (set-cdr! qual-obj obj)
            (set-cdr! entry 
                      (cons (cons qual obj) (cdr entry)))))
      (set! PROPERTY-LIST 
            (cons (cons symb (list (cons qual obj))) 
                  PROPERTY-LIST)))
  PROPERTY-LIST)

(define (get symb qual)
  (define entry (assoc symb PROPERTY-LIST))
  (if entry
      (let ((qual-obj (assoc qual (cdr entry))))
        (if qual-obj
            (cdr qual-obj)
            false))
      false))

;;
;; zie deel 4 p39
;;
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(define (empty-conjunction? exps) (null? exps))

(define (first-conjunct exps) (car exps))

(define (rest-conjuncts exps) (cdr exps))

;;
;; zie deel 4 p43
;;
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(define (empty-disjunction? exps) (null? exps))

(define (first-disjunct exps) (car exps))

(define (rest-disjuncts exps) (cdr exps))

;;
;; zie deel 4 p45
;;
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (negated-query exps) (car exps))

;;
;; zie deel 4 p53
;;
(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (execute exp)
  (apply-in-underlying-scheme (eval-in-underlying-scheme (predicate exp) (scheme-report-environment 5)) 
         (args exp)))

(define (predicate exps) (car exps))

(define (args exps) (cdr exps))

;;
;; zie deel 4 p64
;;
(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

;;
;; zie deel 4 p66
;;
(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

;;
;; zie deel 4 p67
;;
(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

;;
;; zie deel 4 p70
;;
(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ;
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

;;
;; zie deel 4 p72
;;
(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val) 
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame) 
           'failed)
          (else (extend var val frame)))))

;;
;; zie deel 4 p73
;;
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

;;
;; zie deel 4 p87
;;
(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

;;
;; zie deel 4 p88
;;
(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

;;
;; zie deel 4 p89
;;
(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

;;
;; zie deel 4 p54
;;
(define (always-true ignore frame-stream) frame-stream)

(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a rules assertions)
    (cond ((null? r-and-a)
           (set! THE-ASSERTIONS (list->stream assertions))
           (set! THE-RULES (list->stream rules))
           'done)
          (else
           (let ((s (query-syntax-process (car r-and-a))))
             (cond
               ((rule? s)
                (store-rule-in-index s)
                (deal-out (cdr r-and-a) (cons s rules) assertions))
               (else
                (store-assertion-in-index s)
                (deal-out (cdr r-and-a) rules (cons s assertions))))))))
(set! PROPERTY-LIST '())
(put 'and 'qeval conjoin)
(put 'or 'qeval disjoin)
(put 'not 'qeval negate)
(put 'lisp-value 'qeval lisp-value)
(put 'always-true 'qeval always-true)
(deal-out rules-and-assertions '() '()))

;;
;; toegevoegd: database laden uit file
;;

(define (initialize-data-base-from-file filename)
  (call-with-input-file filename
    (lambda (file)
      (initialize-data-base (read file)))))

(initialize-data-base-from-file "icp_4_qeval_zonnestelsel.txt")
(query-driver-loop)


;  
; (assert! (rule (append () ?l ?l)))
; 
; (assert! (rule (append (?first . ?rest) ?l (?first . ?x))
;                (append ?rest ?l ?x)))
; 
; (append (a b) (c d) ?z)
; 
; (append (a b) ?y (a b c d))
; 
; (append ?x ?y (a b c d))



; 
; (assert! (rule (element ?first (?first . ?rest))))
; 
; (assert! (rule (element ?e (?first . ?rest))
;                (element ?e ?rest)))
; 
; (element ?x (1 2 3 4))
; 
; (and (element ?x (1 2 3 4))
;      (lisp-value odd? ?x))
; 


; 
; (assert!  (rule (+ () ?x ?x))  )
; 
; (assert!  (rule (+ (D . ?x) ?y (D . ?z))
;                 (+ ?x ?y ?z))  )
; 
; (+ () (D D) ?x)
; (+ (D D D) (D D) ?x)
; (+ (D D D) ?x (D D D D D D D D))
; (+ ?x ?y (D D D D))
; (+ ?x ?x (D D D D))
; 
; (assert!  (rule (- ?x ?y ?z)
;                 (+ ?y ?z ?x))  )
; (- (D D D D D) (D D D) ?r)
; 
; (assert!  (rule (= () ()))  )
; (assert!  (rule (= (D . ?x) (D . ?y))
;                 (= ?x ?y))  )
; (= (D D D) (D D D))
; 
; (assert!  (rule (/2 () ()))  )
; (assert!  (rule (/2 (D) ()))  )
; (assert!  (rule (/2 (D D . ?x) (D . ?y))
;                 (/2 ?x ?y))  )
; 
; (/2 (D D D D) ?x)
; (/2 (D D D D D) ?x)
; (/2 (D D D D D D) ?x)
; 
; (assert!  (rule (*2 ?x ?y)
;                 (/2 ?y ?x))  )
; 
; (*2 (D D D D D) ?x)
; 
; (*2 ?x (D D D D D))
; (*2 ?x (D D D D))



; 
; (is-planeet ?p)
; 
; (is-planeet Aarde)
; 
; (is-maan-van Maan Aarde)
; 
; (is-maan-van ?maan Aarde)
; 
; (is-maan-van Maan ?planeet)
; 
; (is-maan-van ?maan ?planeet)
; 
; (and (is-maan-van ?maan ?planeet)
;      (is-ontdekt ?maan ?jaar Herschel))
; 
; (and (is-planeet ?planeet)
;      (heeft-massa ?planeet ?massa)
;      (lisp-value > ?massa 5))
; 
; (or (is-planeet ?lichaam)
;     (is-maan-van ?lichaam ?planeet))
; 
; (or (is-maan-van ?lichaam ?planeet)
;     (is-planeet ?lichaam))
; 
; (and (is-planeet ?planeet)
;      (not (is-maan-van ?maan ?planeet)))
; 
; (and (not (is-maan-van ?maan ?planeet))
;      (is-planeet ?planeet))
; 
; (assert! (rule (is-lichaam ?x)
;            (or (is-maan-van ?x ?p)
;                (is-planeet ?x))))
; 
; (is-lichaam ?lichaam)
; 
; (assert! (rule (ml= ?planeet ?middellijn ?e)
;    (and (is-planeet ?planeet)
; 	(heeft-middellijn ?planeet ?middellijn)
; 	(gebruikt-eenheid heeft-middellijn ?e))))
; 
; (assert! (rule (ontdekt-in-de-17e-eeuw ?maan ?persoon)
;            (and (is-ontdekt ?maan ?jaar ?persoon) 
;                 (lisp-value > ?jaar 1599)
;                 (lisp-value < ?jaar 1700))))
; 
; (assert! (rule (same ?v ?v)))
; 
; (assert! (rule (zelfde-planeet ?x ?y)
;            (and (is-maan-van ?x ?p)
;                 (is-maan-van ?y ?p)
;                 (not (same ?x ?y)))))
; 
; (same 3 ?x)
; 
; (zelfde-planeet ?m ?n)
