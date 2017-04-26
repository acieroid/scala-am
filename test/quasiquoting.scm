;; Test simple quasiquotations.
;; Expected result: #t
;; Based on the R5RS specification document and POOL (http://pointcarre.vub.ac.be/index.php?application=weblcms&go=course_viewer&course=2338).

(define VAR
  (lambda (name value)
    `(define ,name ,value)))

(define CLASS
  (lambda (super . defs)
    `(letrec
         ((<<SUPER>> ,super)
          (<<METHODS>> (<<TABLE>>))
          (<<VARS>> (<<SUPER>> '<<COPY>>))
          (<<CLASS>>
           (lambda (msg . args)
             (case msg
               ((new)
                (let*
                    ((context (<<VARS>> 'instantiate))
                     (self
                      (lambda (msg . args)
                        (<<CLASS>> '<<EVAL>> context msg args))))
                  (context 'replace '<<SELF>> self)
                  self))
               ((<<EVAL>>)
                (let*
                    ((context (car args))
                     (msg (cadr args))
                     (args (caddr args))
                     (entry (<<METHODS>> 'get msg)))
                  (if entry
                      (apply entry (cons context args))
                      (<<SUPER>> '<<EVAL>> context msg args))))
               ((<<COPY>>)
                (<<VARS>> 'copy))
               (else
                (error "invalid class message " msg))))))
       ,@defs
       <<CLASS>>)))

(let ((res1 `(list ,(+ 1 2) 4)  )
      (exp1 '(list 3 4))
      (res2 (quasiquote (list (unquote (+ 1 2)) 4)))
      (exp2 '(list 3 4))
      (res3 `,(+ 2 3))
      (exp3 5)
      (res4 (VAR 'NAME 12345))
      (exp4 '(define NAME 12345))
      (res5 (CLASS 'SUPER '(begin (list 1 2 3) (list 4 5 6)) '(define x 4) '(set! x 5)))
      (exp5 '(letrec ((<<super>> super)
                      (<<methods>> (<<table>>))
                      (<<vars>> (<<super>> '<<copy>>))
                      (<<class>>
                       (lambda (msg . args)
                         (case msg
                           ((new) (let* ((context (<<vars>> 'instantiate)) (self (lambda (msg . args) (<<class>> '<<eval>> context msg args)))) (context 'replace '<<self>> self) self))
                           ((<<eval>>) (let* ((context (car args)) (msg (cadr args)) (args (caddr args)) (entry (<<methods>> 'get msg))) (if entry (apply entry (cons context args)) (<<super>> '<<eval>> context msg args))))
                           ((<<copy>>) (<<vars>> 'copy))
                           (else (error "invalid class message " msg))))))
               (begin (list 1 2 3) (list 4 5 6))
               (define x 4)
               (set! x 5)
               <<class>>)))
  (and (equal? res1 exp1)
       (equal? res2 exp2)
       (= exp3 res3)
       (equal? res4 exp4)
       (equal? res5 exp5)))