(define NumValues (int-top))
(define MaxValue (int-top))
(define (for-each f l)
  (if (null? l)
      #t
      (if (pair? l)
          (begin (f (car l)) (for-each f (cdr l)))
          (error "Cannot for-each over a non-list"))))
(define (logand x y) (int-top))       ; logand not in r5rs

(define int-source
  (a/actor "int-source" ()
           (next-actor (actor)
                       (letrec ((loop (lambda (i)
                                        (if (= i NumValues)
                                            #t
                                            (begin
                                              (a/send actor value (random MaxValue))
                                              (loop (+ i 1)))))))
                         (loop 0)
                         (a/terminate)))))

(define sort
  (a/actor "sort" (radix next-actor array values-so-far)
           (value (v)
                  (let ((check-values (lambda (array)
                                        (if (= (+ values-so-far 1) NumValues)
                                            (begin
                                              (for-each (lambda (v) (a/send next-actor value v)) array)
                                              (a/terminate))
                                            (a/become sort radix next-actor array (+ values-so-far 1))))))
                    (if (= (logand v radix) 0)
                        (begin
                          (a/send next-actor value v)
                          (check-values array))
                        (begin
                          (check-values (cons v array))))))))

(define validation
  (a/actor "validation" (sum-so-far values-so-far prev-value error-value)
           (value (v)
                  (let ((error-value (if (and (< v prev-value) (< error-value 0))
                                         v
                                         error-value)))
                    (if (= (+ values-so-far 1) NumValues)
                        (begin
                          (if (>= error-value 0)
                              (display "error!")
                              (display sum-so-far))
                          (a/terminate))
                        (a/become validation (+ sum-so-far prev-value) (+ values-so-far 1) v error-value))))))

(define validation-actor (a/create validation 0 0 0 -1))
(define source-actor (a/create int-source))
(define (main-loop radix next-actor)
  (if (> radix 0)
      (let ((sort-actor (a/create sort radix next-actor '() 0)))
        (main-loop (inexact->exact (floor (/ radix 2))) sort-actor))
      next-actor))
(a/send source-actor next-actor (main-loop (int-top) validation-actor))
