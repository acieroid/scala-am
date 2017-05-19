(define NumValues (int-top))
(define MaxValue (int-top))
(define Seed (int-top))

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
  (a/actor "sort" (radix next-actor array values-so-far j)
           (value (v)
                  (let ((newj (if (= (logand v radix) 0)
                                  (begin (a/send next-actor value v) j)
                                  (begin (vector-set! array j v) (+ 1 j)))))
                    (if (= (+ values-so-far 1) NumValues)
                        (begin
                          (letrec ((loop (lambda (i)
                                           (if (= i j)
                                               #t
                                               (begin
                                                 (a/send next-actor value (vector-ref array i))
                                                 (loop (+ i 1)))))))
                            (loop 0)
                            (a/terminate)))
                        (a/become sort radix next-actor array (+ values-so-far 1) newj))))))
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
      (let ((sort-actor (a/create sort radix next-actor (make-vector NumValues 0) 0 0)))
        (main-loop (inexact->exact (/ radix 2)) sort-actor))
      next-actor))
(a/send source-actor next-actor (main-loop (int-top) validation-actor))
