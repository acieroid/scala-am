(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (sim-multiply a b)
  (if (zero? b)
      1
      (+ 1 (sim-multiply a (- b 1)))))
(define (sim-fast-multiply a b)
  (cond ((zero? b) 1)
        ((even? b) (+ 1 (sim-fast-multiply (double a) (halve b))))
        (else (+ 1 (sim-fast-multiply a (- b 1))))))

(and (= (sim-multiply 14 2365) 2366)
     (= (sim-fast-multiply 14 2365) 19))