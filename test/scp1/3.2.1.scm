(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (rec-fast-multiply a b)
  (cond ((zero? b) 0)
        ((even? b) (rec-fast-multiply (double a) (halve b)))
        (else (+ a (rec-fast-multiply a (- b 1))))))

(define (iter-fast-multiply a b)
  (define (iter a b acc)
    (cond ((zero? b) acc)
          ((even? b) (iter (double a) (halve b) acc))
          (else (iter a (- b 1) (+ acc a)))))
  (iter a b 0))

(and (= (rec-fast-multiply 3 4) 12)
     (= (rec-fast-multiply 100 200) 20000)
     (= (iter-fast-multiply 3 4) 12)
     (= (iter-fast-multiply 100 200) 20000))