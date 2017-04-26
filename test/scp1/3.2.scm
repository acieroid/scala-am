(define (rec-multiply a b)
  (if (zero? b)
      0
      (+ a (rec-multiply a (- b 1)))))

(define (iter-multiply a b)
  (define (iter result counter)
    (if (zero? counter)
        result
        (iter (+ result a) (- counter 1))))
  (iter 0 b))

(= 10
   (rec-multiply 5 2)
   (iter-multiply 5 2))