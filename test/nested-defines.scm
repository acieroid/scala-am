(define (f x)
  (define (g y)
    (+ x y))
  (g 5))

(= (f 0) 5)
