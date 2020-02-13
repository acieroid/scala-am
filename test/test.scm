(define (foo n)
 ((lambda (x) (+ x n)) 100))

(foo 1)
(foo 2)
(foo 3)
(define (too-much)
  (foo 4)
  (foo 5))

(too-much)
