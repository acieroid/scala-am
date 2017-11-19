(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
(define t1 (c/spawn (factorial 4)))
(define t2 (c/spawn (factorial 5)))
(+ (c/join t1) (c/join t2))
