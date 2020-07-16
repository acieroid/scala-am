(define (f x)
  (fork (f x)))

(f 42)