(define (f x) (if (< x 100) f 1))
(define f2 (f 5))
(eq? (f2 2) f)
