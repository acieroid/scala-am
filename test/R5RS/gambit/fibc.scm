;;; FIBC -- FIB using first-class continuations, written by Kent Dybvig

(define (_1+ n) (+ n 1))
(define (_1- n) (- n 1))

;;; fib with peano arithmetic (using numbers) with call/cc

(define (addc x y k)
  (if (zero? y)
    (k x)
    (addc (_1+ x) (_1- y) k)))

(define (fibc x c)
  (if (zero? x)
    (c 0)
    (if (zero? (_1- x))
      (c 1)
      (addc (call-with-current-continuation (lambda (c) (fibc (_1- x) c)))
            (call-with-current-continuation (lambda (c) (fibc (_1- (_1- x)) c)))
            c))))

(equal? (fibc 18 (lambda (n) n)) 2584)
