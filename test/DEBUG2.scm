(define (fib x) x)
(display (fib 10))
(newline)
(begin
  (load "DEBUG.scm") ; Contains real definition for fibonacci.
  (display (fib 10)))
(newline)
(fib 10)