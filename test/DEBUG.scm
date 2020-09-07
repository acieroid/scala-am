(define (fib n)
  (if (< n 2)
    n
    (let ((fib-n-1 (fib (- n 1)))
           (fib-n-2 (fib (- n 2))))
      (+ fib-n-1 fib-n-2))))
(procedure? fib)