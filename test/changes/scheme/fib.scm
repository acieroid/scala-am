(letrec ((fib (lambda (n)
                (if (< n 2)
                  n
                  (+ (fib (- n (<change> 1 2))) (fib (- n (<change> 2 1))))))))
  (fib 4))
