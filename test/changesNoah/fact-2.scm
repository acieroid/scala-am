(letrec ((fact (lambda (n)
                 (if (< n 2)
                   1
                   (* n (fact (- n 1)))))))
  (fact 3))
