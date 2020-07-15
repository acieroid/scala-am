(letrec ((fact (lambda (n)
                 (if (<change> (= n 0) (< n 2))
                   1
                   (* n (fact (- n 1)))))))
  (fact 3))
