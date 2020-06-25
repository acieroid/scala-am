(letrec ((fact (lambda (n)
                 (if (= n 0)
                   1
                   (* n (fact (- n 1))))))
          (t1 (fact 5))
          (t2  (fact 4)))
  (= (+ t1 t2) 144))
