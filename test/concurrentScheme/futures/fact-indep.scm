;; Expected result: #t
(letrec ((fact (lambda (n)
                 (if (= n 0)
                     1
                     (* n (fact (- n 1))))))
         (t1 (future (fact 5)))
         (t2 (future (fact 4))))
  (= (+ (deref t1) (deref t2)) 144))
