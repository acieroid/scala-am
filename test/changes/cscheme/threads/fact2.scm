(letrec ((result 1)
         (fact (lambda (i)
                 (if (= i 0)
                     result
                     (begin
                       (<change> #t (define j (- i 1)))
                       (set! result (* result i))
                       (fact (<change> (- i 1) j))))))
         (t1 (fork (fact 4)))
         (t2 (fork (fact 5))))
  (join t1)
  (join t2)
  result)
