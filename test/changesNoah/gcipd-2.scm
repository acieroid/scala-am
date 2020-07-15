(letrec ((f (lambda (n)
              (if (<= n 1)
                1
                (* n (f (- n 1))))))
         (g (lambda (n)
              (if (<= n 1)
                1
                (+ (* n n) (g (- n 1)))))))
  (+ (f 2) (g 2)))