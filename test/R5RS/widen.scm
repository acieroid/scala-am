(letrec ((g (lambda ()
              1))
         (f (lambda (n)
              (if (= n 0)
                0
                (+ (f (- n 1)) (g))))))
  (f 10))
