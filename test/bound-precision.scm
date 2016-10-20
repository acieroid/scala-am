(letrec ((f (lambda (x)
              (if (< x 100)
                  f
                  1)))
         (f2 (f 5)))
  (f2 2))
