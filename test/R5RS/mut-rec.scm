(letrec ((even? (lambda (x)
                  (if (= x 0)
                      #t
                      (odd? (- x 1)))))
         (odd? (lambda (x)
                 (if (= x 0)
                     #f
                     (even? (- x 1))))))
  (even? 4))
