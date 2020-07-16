(letrec ((rotate (lambda (n x y z)
                   (if (= n 0)
                       y
                     (rotate (- n 1) y z x)))))
  (rotate 41 5 #t "hallo"))
