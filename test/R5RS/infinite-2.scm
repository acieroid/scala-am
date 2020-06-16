(letrec ((t (lambda (x) (t (+ x 1)))))
  (t 0))
