(letrec ((inc (lambda (x) (+ x 1))))
  (inc (inc 2)))
