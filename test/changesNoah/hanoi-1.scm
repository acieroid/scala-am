(letrec ((move-them (lambda (n from to helper)
     (if (> n 1)
         (begin
            (move-them (- n 1) from helper to)
            (move-them (- n 1) helper to from))))))
    (move-them 5 0 1 2))
