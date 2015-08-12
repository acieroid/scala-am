;; Taken from https://github.com/jensnicolay/abstractmemo
;; Expected result: 2
(letrec ((h (lambda (b)
              (letrec ((g (lambda (z) z))
                       (f (lambda (k)
                            (if b
                                (k 1)
                              (k 2))))
                       (y (f (lambda (x) x))))
                (g y))))
         (x (h #t))
         (y (h #f)))
  y)
