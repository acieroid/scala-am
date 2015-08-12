;; Taken from https://github.com/jensnicolay/abstractmemo
;; Expected result: #f
(letrec ((do-something (lambda () 10))
         (id (lambda (y)
               (letrec ((tmp1 (do-something)))
                 y)))
         (tmp2 ((id (lambda (a) a)) #t)))
  ((id (lambda (b) b)) #f))
