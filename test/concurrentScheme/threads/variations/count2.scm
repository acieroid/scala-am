(letrec ((i 100)
         (thread (lambda (n)
                 (if (<= i 0)
                     #t
                     (begin (set! i (- i 1)) (thread n)))))
(t1 (fork (thread 1)))
(t2 (fork (thread 2))))
(join t1)
(join t2))
