(letrec ((i 100)
         (thread (lambda (n)
                  (if (<= i 0)
                   #t
                   (begin (set! i (- i 1)) (thread n)))))
         (t1 (future (thread 1)))
         (t2 (future (thread 2)))
         (t3 (future (thread 3))))
 (and
  (deref t1)
  (deref t2)
  (deref t3)
  (<= i 0)))