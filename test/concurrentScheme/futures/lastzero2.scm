;; Example taken from Optimal Dynamic Partial Order Reduction, Figure 4
(letrec ((n 2)
         (array (atom (make-vector (+ n 1) 0)))
         (thread0 (lambda (i)
                   (if (= (vector-ref (deref atom) i) 0)
                    i
                    (thread0 (- i 1)))))
         (thread (lambda (j)
                  (swap! atom (lambda (v) (vector-set! v j (+ 1 (vector-ref v (- j 1))))))))
         (t1 (future (thread 1)))
         (t2 (future (thread 2))))
 (thread0 n)
 (deref t1)
 (deref t2)
 #t)