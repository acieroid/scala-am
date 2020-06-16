;; Example taken from Optimal Dynamic Partial Order Reduction, Figure 4
(let* ((n 2)
       (array (make-vector (+ n 1) 0))
       (thread0 (lambda (i)
                  (if (= (vector-ref array i) 0)
                      i
                      (thread0 (- i 1)))))
       (thread (lambda (j)
                 (vector-set! array j (+ 1 (vector-ref array (- j 1))))))
       (t1 (spawn (thread 1)))
       (t2 (spawn (thread 2))))
  (thread0 n)
  (join t1)
  (join t2))
