;; Example taken from Optimal Dynamic Partial Order Reduction, Figure 4
(let* ((n 2)
       (array (make-vector (+ n 1) 0)))
  (letrec ((thread0 (lambda (i)
                      (if (= (vector-ref array i) 0)
                          i
                          (thread0 (- i 1)))))
            (thread (lambda (j)
                      (vector-set! array j (+ 1 (vector-ref array (- j 1)))))))
    (let ((t1 (<change> (thread 1) (fork (thread 1)))) ; <==============================================================
          (t2 (thread 2)))
      (thread0 n)
      (<change> t1 (join t1)) ; <=======================================================================================
      t2)))
