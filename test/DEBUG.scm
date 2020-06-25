;; Benchmark that compare recursive concurrent matrix multiplication with naive sequential matrix multiplication
(define (build-vector n init f)
  (letrec ((v (make-vector n init))
            (loop (lambda (i)
                    (if (< i n)
                      (begin
                        (vector-set! v i (f i))
                        (loop (+ i 1)))
                      v))))
    (loop 0)))

(define (extract-matrix M)
  (build-vector 1 (vector)
    (lambda (i)
      (build-vector 1 0 (lambda (j)
                             (vector-ref (vector-ref M (+ 0 i)) (+ 0 j)))))))

(extract-matrix (build-vector 2 (vector) (lambda (i) (build-vector 2 0 (lambda (j) (random 100))))))