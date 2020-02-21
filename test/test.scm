(define (phi x1 x2) #t)
(define (try f)
  (or (f #t) (f #f)))
(define (sat-solve-2 p)
  (try (lambda (n1)
    (try (lambda (n2)
      (p n1 n2))))))

(sat-solve-2 phi)
