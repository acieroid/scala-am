(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (map f (cdr lst)))))


(define (inc n) (+ n 1))
(map inc '(1 2 3))