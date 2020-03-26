(define (atom? x)
  (not (pair? x)))

(define (fringe l)
  (cond ((null? l) '())
    ((atom? l) (list l))
    (else (append (fringe (car l))
            (fringe (cdr l))))))

(equal? (fringe '((1) ((((2)))) (3 (4 5) 6) ((7) 8 9))) '(1 2 3 4 5 6 7 8 9))