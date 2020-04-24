(define (all-but-interval lst min max)
  (define (aux last-smaller-cons aux-lst)
    (cond
      ((null? aux-lst)
       (set-cdr! last-smaller-cons '()))
      ((< (car aux-lst) min)
       (aux aux-lst (cdr aux-lst)))
      ((> (car aux-lst) max)
       (set-cdr! last-smaller-cons aux-lst))
      (else
       (aux last-smaller-cons (cdr aux-lst)))))
  (aux lst lst)
  lst)

(and (equal? (all-but-interval '(1 2 3 4 5 6) 2 4) '(1 5 6))
     (equal? (all-but-interval '(1 2 3 4 5) 2 2) '(1 3 4 5))
     (equal? (all-but-interval '(1 2 5 6 7) 3 9) '(1 2)))