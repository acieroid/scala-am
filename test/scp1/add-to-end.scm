(define (add-to-end e l)
  (if (null? l)
      (cons e '())
      (cons (car l) (add-to-end e (cdr l)))))

(and (equal? (add-to-end 999 '(1 2 3 4 5)) '(1 2 3 4 5 999))
     (equal? (add-to-end 999 '()) '(999))
     (equal? (add-to-end 999 '(1)) '(1 999)))