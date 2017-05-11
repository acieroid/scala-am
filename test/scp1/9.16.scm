(define (insert-aux! lst lst2)
  (set-cdr! lst2 '())
  (if (null? (cdr lst))
      (set-cdr! lst lst2)
      (insert-aux! (cdr lst) lst2))
  lst)

(define (insert! lst1 lst2)
  (if (not (null? lst1))
      (begin
        (insert! (cdr lst1) (cdr lst2))
        (insert-aux! (car lst1) lst2)
        lst1)))

(and (equal? (insert-aux! '(a 12 q) '(v w x y z)) '(a 12 q v))
     (equal? (insert! '((a 12 q) (b 13) (c 14 r s) (f 18) (j 22 t)) '(v w x y z))
             '((a 12 q v) (b 13 w) (c 14 r s x) (f 18 y) (j 22 t z))))