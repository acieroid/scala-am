(define (reverse lst)
  (define (go lst tail)
    (if (null? lst) tail
        (go (cdr lst) (cons (car lst) tail))))
  (go lst '()))

(define (append lst1 lst2)
  (define (loop lst res)
    (if (null? lst)
        res
        (loop (cdr lst) (cons (car lst) res))))
  (loop (reverse lst1) lst2))

(and (equal? (append '(1 2 3) '(4 5)) '(1 2 3 4 5))
     (equal? (append '(1 2 3) '()) '(1 2 3))
     (equal? (append '() '(1 2 3)) '(1 2 3))
     (null? (append '() '())))
