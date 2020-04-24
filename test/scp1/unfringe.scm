(define (unfringe-1 l)
  (cond ((null? l) '())
        ((null? (cdr l)) (list (car l)))
        (else (list (car l)
                    (unfringe-1 (cdr l))))))

(define (unfringe-2 l)
  (define (pair l)
    (cond ((null? l) '())
          ((null? (cdr l)) (list l))
          (else (cons (list (car l) (cadr l))
                      (pair (cddr l))))))

  (let loop ((l l))
    (if (or (null? l)
            (null? (cdr l)))
        l
        (loop (pair l)))))

(and (equal? (unfringe-1 '(1 2 3 4 5 6 7 8 9)) '(1 (2 (3 (4 (5 (6 (7 (8 (9))))))))))
     (equal? (unfringe-2 '(1 2 3 4 5 6 7 8 9)) '(((((1 2) (3 4)) ((5 6) (7 8))) (((9)))))))