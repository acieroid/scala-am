; Expected result: 3

(define (my-cons el lst)
  (cons el lst))

(define my-list
  (my-cons 1
    (my-cons 2
      (my-cons 3 '()))))

my-list