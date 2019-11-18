; Expected result: either 1,2,3 or an error (depending on the result of random)

(define (my-cons el lst)
  (cons el lst))

(define my-list
  (my-cons 1
    (my-cons 2
      (my-cons 3 '()))))

(list-ref my-list (random))
