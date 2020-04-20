(begin
  (define (require p)
    (if (not p) (amb)))
  (define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items))))

  (define (prime? n)
    (define k (sqrt n))
    (define (check i)
      (cond
        ((> i k) true)
         ((= (remainder n i) 0) false) 
        (else (check (+ i 1)))))
    (check 2))

  (define (prime-sum-pair list1 list2)
    (let ((a (an-element-of list1))
          (b (an-element-of list2)))
      (require (prime? (+ a b)))
      (list a b)))

  (prime-sum-pair '(1 3 5 8) '(20 35 110)))