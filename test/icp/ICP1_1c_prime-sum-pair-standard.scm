(begin
  (define true #t)
  (define false #f)

  (define (prime? n)
    (define k (sqrt n))
    (define (check i)
      (cond
        ((> i k) true)
         ((= (remainder n i) 0) false) 
        (else (check (+ i 1)))))
    (check 2))

  (define (prime-sum-pair list1 list2)
    (for-each
      (lambda (n1)
        (for-each
          (lambda(n2)
            (cond
              ((prime? (+ n1 n2))
               (display (list n1 n2)) 
               (newline))))
          list2))
      list1))

   (prime-sum-pair '(1 3 5 8) '(20 35 110)))