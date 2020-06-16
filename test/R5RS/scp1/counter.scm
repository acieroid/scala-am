(define result '())
(define output (lambda (i) (set! result (cons i result))))

(define (count1 x)
  (cond ((= 0 x) (display x))
        (else (display x)
              (count1 (- x 1)))))

(define (count2 x)
  (cond ((= 0 x) (display x))
        (else (count2 (- x 1))
              (display x))))

(count1 4)
(count2 4)
(equal? result '(4 3 2 1 0 0 1 2 3 4))