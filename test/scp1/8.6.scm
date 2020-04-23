(define result '())
(define output (lambda (i) (set! result (cons i result))))

(define hulp 2)
(define (haha x)
  (let ((hulp (* x hulp)))
    (output hulp))
  (output hulp)
  (set! hulp 4))

(haha 2)
(haha 3)
(equal? result '(4 12 2 4))