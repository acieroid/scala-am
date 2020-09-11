;; result: 103

(define saved '())
(define done? #f)

(define (foo x)
    (+ x (call/cc bar)))

(define (bar cnt)
  (set! saved cnt)
  42)

(define (main)
  (let ((res (foo 100)))
    (if done?
        res
        (begin 
            (set! done? #t)
            (saved 3)))))

(main)
  