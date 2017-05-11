(define result '())
(define display (lambda (i) (set! result (cons i result))))
(define newline (lambda () (set! result (cons 'newline result))))

(define (display-n n x)
  (if (> n 0)
      (begin
        (display x)
        (display-n (- n 1) x))))

(define (parasol n)
  (define (triangle i)
    (if (< i n)
        (begin
          (display-n (- n i 1) " ")
          (display-n (+ (* 2 i) 1) "*")
          (newline)
          (triangle (+ i 1)))))

  (define (stick i)
    (if (< i 3)
        (begin
          (display-n (- n 1) " ")
          (display "*")(newline)
          (stick (+ i 1)))))

  (triangle 0)
  (stick 0))

(parasol 10)
(equal? result
        '(newline
          "*"
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          newline
          "*"
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          newline
          "*"
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          newline
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          newline
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          " "
          newline
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          " "
          " "
          newline
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          " "
          " "
          " "
          newline
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          " "
          " "
          " "
          " "
          newline
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          " "
          " "
          " "
          " "
          " "
          newline
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          "*"
          " "
          " "
          " "
          " "
          " "
          " "
          newline
          "*"
          "*"
          "*"
          "*"
          "*"
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          newline
          "*"
          "*"
          "*"
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          newline
          "*"
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "))