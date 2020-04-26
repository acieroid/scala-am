(define result '())
(define output (lambda (i) (set! result (cons i result))))
(define linebreak (lambda () (set! result (cons 'linebreak result))))

(define (output-n n x)
  (if (> n 0)
      (begin
        (output x)
        (output-n (- n 1) x))))

(define (parasol n)
  (define (triangle i)
    (if (< i n)
        (begin
          (output-n (- n i 1) " ")
          (output-n (+ (* 2 i) 1) "*")
          (linebreak)
          (triangle (+ i 1)))))

  (define (stick i)
    (if (< i 3)
        (begin
          (output-n (- n 1) " ")
          (output "*")(linebreak)
          (stick (+ i 1)))))

  (triangle 0)
  (stick 0))

(parasol 10)
(equal? result
        '(linebreak
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
          linebreak
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
          linebreak
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
          linebreak
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
          linebreak
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
          linebreak
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
          linebreak
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
          linebreak
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
          linebreak
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
          linebreak
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
          linebreak
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
          linebreak
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
          linebreak
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