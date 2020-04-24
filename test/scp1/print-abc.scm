(define result '())
(define output (lambda (i) (set! result (cons i result))))
(define linebreak (lambda () (set! result (cons 'linebreak result))))

(define (print-abc a b c)
  (output a) (output " ")
  (output b) (output " ")
  (output c) (linebreak))

(define (foo a b c)
  (print-abc a b c)
  (let ((a 4)
        (c 5)
        (b c))
    (print-abc a b c)
    (let ((b 6)
          (c a))
      (print-abc a b c))
    (let ((a b)
          (c a))
      (print-abc a b c)))
  (print-abc a b c))

(foo 1 2 3)
(equal? result '(linebreak 3 " " 2 " " 1 linebreak 4 " " 3 " " 3 linebreak 4 " " 6 " " 4 linebreak 5 " " 3 " " 4 linebreak 3 " " 2 " " 1))