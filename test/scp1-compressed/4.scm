; 4.1
(define result '())
(define display (lambda (i) (set! result (cons i result))))
(define newline (lambda () (set! result (cons 'newline result))))

(define (print-abc a b c)
  (display a) (display " ")
  (display b) (display " ")
  (display c) (newline))

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
(equal? result '(newline 3 " " 2 " " 1 newline 4 " " 3 " " 3 newline 4 " " 6 " " 4 newline 5 " " 3 " " 4 newline 3 " " 2 " " 1))

; 4.8
(define (incr x) (+ x 1))
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

(define (simp-int f a b n)
  (let ((h (/ (- b a) n)))
    (define (y k) (f (+ a (* h k))))
    (define (term k)
      (* (if (or (= k 0)(= k n)) 1 (+ 2 (* 2 (modulo k 2))))
        (y k)))
    (/ (* h (sum term 0 incr n)) 3)))

(define r (sqrt 2))
(and (= (simp-int (lambda (x) x) 0 10 100) 50)
  (= (simp-int (lambda (x) (sqrt (- (* r r) (* x x)))) (- r) r 100) 3.1402925778303366))
