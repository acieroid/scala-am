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