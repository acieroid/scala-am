;; Taken from http://rosettacode.org/wiki/Roots_of_a_quadratic_function
;; Expected result: #t

(define (quadratic a b c)
  (if (= a 0)
      (if (= b 0) 'fail (- (/ c b)))
      (let ((delta (- (* b b) (* 4 a c))))
        (if (and (real? delta) (> delta 0))
            (let ((u (+ b (* (if (>= b 0) 1 -1) (sqrt delta)))))
              (list (/ u -2 a) (/ (* -2 c) u)))
            (list
             (/ (- (sqrt delta) b) 2 a)
             (/ (+ (sqrt delta) b) -2 a))))))

(let ((res1 (quadratic 0 0 1))
      (exp1 'fail)
      (res2 (quadratic 1 2 0))
      (exp2 (cons -2 (cons 0 '()))))
  (and (eq? res1 exp1)
       (equal? res2 exp2)))
