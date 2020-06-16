(define (calc-e-iter n)
  (define (iter ctr res fac-prev)
    (if (> ctr n)
        res
        (let ((new-fac (* ctr fac-prev)))
          (iter (+ ctr 1) (+ res (/ 1 new-fac)) new-fac))))
  (iter 1 1 1))

(define (calc-cos x n)
  (define (iter ctr acc fac xpow sign)
    (if (>= ctr n)
        acc
        (let* ((i (* 2 ctr))
               (newfac (* fac (- i 1) i))
               (newxpow (* xpow x x))
               (newsign (- sign)))
          (iter (+ ctr 1)
                (+ acc (/ (* newsign newxpow) newfac))
                newfac
                newxpow
                newsign))))
  (iter 1 1 1 1 1))

(define (close-to x y)
  (< (abs (- x y)) 0.00000001))

(and (close-to (exact->inexact (calc-e-iter 10)) 2.7182818011463845)
     (close-to (calc-cos 0 10) 1)
     (close-to (calc-cos (/ 3.1415 2) 10) 4.6326794876592664e-05)
     (close-to (calc-cos 3.1415 10) -0.9999999992346591))
