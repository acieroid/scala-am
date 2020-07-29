;; Monte-carlo simulation using thrds
(define MAXSIZE 100)

(define (inside-circle? radius x y)
  (< (+ (* x x) (* y y)) (* radius radius)))

(define (monte-carlo-seq size n)
  ;; Int -> Int
  ;; places n points randomly on a size * size square, check how many are inside a size-radius quarter circle and return that number
  (define (monte-carlo-helper i amount)
    (if (= i 0)
        amount
        (let ((px (random size))
              (py (random size)))
          (if (inside-circle? size px py)
              (monte-carlo-helper (- i 1) (+ amount 1))
              (monte-carlo-helper (- i 1) amount)))))
  (monte-carlo-helper n 0))

(define (monte-carlo-conc size n)
  (if (< n MAXSIZE)
      (monte-carlo-seq size n)
      (let ((t1 (fork (monte-carlo-conc size (quotient n 2))))
            (t2 (fork (monte-carlo-conc size (quotient n 2)))))
        (+ (join t1) (join t2)))))

(define (approximate-pi size iterations)
  (/ (* 4.0 (monte-carlo-conc size iterations)) iterations))

(define radius (<change> 100 -100)) ; <=================================================================================
(define pi (approximate-pi radius 100))
(display pi)
(if (< (abs (- 3.14 pi)) 0.01)
    (<change> (display "looks like pi") #t) ; <=========================================================================
    (<change> (display "not really good") #f)) ; <======================================================================
; To make things parse:
#t