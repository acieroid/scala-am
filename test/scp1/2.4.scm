(define (derde-machtswortel x)
  (define epsilon 0.01)
  (define (hulp-derde-machtswortel y)
    (if (< (abs (- (* y y y) x)) epsilon)
        y
        (hulp-derde-machtswortel (/ (+ (/ x (* y y)) y y) 3))))
  (hulp-derde-machtswortel (/ x 3)))

(= 3.000000068671529 (exact->inexact (derde-machtswortel 27)))