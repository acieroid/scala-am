(define global-current 0)

(define (arrow-right arrow-right-arrow)
  (set! global-current (+ 1 global-current)))

(arrow-right (set! global-current 0))
global-current