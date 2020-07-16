(begin
  (define (g x) x)
  (define (h x) (g x))
  (define (f x) (h (g (g (g x)))))
  (f 2))
