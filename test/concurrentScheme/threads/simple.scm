;; Example taken from Dynamic Partial Order Reduction paper
(let* ((x 0)
       (y 0)
       (t1 (fork (begin (set! x 1) (set! x 2))))
       (t2 (fork (begin (set! y 1) (set! x 3)))))
  (join t1)
  (join t2)
  x)
