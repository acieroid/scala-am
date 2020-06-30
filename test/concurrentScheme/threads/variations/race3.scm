(letrec ((counter 0)
         (inc (lambda ()
                (set! counter (+ counter 1))))
         (dec (lambda ()
                (set! counter (- counter 1))))
         (t1 (fork (inc)))
         (t2 (fork (dec)))
         (t3 (fork (inc))))
  (join t1)
  (join t2)
  (join t3)
  counter)