;; Adapted from Savina benchmarks ("Frok Join (actor creation)" benchmark, coming from JGF
(letrec ((N (int-top))
         (perform-computation (lambda (theta) theta))
         (forkjoin-actor
          (a/actor "forkjoin" ()
                 (message ()
                          (perform-computation 37.2)
                          (a/terminate))))
          (loop (lambda (n)
                  (if (= n N)
                      'done
                      (begin
                        (a/send (a/create forkjoin-actor) message)
                        (loop (+ n 1)))))))
  (loop 0))
