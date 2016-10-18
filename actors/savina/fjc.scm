;; Adapted from Savina benchmarks ("Frok Join (actor creation)" benchmark, coming from JGF
(letrec ((N 10)
         (perform-computation (lambda (theta)
                                (let ((sint (sin theta)))
                                  (* sint sint))))
         (forkjoin-actor
          (actor "forkjoin" ()
                 (message ()
                          (perform-computation 37.2)
                          (terminate))))
         (loop (lambda (n)
                 (if (= n N)
                     'done
                     (begin
                       (send (create forkjoin-actor) message)
                       (loop (+ n 1)))))))
  (loop 0))
