;; Adapted from Savina benchmarks ("Frok Join (actor creation)" benchmark, coming from JGF
(letrec ((N 3)
         (perform-computation (lambda (theta)
                                (let ((sint (+ 1 theta))) ; was (sint (sin theta)) in the original implementation
                                  (* sint sint))))
         (forkjoin-actor
          (a/actor "forkjoin" ()
                 (message ()
                          (perform-computation 37.2)
                          (a/terminate)))))
  (a/send (a/create forkjoin-actor) message)
  (a/send (a/create forkjoin-actor) message)
  (a/send (a/create forkjoin-actor) message))
