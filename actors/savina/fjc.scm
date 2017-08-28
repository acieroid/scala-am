;; Adapted from Savina benchmarks ("Frok Join (actor creation)" benchmark, coming from JGF
(define N (int-top))
(define (perform-computation theta)
  (let ((sint (+ 1 theta)))
    (* sint sint)))
(define forkjoin-actor
  (a/actor "forkjoin" ()
           (message ()
                    (perform-computation 37.2)
                    (a/terminate))))
(define loop (lambda (n)
               (if (= n N)
                   'done
                   (begin
              (a/send (a/create forkjoin-actor) message)
              (loop (+ n 1))))))
(loop 0)
