(define master
  (a/actor "master"))

(define series-worker
  (a/actor "series-worker" (computer)
           (next-term ()
                      (a/send computer compute ))
           (stop () (a/terminate))))

(define (compute-next-term cur rate)
  (* rate cur (- 1 cur)))

(define rate-computer
  (a/actor "rate-computer" (rate)
           (compute (term sender)
                    (a/send sender result (compute-next-term term rate)))
           (stop ()
                 (a/terminate))))

(define master-actor (a/create master ???))
(a/send master-actor start)
