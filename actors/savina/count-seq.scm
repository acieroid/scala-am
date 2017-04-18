;; Bound: producer-actor: 1, counting-actor: 2
;; Error: not reachable
(letrec ((producer-actor
          (actor "producer" (counter)
                 (increment ()
                            (send counter increment)
                            (send counter retrieve self)
                            (become producer-actor counter))
                 (result (count)
                         (if (= count 1)
                             (display "Success!")
                             (error "Error!"))
                         (terminate))))
         (counting-actor
          (actor "counting" (count)
                 (increment ()
                            (become counting-actor (+ count 1)))
                 (retrieve (to)
                           (send to result count)
                           (terminate))))
         (counter (create counting-actor 0))
         (producer (create producer-actor counter)))
  (send producer increment))
