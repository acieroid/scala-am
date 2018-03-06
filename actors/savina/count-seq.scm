;; Bound: producter-actor: 1, counting-actor: 2
;; Error: not reachable
(letrec ((producer-actor
          (a/actor "producer" (counter)
                 (increment ()
                            (a/send counter increment)
                            (a/send counter retrieve a/self)
                            (a/become producer-actor counter))
                 (result (count)
                         (if (= count 1)
                             (display "Success!")
                             (error "Error!"))
                         (a/terminate))))
         (counting-actor
          (a/actor "counting" (count)
                 (increment ()
                            (a/become counting-actor (+ count 1)))
                 (retrieve (to)
                           (a/send to result count)
                           (a/terminate))))
         (counter (a/create counting-actor 0))
         (producer (a/create producer-actor counter)))
  (a/send producer increment))
