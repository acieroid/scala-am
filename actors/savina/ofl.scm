(define Alpha (int-top))
(define F (int-top))
(define GridSize (int-top))
(define NumPoints (int-top))

(define producer-actor
  (a/create "producer-actor" (consumer items-produced)
            (next-customer ()
                           (if (< items-produced NumPoints)
                               (begin
                                 (a/send consumer customer self (random-point GridSize))
                                 (a/become producer-actor consumer (+ items-produced 1)))
                               (begin
                                 (a/send consumer request-exit)
                                 (a/terminate))))))
(define quadrant-actor
  (a/create "quadrant-actor" (parent position-relative-to-parent
                                     box )))
(define threshold (* Alpha F))
(define )
