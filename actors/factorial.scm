;; From Agha, 1986, p. 54
(letrec ((fact-actor
          (actor "fac" ()
                 (compute (n customer)
                          (if (= n 0)
                              (send customer result 1)
                              (let ((c (create customer-actor n customer)))
                                (send self compute (- n 1) c)))
                          (become fact-actor))))
         (customer-actor
          (actor "customer" (n customer)
                 (result (k)
                         (send customer result (* n k))
                         (become customer-actor n customer))))
         (display-actor
          (actor "display" ()
                 (result (n) (display n))))
         (f (create fact-actor))
         (disp (create display-actor)))
  (send f compute 5 disp))
