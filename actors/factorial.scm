;; From Agha, 1986, p. 54
(letrec ((fact-actor
          (a/actor "fac" ()
                 (compute (n customer)
                          (if (= n 0)
                              (a/send customer result 1)
                              (let ((c (a/create customer-actor n customer)))
                                (a/send self compute (- n 1) c)))
                          (a/become fact-actor))))
         (customer-actor
          (a/actor "customer" (n customer)
                 (result (k)
                         (a/send customer result (* n k))
                         (a/become customer-actor n customer))))
         (display-actor
          (a/actor "display" ()
                 (result (n) (a/display n))))
         (f (a/create fact-actor))
         (disp (a/create display-actor)))
  (a/send f compute 5 disp))
