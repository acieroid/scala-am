;; From Agha, 1986, p. 54
(define fact-actor
  (a/actor "fact-actor" ()
           (compute (n customer)
                    (if (= n 0)
                        (a/send customer result 1)
                        (let ((c (a/create customer-actor n customer)))
                          (a/send a/self compute (- n 1) c)))
                    (a/become fact-actor))))
(define customer-actor
  (a/actor "customer" (n customer)
           (result (k)
                   (a/send customer result (* n k))
                   (a/become customer-actor n customer))))
(define display-actor
  (a/actor "display" ()
           (result (n) (display n) (a/become display-actor))))
(define f (a/create fact-actor))
(define disp (a/create display-actor))
(a/send f compute 5 disp)
