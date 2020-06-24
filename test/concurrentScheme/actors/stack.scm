;; From Agha 1986, p. 52
(letrec ((stack-node (actor "stack-node" (content link)
                            (pop (customer)
                                 (if link
                                     (begin
                                       (send customer message content)
                                       (link))
                                     (begin
                                       (error "popping an empty stack")
                                       (terminate))))
                            (push (v)
                                  (become stack-node v (lambda () (become stack-node content link))))))
         (display-actor (actor "display" ()
                               (message (v) (display v) (become display-actor))))
         (disp (create display-actor))
         (act (create stack-node #f #f)))
  (send act push (random 42))
  (send act push (bool-top))
  (send act push 3)
  (send act pop disp))
