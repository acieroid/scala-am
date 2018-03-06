;; From Agha 1986, p. 52
(letrec ((stack-node (a/actor "stack-node" (content link)
                            (pop (customer)
                                 (if link
                                     (begin
                                       (a/send customer message content)
                                       (link))
                                     (begin
                                       (error "popping an empty stack")
                                       (a/terminate))))
                            (push (v)
                                  (a/become stack-node v (lambda () (a/become stack-node content link))))))
         (display-actor (a/actor "display" ()
                               (message (v) (display v) (a/become display-actor))))
         (disp (a/create display-actor))
         (act (a/create stack-node #f #f)))
  (a/send act push (int-top))
  (a/send act push (bool-top))
  (a/send act push 3)
  (a/send act pop disp))
