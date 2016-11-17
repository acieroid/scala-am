;; From D'Osualdo
(letrec ((server-actor (actor "server" (state)
                              (message (x p)
                                       (send p state state)
                                       (become server-actor state))
                              (bye () (terminate))))
         (main-actor (actor "main" ()
                            ))
         (server (create server-actor ))))
