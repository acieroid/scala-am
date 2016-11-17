;; From SOTER benchmarks (pipe). Description from SOTER:
;; A simple sequence of N processes arranged in a linear pipe: every process knows only the next in the row and forwards everything to it.
;; A property Soter can prove is that the mailbox of each pipe_node contains at most 1 message at any given time.
;; Adapted from (Kobayashi, Nakade and Yonezawa, 1995)
(letrec ((pipe-node (actor "pipe-node" (f next)
                           (message (m)
                                    (send next message (f m))
                                    (become pipe-node f next))))
         (init-pipe (lambda (f n next)
                      (if (= n 0)
                          next
                          (init-pipe f (- n 1) (create pipe-node f next)))))
         (sink-actor (actor "sink" ()
                            (message (m) (terminate))))
         (sink (create sink-actor))
         (N 3)
         (head (init-pipe (lambda (x) (+ x 1)) N sink)))
  (send head message 0))
