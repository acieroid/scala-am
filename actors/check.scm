;; Benchmark showing that when actors are allocated at the same call site, precision is lost.
(letrec ((check-actor (actor "check" (n)
                             (check (m)
                                    (if (= m n) (become check-actor n) (error "error!")))))
         (new-check (lambda (n) (create check-actor n)))
         ;; Replace (new-check i) by (create check-actor i) and the program is easily proved to be error-free
         ;; Note: it isn't the case right now but it will be (TODO). The problem comes from the fact that
         ;; it is the main process that allocates check-actor's constructor argument n, therefore c1, c2
         ;; and c3's n are allocated the same address, and 1, 2 and 3 are joined together, resulting in Int.
         (c1 (new-check 1))
         (c2 (new-check 2))
         (c3 (new-check 3))
         )
  (send c1 check 1)
  (send c2 check 2)
  (send c3 check 3)
  )
