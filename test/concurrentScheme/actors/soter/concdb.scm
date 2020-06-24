;; From D'Osualdo, inspired by Huch
(letrec ((db-actor (actor "db" (l)
                          (allocate (key p)
                                    (if (lookup key l)
                                        ;; Case fail in Soter benchmark
                                        (begin
                                          (send p free)
                                          (become db-write-actor l p key))
                                        ;; Case {succ, V} in Soter benchmark
                                        (begin
                                          (send p allocated)
                                          (become db-actor l))))
                          (lookup (key p)
                                  (send p value (lookup key l))
                                  (become db-actor l))))
         (db-write-actor (actor "db-write" (l p key)
                                ;; In this actor language we can't express the fact that we only receive if p == p2
                                ;; therefore the benchmark isn't completely equivalent, but we can still show that
                                ;; the label client_writes is accessible only by one client at a time
                                (value (value p2)
                                       (if (eq? p p2)
                                           (become db-actor (cons (cons key value) l))
                                           (become db-write-actor l p key)))))
         (client-actor (actor "client" (db)
                              (send-req ()
                                        (if (read-bool)
                                            ;; Case {ok, i} in Soter benchmark
                                            (begin
                                              (send db allocate (read-int) self)
                                              (become client-write-actor db))
                                            ;; Case {ok, l} in Soter benchmark
                                            (begin
                                              (send db lookup (read-int))
                                              (become client-read-actor db))))))
         (client-write-actor (actor "client-write" (db)
                                    (free ()
                                          ;; Client write label
                                          (send db value (read-int) self)
                                          (send self send-req)
                                          (become client-actor db))
                                    (allocated ()
                                     (send self send-req)
                                     (become client-actor db))))
         (client-read-actor (actor "client-read" (db)
                                   (value (v)
                                          (if v
                                              ;; Client fail label
                                              (become client-actor db)
                                              ;; Client reads label
                                              (become client-actor db)))))
         (lookup (lambda (key l)
                   (if (null? l)
                       #f
                       (if (= (caar l) key)
                           (cadr l)
                           (lookup key (cdr l))))))
         (read-bool (lambda () (bool-top)))
         (read-int (lambda () (random 42)))
         (db (create db-actor '()))
         (client1 (create client-actor db))
         (client2 (create client-actor db)))
  (send client1 send-req)
  (send client2 send-req))
