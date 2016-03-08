;; Dining philosophers problem
(letrec ((n 2)                          ; number of philosophers
         (turns 5)                      ; number of turns to run
         (forks (vector (new-lock) (new-lock)))
         (pickup (lambda (left right)
                   (acquire (vector-ref forks (min left right)))
                   (acquire (vector-ref forks (max left right)))))
         (putdown (lambda (left right)
                    (release (vector-ref forks (min left right)))
                    (release (vector-ref forks (max left right)))))
         (philosopher (lambda (i)
                        (letrec ((left i)
                                 (right (modulo (- i 1) n))
                                 (process (lambda (turn)
                                            (if (> turn turns)
                                                #t
                                                (begin
                                                  (pickup left right)
                                                  (display i) (newline)
                                                  (putdown left right)
                                                  (process (+ turn 1)))))))
                          (process 0))))
         (t1 (spawn (philosopher 0)))
         (t2 (spawn (philosopher 1))))
  (join t1)
  (join t2))
