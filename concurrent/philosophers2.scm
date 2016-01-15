;; Dining philosophers problem
(letrec ((make-initialized-vector (lambda (n f)
                                    (letrec ((v (make-vector n (bottom)))
                                             (loop (lambda (i)
                                                     (if (= i n)
                                                         v
                                                         (begin
                                                           (vector-set! v i (f i))
                                                           (loop (+ i 1)))))))
                                      (loop 0))))
         (join-all (lambda (v)
                     (letrec ((loop (lambda (i)
                                      (if (= i (vector-length v))
                                          #t
                                          (begin (join (vector-ref v i))
                                                 (loop (+ i 1)))))))
                       (loop 0)))))
  (letrec ((n 2) ; number of philosophers
           (turns 5) ; number of turns to run
           (forks (make-initialized-vector n (lambda (x) (new-lock))))
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
           (threads (make-initialized-vector n (lambda (i) (spawn (philosopher i))))))
    (join-all threads)))
