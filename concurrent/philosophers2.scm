;; Dining philosophers problem
(define Turns (int-top))
(define forks (vector (new-lock) (new-lock)))
(define N 2)
(define (philosopher i)
  (letrec ((left i)
           (right (modulo (- i 1) N))
           (process (lambda (turn)
                      (if (> turn Turns)
                          'done
                          (begin
                            (acquire (vector-ref forks (min left right)))
                            (acquire (vector-ref forks (max left right)))
                            ;; eat
                            (display "Eating...")
                            (release (vector-ref forks (min left right)))
                            (release (vector-ref forks (max left right)))
                            (process (+ turn 1)))))))
    (process 0)))
(define phi0 (spawn (philosopher 0)))
(define phi1 (spawn (philosopher 1)))
(join phi0)
(join phi1)
