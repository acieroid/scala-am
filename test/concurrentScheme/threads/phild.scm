;; Dining philosophers problem with dictionary
(define N (+ 1 (random 42)))
(define Turns (random 42))

(define (build-vector n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))
(define forks
  (build-vector N #f (lambda (i) (new-lock))))

(define dictionary (ref 0))
(define dictionary-lock (new-lock))

(define (philosopher i)
  (letrec ((left i)
           (right (modulo (- i 1) N))
           (process (lambda (turn)
                      (if (> turn Turns)
                          'done
                          (if (bool-top)
                              (begin
                                (acquire (vector-ref forks (min left right)))
                                (acquire (vector-ref forks (max left right)))
                                ;; eat
                                (display "Eating...")
                                (release (vector-ref forks (min left right)))
                                (release (vector-ref forks (max left right)))
                                (process (+ turn 1)))
                              (begin
                                (acquire dictionary-lock)
                                (if (= (deref dictionary) i)
                                    (ref-set dictionary (modulo (+ i 1) N))
                                    ;; doesn't have the dictionary
                                    'nothing)
                                (release dictionary-lock)
                                (process turn)))))))
    (process 0)))

(define (do-n n f)
  (letrec ((loop (lambda (i acc)
                   (if (= i n)
                       acc
                       (loop (+ i 1) (cons (f i) acc))))))
    (loop 0 '())))

(define philosophers (do-n N (lambda (i) (fork (philosopher i)))))

;; Wait until the end
(map (lambda (t) (join t)) philosophers)
