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
  (build-vector N #f (lambda (i) (t/new-lock))))

(define dictionary (t/ref 0))
(define dictionary-lock (t/new-lock))

(define (philosopher i)
  (letrec ((left i)
           (right (modulo (- i 1) N))
           (process (lambda (turn)
                      (if (> turn Turns)
                          'done
                          (if (bool-top)
                              (begin
                                (t/acquire (vector-ref forks (min left right)))
                                (t/acquire (vector-ref forks (max left right)))
                                ;; eat
                                (display "Eating...")
                                (t/release (vector-ref forks (min left right)))
                                (t/release (vector-ref forks (max left right)))
                                (process (+ turn 1)))
                              (begin
                                (t/acquire dictionary-lock)
                                (if (= (t/deref dictionary) i)
                                    (t/ref-set dictionary (modulo (+ i 1) N))
                                    ;; doesn't have the dictionary
                                    'nothing)
                                (t/release dictionary-lock)
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
