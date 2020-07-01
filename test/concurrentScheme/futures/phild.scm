;; Dining philosophers problem with dictionary
(define N (+ 1 42))
(define Turns 42)

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

(define dictionary (atom 0))
(define dictionary-lock (new-lock))

(define (philosopher i)
  (letrec ((left i)
           (right (modulo (- i 1) N))
           (process (lambda (turn)
                      (if (> turn Turns)
                          'done
                          (if #t
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
                                (if (= (read dictionary) i)
                                    (reset! dictionary (modulo (+ i 1) N))
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

(define philosophers (do-n N (lambda (i) (future (philosopher i)))))

;; Wait until the end
(map (lambda (t) (deref t)) philosophers)
