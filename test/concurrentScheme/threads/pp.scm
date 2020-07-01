;; Ping-pong
(define N (+ 1 (random 42)))
(define Iterations (random 42))
(define (prev-id id)
  (if (= id 0)
      (- N 1)
      (- id 1)))

(define (ping n str id lock last-ref)
  (acquire lock)
  (if (= (deref last-ref) (prev-id id))
      ;; my turn
      (begin
        (display str) (newline)
        (ref-set last-ref id)
        (release lock)
        (if (= n Iterations)
            'done
            (ping (+ n 1) str id lock last-ref)))
      ;; not my turn
      (begin
        (release lock)
        (ping n str id lock last-ref))))

(define lck (new-lock))
(define last (ref 1))

(define (do-n n f)
  (letrec ((loop (lambda (i acc)
                   (if (= i n)
                       acc
                       (loop (+ i 1) (cons (f i) acc))))))
    (loop 0 '())))

(define strings (vector "ping" "pong" "peng" "pung" "pang"))
(define (pick-str n)
  (vector-ref strings (modulo n (vector-length strings))))

(define threads (do-n N (lambda (i)
                          (fork (ping 0 (pick-str i) i lck last)))))
(map (lambda (t) (join t)) threads)
