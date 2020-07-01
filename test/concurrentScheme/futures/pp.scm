(define (new-lock)
  (atom #f))
(define (acquire lock)
  (let try ()
    (if (compare-and-set! lock #f #t)
        #t
        (try))))
(define (release lock)
  (reset! lock #f))

;; Ping-pong
(define N (+ 1 42))
(define Iterations 42)
(define (prev-id id)
  (if (= id 0)
      (- N 1)
      (- id 1)))

(define (ping n str id lock last-ref)
  (acquire lock)
  (if (= (read last-ref) (prev-id id))
      ;; my turn
      (begin
        (display str) (newline)
        (reset! last-ref id)
        (release lock)
        (if (= n Iterations)
            'done
            (ping (+ n 1) str id lock last-ref)))
      ;; not my turn
      (begin
        (release lock)
        (ping n str id lock last-ref))))

(define lck (new-lock))
(define last (atom 1))

(define (do-n n f)
  (letrec ((loop (lambda (i acc)
                   (if (= i n)
                       acc
                       (loop (+ i 1) (cons (f i) acc))))))
    (loop 0 '())))

(define strings (list "ping" "pong" "peng" "pung" "pang"))
(define (pick-str n)
  (list-ref strings (modulo n (length strings))))

(define thrds (do-n N (lambda (i)
                          (future (ping 0 (pick-str i) i lck last)))))
(map (lambda (t) (deref t)) thrds)
