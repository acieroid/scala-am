;; Parallel sieve of eratosthenes
(define (build-vector n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))

(define (range a b)
  (letrec ((loop (lambda (i acc)
                   (if (< i a)
                       acc
                       (loop (- i 1) (cons i acc))))))
    (loop (- b 1) '())))



(define (for-each-parallel f l)
  (let ((ts (map (lambda (x) (fork (f x))) l)))
    (map (lambda (t) (join t)) ts)))

(define (sieve N)
  (let ((sqrtN (inexact->exact (floor (sqrt N))))
        (list (build-vector N (ref 0) (lambda (i) (ref 0)))))
    (letrec ((loopc (lambda (c)
                      (if (= c sqrtN)
                          #t
                          (if (= (deref (vector-ref list c)) 0)
                              ;; c unmarked
                              (begin
                                (for-each-parallel
                                 (lambda (m)
                                   (if (= (modulo m c) 0)
                                       ;; m multiple, mark it
                                       (ref-set (vector-ref list m) 1)
                                       #t))
                                 (range (+ c 1) N))
                                (loopc (+ c 1)))
                              ;; c marked
                              (loopc (+ c 1))
                              )))))
      (loopc 2))
    list))

(define (print-primes list)
  (letrec ((loop (lambda (i N)
                   (if (= i N)
                       'done
                       (begin
                         (if (= (deref (vector-ref list i)) 0)
                             (begin
                               (display i)
                               (newline))
                             #t)
                         (loop (+ i 1) N))))))
    (loop 2 (vector-length list))))

(define N (+ 10 (random 42)))
(print-primes (sieve N))
