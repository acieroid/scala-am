;; Producer-consumer problem
(define N (+ 10 (random 42)))
(define NCONS (random 42))
(define buffer (ref '()))
(define done (ref #f))
(define lock (new-lock))
(define (do-something element)
  (display element) (newline))
(define (producer n)
  (if (= n 0)
      (ref-set done #t)
      (begin
        (acquire lock)
        (ref-set buffer (cons n (deref buffer)))
        (release lock)
        (producer (- n 1)))))
(define (consumer)
  (if (null? (deref buffer))
      (begin
        (if (deref done)
            'done
            (consumer)))
      (begin
        (acquire lock)
        (do-something (car (deref buffer)))
        (ref-set buffer (cdr (deref buffer)))
        (release lock)
        (consumer))))

(define producer-thread (fork (producer N)))

(define (do-n n f)
  (if (= n 0)
      '()
      (cons (f) (do-n (- n 1) f))))

(define consumer-threads (do-n NCONS (lambda () (fork (consumer)))))

(join producer-thread)
(map (lambda (t) (join t)) consumer-threads)
