;; Producer-consumer problem
(define N (+ 10 (int-top)))
(define NCONS (int-top))
(define buffer (t/ref '()))
(define done (t/ref #f))
(define lock (t/new-lock))
(define (do-something element)
  (display element) (newline))
(define (producer n)
  (if (= n 0)
      (t/ref-set done #t)
      (begin
        (t/acquire lock)
        (t/ref-set buffer (cons n (t/deref buffer)))
        (t/release lock)
        (producer (- n 1)))))
(define (consumer)
  (if (null? (t/deref buffer))
      (begin
        (if (t/deref done)
            'done
            (consumer)))
      (begin
        (t/acquire lock)
        (do-something (car (t/deref buffer)))
        (t/ref-set buffer (cdr (t/deref buffer)))
        (t/release lock)
        (consumer))))

(define producer-thread (fork (producer N)))

(define (do-n n f)
  (if (= n 0)
      '()
      (cons (f) (do-n (- n 1) f))))

(define consumer-threads (do-n NCONS (lambda () (fork (consumer)))))

(join producer-thread)
(map (lambda (t) (join t)) consumer-threads)
