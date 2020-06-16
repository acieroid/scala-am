;; Producer-consumer problem
(define N (+ 10 42))
(define NCONS 42)
(define buffer (atom '()))
(define done (atom #f))
(define lock (t/new-lock))
(define (do-something element)
  (display element) (newline))
(define (producer n)
  (if (= n 0)
      (reset! done #t)
      (begin
        (t/acquire lock)
        (reset! buffer (cons n (read buffer)))
        (t/release lock)
        (producer (- n 1)))))
(define (consumer)
  (if (null? (read buffer))
      (begin
        (if (read done)
            'done
            (consumer)))
      (begin
        (t/acquire lock)
        (do-something (car (read buffer)))
        (reset! buffer (cdr (read buffer)))
        (t/release lock)
        (consumer))))

(define producer-thrd (future (producer N)))

(define (do-n n f)
  (if (= n 0)
      '()
      (cons (f) (do-n (- n 1) f))))

(define consumer-thrds (do-n NCONS (lambda () (future (consumer)))))

(deref producer-thrd)
(map (lambda (t) (deref t)) consumer-thrds)
