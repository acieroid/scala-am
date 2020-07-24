;; Producer-consumer problem
(define N (+ 10 (random 42)))
(define NCONS (random 42))
(define buffer (ref '())) ; Protected by lock.
(define done (ref #f))
(define lock (new-lock))
(define done-lock (new-lock))
(define (do-something element)
  (display element) (newline))
(define (producer n)
  (if (= n 0)
      (begin
        (acquire done-lock)
        (ref-set done #t)
        (release done-lock))
      (begin
        (acquire lock)
        (ref-set buffer (cons n (deref buffer)))
        (release lock)
        (producer (- n 1)))))
(define (consumer)
  (acquire lock)
  (if (null? (deref buffer))
      (begin
        (release lock)
        (acquire done-lock)
        (let ((res (if (deref done)
                       'done
                       (consumer))))
          (release done-lock)
          res))
      (begin
        (do-something (car (deref buffer)))
        (ref-set buffer (cdr (deref buffer)))
        (release lock)
        (consumer))))

(define producer-thread (<change> (fork (producer N)) (lambda (n) (fork (producer n))))) ; <============================

(define (do-n n f)
  (if (= n 0)
      '()
      (cons (f) (do-n (- n 1) f))))

(define consumer-threads (do-n NCONS (lambda () (fork (consumer)))))

(join (<change> producer-thread (producer-thread N))) ; <===============================================================
(map (lambda (t) (join t)) consumer-threads)
