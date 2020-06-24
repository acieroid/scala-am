;; Simple program to show randomness of a concurrent program. (Note that race conditions are present in the program, which should increase randomness even more.)
;; Author: jevdplas

(define count 0)

(define (reader n)
  (let loop ((n n))
    (if (> n 0)
      (begin (display count)
        (loop (- n 1))))))

(define (writer m)
  (let loop ((m m))
    (if (> m 0)
      (begin (set! count m)
        (loop (- m 1))))))

(define threads '())

(let loop ((n 100))
  (cond ((= n 0) #t)
    ((even? n) (set! threads (cons (fork (writer (/ n 2))) threads))
      (loop (- n 1)))
    (else (set! threads (cons (fork (reader n)) threads))
      (loop (- n 1)))))

(for-each (lambda (x) (join x)) threads)

