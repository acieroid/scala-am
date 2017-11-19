(define n 5)
(define size 16)
(define done #f)
(define (produce) (random 10))
(define buffer (make-vector size 0))
;; Producer-consumer problem implemented with semaphores
(letrec ((n 5)                          ; number of items produced
         (size 16)                      ; buffer size
         (done #f)                      ; set to #t when producer finished
         (produce (lambda () (random 10)))
         (l (new-lock))
         (buffer (make-vector size 0))
         (head 0)
         (tail 0)
         (count 0)
         (producer (lambda (n)
                     (if (= n 0)
                         (set! done #t)
                         (begin
                           (acquire l)
                           (if (= count size)
                               (begin
                                 (release l)
                                 (producer n))
                               (let ((item (produce)))
                                 (display 'prod-) (display item) (newline)
                                 (set! count (+ count 1))
                                 (vector-set! buffer head item)
                                 (set! head (modulo (+ head 1) size))
                                 (release l)
                                 (producer (- n 1))))))))
         (consumer (lambda ()
                     (if (and done (= count 0))
                         #t
                         (begin
                           (acquire l)
                           (if (= count 0)
                               (begin
                                 (release l)
                                 (consumer))
                               (begin
                                 (set! count (- count 1))
                                 (display 'cons-) (display (vector-ref buffer tail)) (newline)
                                 (set! tail (modulo (+ tail 1) size))
                                 (release l)
                                 (consumer)))))))
         (t1 (spawn (producer n)))
         (t2 (spawn (consumer))))
  (join t1)
  (join t2))
