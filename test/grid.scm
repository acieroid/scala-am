;; BEGIN PRELUDE

(define (equal? a b)
  (or (eq? a b)
      (and (null? a) (null? b))
      (and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
      (and (vector? a) (vector? b)
      (let ((n (vector-length a)))
        (and (= (vector-length b) n)
             (letrec ((loop (lambda (i)
                              (or (= i n)
                                  (and (equal? (vector-ref a i) (vector-ref b i))
                                  (loop (+ i 1)))))))
                (loop 0)))))))

;; END PRELUDE

(define (make-grid start dims)
  (let ((v (make-vector (car dims) start)))
    (if (not (null? (cdr dims)))
        (letrec ((loop (lambda (i)
                         (if (>= i (car dims))
                             #t
                             (begin
                               (vector-set! v i
                                            (make-grid start (cdr dims)))
                               (loop (+ i 1)))))))
          (loop 0))
        #t)
    v))

(define (grid-ref g n)
  (if (null? (cdr n))
      (vector-ref g (car n))
      (grid-ref (vector-ref g (car n)) (cdr n))))

(define (grid-set! g v n)
  (if (null? (cdr n))
      (vector-set! g (car n) v)
      (grid-set! (vector-ref g (car n)) v (cdr n))))

(define t (make-grid 0 '(4 5 6)))
(define u (make-grid #f '(2 2)))

(and (equal? (grid-ref t '(2 2 3)) 0)
     (begin
       (grid-set! t '24 '(2 2 3))
       (equal? (grid-ref t '(2 2 3)) 24))
     (equal? (grid-ref t '(1 0)) (make-vector 6 0))
     (begin
       (grid-set! t #t '(1 0))
       (equal? (grid-ref t '(1 0)) #t)))
