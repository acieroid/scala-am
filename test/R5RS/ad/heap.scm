(define (my-++ n) (+ n 1))

(define (my--- n) (- n 1))

(define false #f)

(define true #t)

(define nil '())

(define (key x) x)

(define (make-heap a-vector nr-of-elements)
  (define (iter index)
    (cond ((> index 0)
           (sift-down a-vector index nr-of-elements)
           (iter (my--- index)))))
  (iter (quotient nr-of-elements 2)))

(define (sift-down heap from to)
  (define (smallest-child parent)
    (let* ((child1 (* 2 parent))
           (child2 (my-++ child1)))
      (cond ((> child1 to) false)
            ((> child2 to) child1)
            ((< (key (vector-ref heap child1))
                (key (vector-ref heap child2)))
             child1)
            (else child2))))
  (define (iter parent)
    (let ((child (smallest-child parent)))
      (if child
          (cond ((> (key (vector-ref heap parent))
                    (key (vector-ref heap child)))
                 (swap heap child parent)
                 (iter child))))))
  (iter from))

(define (swap a-vector i1 i2)
  (let ((temp (vector-ref a-vector i1)))
    (vector-set! a-vector i1 (vector-ref a-vector i2))
    (vector-set! a-vector i2 temp)))

(define (sift-up heap from)
  (define (iter child)
    (let ((parent (quotient child 2)))
      (if (> parent 0)
          (cond ((> (key (vector-ref heap parent))
                    (key (vector-ref heap child)))
                 (swap heap child parent)
                 (iter parent))))))
  (iter from))

(define (create-heap size)
  (cons 0 (make-vector (my-++ size))))

(define (is-empty? heap)
  (eq? (car heap) 0))

(define (insert heap item)
  (let* ((content (cdr heap))
         (new-nr-of-elements (my-++ (car heap)))
         (size (my--- (vector-length content))))
    (display "insert    ")
    (if (> new-nr-of-elements size)
        false
        (begin
          (vector-set! content new-nr-of-elements item)
          (sift-up content new-nr-of-elements)
          (set-car! heap new-nr-of-elements)))
    (display heap)(newline)))

(define v (vector 'lol 5 8 1 3 9 10 2 0))

(make-heap v 8)
(equal? v (vector 'lol 0 3 1 5 9 10 2 8))
