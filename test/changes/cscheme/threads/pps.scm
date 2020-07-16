;; Parallel prefix sum with threads
;; https://stackoverflow.com/questions/10053629/parallel-prefix-sum-fastest-implementation/12874227#12874227
(define (build-vector n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))

(define N (expt 2 (random 42)))
(define input
  (build-vector N #f (lambda (i) (ref i))))

(define (log2 n)
  (inexact->exact (/ (log n) (log 2))))

(define (range a b)
  (letrec ((loop (lambda (i acc)
                   (if (< i a)
                       acc
                       (loop (- i 1) (cons i acc))))))
    (loop (- b 1) '())))

(define (range-down from to)
  (letrec ((loop (lambda (i acc)
                   (if (> i from)
                       acc
                       (loop (+ i 1) (cons i acc))))))
    (loop to '())))

(define (range-offset a b offset)
  (letrec ((loop (lambda (i acc)
                   (if (>= i b)
                       (reverse acc)
                       (loop (+ i offset) (cons i acc))))))
    (loop a '())))


(define (display-vector v)
  (for-each (lambda (i)
              (display (deref (vector-ref v i))) (display " "))
            (range 0 (vector-length v)))
  (newline))

(define (up-sweep-phase v n)
  (define (computation d k)
    (let ((v1 (deref (vector-ref v (- (+ k (expt 2 d)) 1))))
          (v2 (deref (vector-ref v (- (+ k (expt 2 (+ d 1))) 1)))))
      (ref-set (vector-ref v (- (+ k (expt 2 (+ d 1))) 1)) (+ v1 v2))))
  (for-each (lambda (d)
              (for-each (lambda (t) (join t))
                        (map (lambda (k)
                               (fork (computation d k)))
                             (range-offset 0 n (expt 2 (+ d 1))))))
            (range 0 (log2 n))))

(define (down-sweep-phase v n)
  (define (computation d k)
    (let ((t (deref (vector-ref v (- (+ k (expt 2 d)) 1))))
          (v1 (deref (vector-ref v (- (+ k (expt 2 (+ d 1))) 1)))))
      (<change> (begin (ref-set (vector-ref v (- (+ k (expt 2 d)) 1)) v1) ; <===========================================
                       (ref-set (vector-ref v (- (+ k (expt 2 (+ d 1))) 1)) (+ t v1)))
                (begin (ref-set (vector-ref v (- (+ k (expt 2 (+ d 1))) 1)) (+ t v1))
                       (ref-set (vector-ref v (- (+ k (expt 2 d)) 1)) v1)))))
  (ref-set (vector-ref v (- n 1)) 0)
  (for-each (lambda (d)
              (for-each (lambda (t) (join t))
                        (map (lambda (k)
                               (fork (computation d k)))
                             (range-offset 0 n (expt 2 (+ d 1))))))
            (range-down (- (log2 n) 1) 0)))

(define (psum v)
  (let ((n (vector-length v)))
    (up-sweep-phase v n)
    (display-vector v)
    (down-sweep-phase v n)))

(display-vector input)
(psum input)
(display-vector input)
