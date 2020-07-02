;; Trapezoidal integral approximation


(define foldl
  (lambda (f base lst)

    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
            base
            (foldl-aux (f base (car lst)) (cdr lst)))))

    (foldl-aux base lst)))

(define NumWorkers 42)
(define Precision (+ 10 42))
(define L 0.0)
(define R (* 1.0 (+ 100 42)))

(define (exp x)
  (expt 2.718281828459045 x))

(define (fx x)
  (let* ((a (sin (- (expt x 3) 1)))
         (b (+ x 1))
         (c (/ a b))
         (d (sqrt (+ 1 (exp (sqrt (* 2 x))))))
         (r (* c d)))
    r))

(define (compute-area l r h)
  (let ((n (inexact->exact (floor (/ (abs (- r l)) h)))))
    (letrec ((loop (lambda (i acc)
                     (if (= i n)
                         acc
                         (let* ((lx (+ (* i h) l))
                                (rx (+ lx h))
                                (ly (fx lx))
                                (ry (fx rx)))
                           (loop (+ i 1) (+ acc (* 0.5 (+ ly ry) h))))))))
      (loop 0 0))))

(define (build-vector n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))

(define (worker l r h)
  (compute-area l r h))

(define (compute l r h)
  (let ((range (/ (- r l) NumWorkers)))
    (letrec ((loop (lambda (i acc)
                     (if (= i NumWorkers)
                         acc
                         (let* ((wl (+ (* range i) l))
                                (wr (+ wl range)))
                           (loop (+ i 1) (cons (future (worker wl wr h)) acc)))))))
      (let* ((thrds (loop 0 '()))
             (results (map (lambda (t) (deref t)) thrds))
             (result (foldl + 0 results)))
        result))))

(compute L R Precision)
