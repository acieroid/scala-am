;; Ring-buffer benchmark
(define (build-vector n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))

(define (ring-buf size)
  ;; The buffer
  (define buffer (build-vector size (t/ref 0) (lambda (i) (t/ref 0))))
  ;; The read pointer
  (define read (t/ref 0))
  ;; The write pointer
  (define write (t/ref 0))
  ;; Number of elements in the buffer
  (define nelems (t/ref 0))
  ;; Lock protecting the buffer
  (define lock (t/new-lock))
  ;; Push a value in the buffer
  (define (push v)
    (t/acquire lock)
    (t/ref-set (vector-ref buffer (modulo (t/deref write) size)) v)
    (t/ref-set write (+ (t/deref write) 1))
    (t/ref-set nelems (+ (t/deref nelems) 1))
    (t/release lock))
  ;; Pop a value from the buffer
  (define (pop)
    (t/acquire lock)
    (let ((v (t/deref (vector-ref buffer (modulo (t/deref read) size)))))
      (t/ref-set read (+ (t/deref read) 1))
      (t/ref-set nelems (- (t/deref nelems) 1))
      (t/release lock)
      v))
  (list push pop (lambda () (t/deref nelems))))

(define NWR (int-top))
(define NRD (int-top))
(define Iterations (int-top))
(define Size (int-top))
(define (do-n n f)
  (if (= n 0)
      '()
      (cons (f) (do-n (- n 1) f))))

(define buf-struct (ring-buf Size))
(define buf-push (car buf-struct))
(define buf-pop (cadr buf-struct))
(define buf-size (caddr buf-struct))

(define wthreads
  (do-n NWR (lambda ()
                 (letrec ((loop (lambda (i)
                                  (if (= i Iterations)
                                      'done
                                      (begin
                                        (buf-push (random 100))
                                        (loop (+ i 1)))))))
                   (fork (loop 0))))))

(define rthreads
  (do-n NRD (lambda ()
                 (letrec ((loop (lambda (i)
                                  (if (= i Iterations)
                                      'done
                                      (begin
                                        (display (buf-pop))
                                        (newline)
                                        (loop (+ i 1)))))))
                   (fork (loop 0))))))

(map (lambda (t) (join t)) wthreads)
(map (lambda (t) (join t)) rthreads)
(if (= NWR NRD)
    (= (buf-size) 0)
    #t)
