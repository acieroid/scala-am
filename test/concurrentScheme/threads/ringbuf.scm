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
  (define buffer (build-vector size (ref 0) (lambda (i) (ref 0))))
  ;; The read pointer
  (define read (ref 0))
  ;; The write pointer
  (define write (ref 0))
  ;; Number of elements in the buffer
  (define nelems (ref 0))
  ;; Lock protecting the buffer
  (define lock (new-lock))
  ;; Push a value in the buffer
  (define (push v)
    (acquire lock)
    (ref-set (vector-ref buffer (modulo (deref write) size)) v)
    (ref-set write (+ (deref write) 1))
    (ref-set nelems (+ (deref nelems) 1))
    (release lock))
  ;; Pop a value from the buffer
  (define (pop)
    (acquire lock)
    (let ((v (deref (vector-ref buffer (modulo (deref read) size)))))
      (ref-set read (+ (deref read) 1))
      (ref-set nelems (- (deref nelems) 1))
      (release lock)
      v))
  (list push pop (lambda () (deref nelems))))

(define NWR (random 42))
(define NRD (random 42))
(define Iterations (random 42))
(define Size (random 42))
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
