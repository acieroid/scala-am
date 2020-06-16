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
  (define buffer (build-vector size (atom 0) (lambda (i) (atom 0))))
  ;; The read pointer
  (define read (atom 0))
  ;; The write pointer
  (define write (atom 0))
  ;; Number of elements in the buffer
  (define nelems (atom 0))
  ;; Lock protecting the buffer
  (define lock (t/new-lock))
  ;; Push a value in the buffer
  (define (push v)
    (t/acquire lock)
    (reset! (vector-ref buffer (modulo (read write) size)) v)
    (reset! write (+ (read write) 1))
    (reset! nelems (+ (read nelems) 1))
    (t/release lock))
  ;; Pop a value from the buffer
  (define (pop)
    (t/acquire lock)
    (let ((v (read (vector-ref buffer (modulo (read read) size)))))
      (reset! read (+ (read read) 1))
      (reset! nelems (- (read nelems) 1))
      (t/release lock)
      v))
  (list push pop (lambda () (read nelems))))

(define NWR 42)
(define NRD 42)
(define Iterations 42)
(define Size 42)
(define (do-n n f)
  (if (= n 0)
      '()
      (cons (f) (do-n (- n 1) f))))

(define buf-struct (ring-buf Size))
(define buf-push (car buf-struct))
(define buf-pop (cadr buf-struct))
(define buf-size (caddr buf-struct))

(define wthrds
  (do-n NWR (lambda ()
                 (letrec ((loop (lambda (i)
                                  (if (= i Iterations)
                                      'done
                                      (begin
                                        (buf-push (random 100))
                                        (loop (+ i 1)))))))
                   (future (loop 0))))))

(define rthrds
  (do-n NRD (lambda ()
                 (letrec ((loop (lambda (i)
                                  (if (= i Iterations)
                                      'done
                                      (begin
                                        (display (buf-pop))
                                        (newline)
                                        (loop (+ i 1)))))))
                   (future (loop 0))))))

(map (lambda (t) (deref t)) wthrds)
(map (lambda (t) (deref t)) rthrds)
(if (= NWR NRD)
    (= (buf-size) 0)
    #t)
