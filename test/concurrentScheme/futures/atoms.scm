;; Implementation of Clojure's atoms on top of thrds and shared variables
(define foldl
  (lambda (f base lst)

    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
            base
            (foldl-aux (f base (car lst)) (cdr lst)))))

    (foldl-aux base lst)))

;(define (atom v)
;  (cons (atom v)
;        (new-lock)))

;(define (atom-deref a)
;  (read (car a)))

;(define (atom-swap! a f)
;  (acquire (cdr a))
;  (reset! (car a) (f (read (car a))))
;  (release (cdr a)))

(define (memoize f)
  (let ((mem (atom '())))
    (lambda (x)
      (let ((e (assoc x (read mem))))
        (if e
            (cdr e)
            (let ((ret (f x)))
              (swap! mem (lambda (v) (cons (cons x ret) v)))
              ret))))))

(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define mem-fib (memoize fib))

(define mem-fib2 (memoize (lambda (n)
                            (if (<= n 1)
                                n
                                (+ (mem-fib2 (- n 1)) (mem-fib2 (- n 2)))))))


(define N 42)

(define (do-n n f)
  (letrec ((loop (lambda (i acc)
                   (if (= i n)
                       acc
                       (loop (+ i 1) (cons (f i) acc))))))
    (loop 0 '())))

(define thrds (do-n N (lambda (i) (future (= (mem-fib i) (mem-fib2 i))))))

(foldl (lambda (a b) (and a b)) #t
       (map (lambda (t) (deref t)) thrds))
