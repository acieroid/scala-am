;; Implementation of Clojure's atoms on top of threads and shared variables
(define foldl
  (lambda (f base lst)

    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
            base
            (foldl-aux (f base (car lst)) (cdr lst)))))

    (foldl-aux base lst)))

(define (atom v)
  (cons (t/ref v)
        (t/new-lock)))

(define (atom-deref a)
  (t/deref (car a)))

(define (atom-swap! a f)
  (t/acquire (cdr a))
  (t/ref-set (car a) (f (t/deref (car a))))
  (t/release (cdr a)))

(define (memoize f)
  (let ((mem (atom '())))
    (lambda (x)
      (let ((e (assoc x (atom-deref mem))))
        (if e
            (cdr e)
            (let ((ret (f x)))
              (atom-swap! mem (lambda (v) (cons (cons x ret) v)))
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


(define N (int-top))

(define (do-n n f)
  (letrec ((loop (lambda (i acc)
                   (if (= i n)
                       acc
                       (loop (+ i 1) (cons (f i) acc))))))
    (loop 0 '())))

(define threads (do-n N (lambda (i) (t/spawn (= (mem-fib i) (mem-fib2 i))))))

(foldl (lambda (a b) (and a b)) #t
       (map (lambda (t) (t/join t)) threads))
