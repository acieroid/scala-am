;; Example taken from https://github.com/jensnicolay/abstractmemo
(define (plus n1 n2)
  (lambda (f) (lambda (x) ((n1 f) ((n2 f) x)))))

(define (mult n1 n2)
  (lambda (f) (n2 (n1 f))))

(define (pred n)
  (lambda (f)
    (lambda (x)
      (((n (lambda (g) (lambda (h) (h (g f)))))
        (lambda (ignored) x))
      (lambda (id) id)))))

(define (sub n1 n2)
  ((n2 pred) n1))

(define (church0? n)
  ((n (lambda (x) #f)) #t))

(define (church=? n1 n2)
  (if (church0? n1)
    (church0? n2)
    (if (church0? n2)
      #f
      (church=? (sub n1 church1) (sub n2 church1)))))

(define church0 (lambda (f) (lambda (x) x)))
(define church1 (lambda (f) (lambda (x) (f x))))
(define church2 (lambda (f) (lambda (x) (f (f x)))))
(define church3 (lambda (f) (lambda (x) (f (f (f x))))))

;; multiplication distributes over addition
(church=? (mult church2 (plus church1 church3))
          (plus (mult church2 church1) (mult church2 church3)))
