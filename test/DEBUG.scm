(define (put key1 key2 val) #t)

(define (tree-copy x)
  (if (not (pair? x))
    x
    (cons (tree-copy (car x))
      (tree-copy (cdr x)))))

(define (init n m npats ipats)
  (let ((ipats (tree-copy ipats))) ; Removing this let makes things deterministic...
    (do ((n n (- n 1))
          (i m (cond ((zero? i) m)
                 (else (- i 1))))
          (name 's 's)
          (a '()))
      ((= n 0) a)
      (set! a (cons name a))
      (do ((j (- m i) (- j 1)))
        ((zero? j))
        (put name 's #f)))))

(init 1 1 1 '((a a b b b b a a (b b)) (a (b a) b a)))
