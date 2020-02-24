(define properties '())
(define (put key1 key2 val) (set! properties (cons (list key1 (cons key2 val)) properties)))
(define *current-gensym* 0)
(define (generate-symbol)
  (set! *current-gensym* (+ *current-gensym* 1))
  (string->symbol (number->string *current-gensym*)))
(define (tree-copy x)
  (if (not (pair? x))
    x
    (cons (tree-copy (car x))
      (tree-copy (cdr x)))))
;;; n is # of symbols
;;; m is maximum amount of stuff on the plist
;;; npats is the number of basic patterns on the unit
;;; ipats is the instantiated copies of the patterns
(define (init n m npats ipats)
  (let ((ipats (tree-copy ipats)))
    (do ((p ipats (cdr p)))
      ((null? (cdr p)) (set-cdr! p ipats)))
    (do ((n n (- n 1))
          (i m (cond ((= 0 i) m)
                 (else (- i 1))))
          (name (generate-symbol) (generate-symbol))
          (a '()))
      ((= n 0) a)
      (set! a (cons name a))
      (do ((i i (- i 1)))
        ((= 0 i))
        (put name (generate-symbol) #f))
      (put name
          'pattern
        (do ((i npats (- i 1))
              (ipats ipats (cdr ipats))
              (a '()))
          ((= 0 i) a)
          (set! a (cons (car ipats) a))))
      (do ((j (- m i) (- j 1)))
        ((= 0 j))
        (put name (generate-symbol) #f)))))
(define database (init 10 3 2 '((a b))))
*current-gensym*