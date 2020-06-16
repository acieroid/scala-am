(define (atom? x)
  (not (pair? x)))

(define (deep-combine combiner null-value l)
  (cond ((null? l) null-value)
        ((atom? l) l)
        (else (combiner (deep-combine combiner
                                      null-value
                                      (car l))
                        (deep-combine combiner
                                      null-value
                                      (cdr l))))))

(define (deep-map f l)
  (cond
    ((null? l) '())
    ((atom? l) (f l))
    (else (cons (deep-map f (car l))
                (deep-map f (cdr l))))))

(and (= (deep-combine + 0 '((((1 2) (3 4)) ((5 6) (7 8))) 9)) 45)
     (equal? (deep-map (lambda (x) (* x x)) '((((1 . 2) (3 4)) ((5 6) (7 8))) . 9))
             '((((1 . 4) (9 16)) ((25 36) (49 64))) . 81)))