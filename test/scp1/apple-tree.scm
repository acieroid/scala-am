(define boom
  '((blad (appel . golden))
    (blad (appel . granny))
    (((appel . golden) blad) blad (appel . cox))))

(define (blad? boom)
  (eq? boom 'blad))

(define (appel? boom)
  (and (pair? boom) (eq? (car boom) 'appel)))

(define (type appel) (cdr appel))

(define (leafs boom)
  (cond  ((null? boom) 0)
         ((blad? boom) 1)
         ((appel? boom) 0)
         (else (+ (leafs (car boom))
                  (leafs (cdr boom))))))

(define (all-apples boom)
  (cond ((null? boom) '())
        ((blad? boom) '())
        ((appel? boom) (list (type boom)))
        (else (append (all-apples (car boom))
                      (all-apples (cdr boom))))))

(define (conditional-append l1 l2)
  (cond
    ((null? l1) l2)
    ((member (car l1) l2)(conditional-append (cdr l1) l2))
    (else (cons (car l1)(conditional-append (cdr l1) l2)))))

(define (apple-types boom)
  (cond ((null? boom) '())
        ((blad? boom) '())
        ((appel? boom) (list (type boom)))
        (else (conditional-append (apple-types (car boom))
                                  (apple-types (cdr boom))))))

(define (bewerk-boom boom doe-blad doe-appel combiner init)
  (cond
    ((null? boom) init)
    ((blad? boom) (doe-blad boom))
    ((appel? boom) (doe-appel boom))
    (else (combiner
           (bewerk-boom (car boom) doe-blad doe-appel combiner init)
           (bewerk-boom (cdr boom) doe-blad doe-appel combiner init)))))

(define (leafs-dmv-bewerk boom)
  (bewerk-boom boom
               (lambda (blad) 1)
               (lambda (appel) 0)
               +
               0))

(define (all-apples-dmv-bewerk boom)
  (bewerk-boom boom
               (lambda(blad) '())
               (lambda(appel) (list (type appel)))
               append
               '()))

(define (apple-types-dmv-bewerk boom)
  (bewerk-boom boom
               (lambda(blad) '())
               (lambda(appel) (list(type  appel)))
               conditional-append
               '()))

(and (= (leafs boom) 4)
     (equal? (all-apples boom) '(golden granny golden cox))
     (equal? (apple-types boom) '(granny golden cox))
     (= (leafs-dmv-bewerk boom) 4)
     (equal? (all-apples-dmv-bewerk boom) '(golden granny golden cox))
     (equal? (apple-types-dmv-bewerk boom) '(granny golden cox)))