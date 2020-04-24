(define (atom? x)
  (not (pair? x)))

(define VUB-circus '(ann (mien (eef (bas)
                                    (bob))
                               (els (jan)
                                    (jos))
                               (eva (tom)
                                    (tim)))
                         (mies (ine (cas)
                                    (cor))
                               (ils (rik)
                                    (raf))
                               (ines (stef)
                                     (staf)))))



(define (hoofdartiest piramide) (car piramide))
(define (artiesten piramide) (cdr piramide))
(define (artiest? piramide)
  (and (pair? piramide) (atom? (car piramide))))
(define (onderaan? piramide) (null? (cdr piramide)))

(define (jump piramide artiest)
  (define (jump-hulp piramide pad)
    (if (and (artiest? piramide)
             (eq? (hoofdartiest piramide) artiest))
        pad
        (jump-in (artiesten piramide)
                 (cons (hoofdartiest piramide) pad))))

  (define (jump-in lst pad)
    (if (null? lst)
        #f
        (or (jump-hulp (car lst) pad)
            (jump-in   (cdr lst) pad))))
  (reverse (jump-hulp piramide '())))

(define (fall piramide artiest)
  (define (fall-hulp piramide pad)
    (if (and (artiest? piramide)
             (eq? (hoofdartiest piramide) artiest))
        (append pad
                (list (hoofdartiest piramide))
                (map hoofdartiest (artiesten piramide))))
    (fall-in (artiesten piramide)
             (append pad
                     (list (hoofdartiest piramide)))))

  (define (fall-in lst pad)
    (if (null? lst)
        #f
        (or (fall-hulp (car lst) pad)
            (fall-in (cdr lst) pad))))
  (fall-hulp piramide '()))

(and (equal? (jump VUB-circus 'eva) '(ann mien))
     (equal? (jump VUB-circus 'stef) '(ann mies ines))
     (not (or (fall VUB-circus 'eva) (fall VUB-circus 'stef) (fall VUB-circus 'mies))))