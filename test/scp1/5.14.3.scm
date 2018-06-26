(define (append l m)
  (if (null? l)
      m
      (cons (car l) (append (cdr l) m))))

(define (super-merge-n lsts n)

  (define (geef-n+rest lst n)
    (if (or (= 0 n) (null? lst))
        (cons '() lst)
        (let* ((res (geef-n+rest (cdr lst) (- n 1)))
                       (first (car res))
                       (rest (cdr res)))
                  (cons (cons (car lst) first) rest))))

  (if (null? lsts)
      '()
      (let* ((g-n+rest (geef-n+rest (car lsts) n))
             (first (car g-n+rest))
             (rest (cdr g-n+rest)))
        (append first
                (super-merge-n (append (cdr lsts)
                                       (if (null? rest)
                                           rest
                                           (list rest)))
                               n)))))


(equal? (super-merge-n '((a b c d e f)
                   (g h i j k)
                   (l m n o p q)
                   (r s t u v w))
                 3)
        '(a b c g h i l m n r s t d e f j k o p q u v w))
