; 5.6
(define (add-to-end e l)
   (if (null? l)
     (cons e '())
     (cons (car l) (add-to-end e (cdr l)))))

(and (equal? (add-to-end 999 '(1 2 3 4 5)) '(1 2 3 4 5 999))
   (equal? (add-to-end 999 '()) '(999))
   (equal? (add-to-end 999 '(1)) '(1 999)))

; 5.7
(and (equal? (append '(1 2 3) '(4 5)) '(1 2 3 4 5))
  (equal? (append '(1 2 3) '()) '(1 2 3))
  (equal? (append '() '(1 2 3)) '(1 2 3))
  (null? (append '() '())))

; 5.14.3
(define (super-merge-n lsts n)

  (define (geef-n+rest lst n)
    (cond ((or (= 0 n) (null? lst)) (cons '() lst))
      (else (let* ((res (geef-n+rest (cdr lst) (- n 1)))
                    (first (car res))
                    (rest (cdr res)))
              (cons (cons (car lst) first) rest)))))

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

; 5.19
(define (compare lijst1 lijst2)
  (cond ((or (null? lijst1) (null? lijst2)) 0)
    ((eq? (car lijst1) (car lijst2))
      (+ 1 (compare (cdr lijst1) (cdr lijst2))))
    (else 0)))

(define (compare-iter lijst1 lijst2)
  (define (loop l1 l2 res)
    (cond ((or (null? l1) (null? l2)) res)
      ((eq? (car l1) (car l2)) (loop (cdr l1) (cdr l2) (+ res 1)))
      (else res)))
  (loop lijst1 lijst2 0))

(define (algemene-compare lijst1 lijst2 test)
  (cond ((or (null? lijst1) (null? lijst2)) 0)
    ((test (car lijst1) (car lijst2))
      (+ 1 (algemene-compare (cdr lijst1) (cdr lijst2) test)))
    (else 0)))

(define (compare-greater lijst1 lijst2)
  (algemene-compare lijst1 lijst2 >))

(and (= (compare '(a b c d e f g) '(a b c x y)) 3)
  (= (compare '(x a b) '(a b c d e f g)) 0)
  (= (compare '(a b c e f g) '(a b)) 2)
  (= (compare-iter '(a b c d e f g) '(a b c x y)) 3)
  (= (compare-iter '(x a b) '(a b c d e f g)) 0)
  (= (compare-iter '(a b c e f g) '(a b)) 2)
  (= (compare-greater '(3 5 6 1 2 5) '(2 1 0 8 5 5)) 3))

; 5.20.4
(define (show namen punten test?)
  (if (null? namen)
      '()
    (let ((res (show (cdr namen) (cdr punten) test?)))
      (if (test? (car punten))
        (cons (car namen) res)
        res))))

(define (one namen punten)
  (define (één-buis? punten)
    (if (null? punten)
      #f
      (let ((punt (car punten))
             (rest (cdr punten)))
        (if (< punt 10)
          (geen-buis? rest)
          (één-buis? rest)))))


  (define (geen-buis? punten)
    (if (null? punten)
      #t
      (let ((punt (car punten))
             (rest (cdr punten)))
        (if (< punt 10)
          #f
          (geen-buis? rest)))))

  (show namen punten één-buis?))

(equal? (one '(wendy dirk kris jan eef)
            '((12 13 15 18) (7 10 14 17) (13 8 7 11)
               (9 12 11 10) (18 14 17 19)))
    '(dirk jan))

; 5.21
(define (comprimeer metingen)
  (define (hulp lst prev count)
    (cond ((null? lst) (list (list prev count)))
      ((= (car lst) prev)
        (hulp (cdr lst) prev (+ count 1)))
      (else (cons (list prev count) (hulp (cdr lst) (car lst) 1)))))
  (if (null? metingen)
      '()
    (hulp (cdr metingen) (car metingen) 1)))

(define (comprimeer-iter metingen)
  (define (hulp lst prev count res)
    (cond ((null? lst) (reverse (cons (list prev count) res)))
      ((= (car lst) prev)
        (hulp (cdr lst) prev (+ count 1) res))
      (else (hulp (cdr lst)
              (car lst)
              1
              (cons (list prev count) res)))))
  (if (null? metingen)
      '()
    (hulp (cdr metingen) (car metingen) 1 '())))

(and (equal? (comprimeer '(37.5 37.5 37.2 38.0 38.0 38.0 38.3))
         '((37.5 2) (37.2 1) (38.0 3) (38.3 1)))
  (equal? (comprimeer-iter '(37.5 37.5 37.2 38.0 38.0 38.0 38.3))
      '((37.5 2) (37.2 1) (38.0 3) (38.3 1))))

; 5.22
;; replaced (apply + l) by (foldr + 0 l) + def of foldr
(define foldr
  (lambda (f base lst)

    (define foldr-aux
      (lambda (lst)
        (if (null? lst)
          base
          (f (car lst) (foldr-aux (cdr lst))))))

    (foldr-aux lst)))

(define (totaal aankopen kortingen)

  (define (zoek-korting kortingen artikel)
    (foldr + 0
      (map
        (lambda (x) (if (eq? (car x) artikel)
                      (cadr x)
                      0)) kortingen)))

  (if (null? aankopen)
    0
    (let* ((aankoop (car aankopen))
            (korting (zoek-korting kortingen (car aankoop)))
            (prijs (cadr aankoop)))
      (+ (- prijs (/ (* prijs korting) 100))
        (totaal (cdr aankopen) (cdr kortingen))))))

(define (totaal-iter aankopen kortingen)

  (define (zoek-korting kortingen artikel)
    (foldr + 0
      (map
        (lambda (x) (if (eq? (car x) artikel)
                      (cadr x)
                      0)) kortingen)))

  (define (loop lst res)
    (if (null? lst)
      res
      (let* ((aankoop (car lst))
              (korting (zoek-korting kortingen (car aankoop)))
              (prijs (cadr aankoop)))
        (loop (cdr lst)
          (+ res (- prijs
                   (/ (* prijs korting) 100)))))))
  (loop aankopen 0))

(define Z&Mkortingen '((jas 50) (kleed 50) (rok 30) (trui 20)))

(and (= (totaal '((jas 100) (trui 25) (rok 70) (t-shirt 20))
            '((jas 50) (kleed 50) (rok 30) (trui 20)))
       139)
  (= (totaal-iter '((jas 100) (trui 25) (rok 70) (t-shirt 20))
         '((jas 50) (kleed 50) (rok 30) (trui 20)))
    139))
