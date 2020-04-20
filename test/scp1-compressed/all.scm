; 2.1
(define (sign number)
  (cond ((zero? number) 0)
    ((> number 0) 1)
    (else -1)))

(define (divides? deler deeltal)
  (= 0  (modulo deeltal deler)))

(define (leap-year? year)
  (if (divides? 4 year)
    (if (divides? 100 year)
      (divides? 400 year)
      #t)
    #f))

(define (leap-year2? year)
  (cond ((divides? 400 year) #t)
    ((divides? 100 year) #f)
    ((divides? 4 year) #t)
    (else #f)))

(define (leap-year3? year)
  (if (divides? 400 year)
    #t
    (if (divides? 100 year)
      #f
      (divides? 4 year))))


(define (leap-year4? year)
  (or (divides? 400 year)
    (and (divides? 4 year)
      (not (divides? 100 year)))))


(and (not (or (leap-year? 1989)
            (leap-year? 1900)))
  (leap-year? 2000)
  (= -1 (sign -5))
  (= 1 (sign 17.28))
  (= 0 (sign 0)))

; 2.4
(define (derde-machtswortel x)
  (define epsilon 0.01)
  (define (hulp-derde-machtswortel y)
    (if (< (abs (- (* y y y) x)) epsilon)
      y
      (hulp-derde-machtswortel (/ (+ (/ x (* y y)) y y) 3))))
  (hulp-derde-machtswortel (/ x 3)))

(= 3.000000068671529 (exact->inexact (derde-machtswortel 27)))

; 3.1
(define (1- x) (- x 1))
(define (1+ x) (+ 1 x))

(define (rec-add a b)
  (if (= b 0)
    a
    (1+ (rec-add a (1- b)))))

(define (iter-add a b)
  (cond
    ((= a 0) b)
    ((< a 0) (iter-add (1+ a) (1- b)))
    ((> a 0) (iter-add (1- a) (1+ b)))))

(= 9
  (rec-add 4 5)
  (iter-add 4 5))

; 3.2.1
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (rec-fast-multiply a b)
  (cond ((zero? b) 0)
    ((even? b) (rec-fast-multiply (double a) (halve b)))
    (else (+ a (rec-fast-multiply a (- b 1))))))

(define (iter-fast-multiply a b)
  (define (iter a b acc)
    (cond ((zero? b) acc)
      ((even? b) (iter (double a) (halve b) acc))
      (else (iter a (- b 1) (+ acc a)))))
  (iter a b 0))

(and (= (rec-fast-multiply 3 4) 12)
  (= (rec-fast-multiply 100 200) 20000)
  (= (iter-fast-multiply 3 4) 12)
  (= (iter-fast-multiply 100 200) 20000))

; 3.2
(define (rec-multiply a b)
  (if (zero? b)
    0
    (+ a (rec-multiply a (- b 1)))))

(define (iter-multiply a b)
  (define (iter result counter)
    (if (zero? counter)
      result
      (iter (+ result a) (- counter 1))))
  (iter 0 b))

(= 10
  (rec-multiply 5 2)
  (iter-multiply 5 2))

; 3.3
(define (calc-e-iter n)
  (define (iter ctr res fac-prev)
    (if (> ctr n)
      res
      (let ((new-fac (* ctr fac-prev)))
        (iter (+ ctr 1) (+ res (/ 1 new-fac)) new-fac))))
  (iter 1 1 1))

(define (calc-cos x n)
  (define (iter ctr acc fac xpow sign)
    (if (>= ctr n)
      acc
      (let* ((i (* 2 ctr))
              (newfac (* fac (- i 1) i))
              (newxpow (* xpow x x))
              (newsign (- sign)))
        (iter (+ ctr 1)
          (+ acc (/ (* newsign newxpow) newfac))
          newfac
          newxpow
          newsign))))
  (iter 1 1 1 1 1))

(define (close-to x y)
  (< (abs (- x y)) 0.00000001))

(and (close-to (exact->inexact (calc-e-iter 10)) 2.7182818011463845)
  (close-to (calc-cos 0 10) 1)
  (close-to (calc-cos (/ 3.1415 2) 10) 4.6326794876592664e-05)
  (close-to (calc-cos 3.1415 10) -0.9999999992346591))

; 3.4
(define result '())
(define display (lambda (i) (set! result (cons i result))))

(define (count1 x)
  (cond ((= 0 x) (display x))
    (else (display x)
      (count1 (- x 1)))))

(define (count2 x)
  (cond ((= 0 x) (display x))
    (else (count2 (- x 1))
      (display x))))

(count1 4)
(count2 4)
(equal? result '(4 3 2 1 0 0 1 2 3 4))

; 3.6
(define (for-each f l)
  (if (null? l)
    #t
    (if (pair? l)
      (begin (f (car l)) (for-each f (cdr l)))
      (error "Cannot for-each over a non-list"))))

(define result '())
(define display (lambda (i) (set! result (cons i result))))

(define (weird x)
  (cond
    ((= x 1) 1)
    ((even? x) (weird (/ x 2)))
    (else (weird (+ (* 3 x) 1)))))

(define (depth-weird x)
  (cond
    ((= x 1) 0)
    ((even? x) (+ 1 (depth-weird (/ x 2))))
    (else (+ (depth-weird (+ (* 3 x) 1)) 1))))

(define (weird-table min max)
  (cond
    ((< min max)
      (for-each display (list min "\t" (depth-weird min) "\n"))
      (weird-table (+ min 1) max))))

(weird-table 1 10)

(and (= (weird 15) 1)
  (= (depth-weird 15) 17)
  (equal? result '("\n" 19 "\t" 9 "\n" 3 "\t" 8 "\n" 16 "\t" 7 "\n" 8 "\t" 6 "\n" 5 "\t" 5 "\n" 2 "\t" 4 "\n" 7 "\t" 3 "\n" 1 "\t" 2 "\n" 0 "\t" 1)))

; 3.8
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (sim-multiply a b)
  (if (zero? b)
    1
    (+ 1 (sim-multiply a (- b 1)))))
(define (sim-fast-multiply a b)
  (cond ((zero? b) 1)
    ((even? b) (+ 1 (sim-fast-multiply (double a) (halve b))))
    (else (+ 1 (sim-fast-multiply a (- b 1))))))

(and (= (sim-multiply 14 2365) 2366)
  (= (sim-fast-multiply 14 2365) 19))

; 3.9
(define result '())
(define display (lambda (i) (set! result (cons i result))))
(define newline (lambda () (set! result (cons 'newline result))))

(define (display-n n x)
  (if (> n 0)
    (begin
      (display x)
      (display-n (- n 1) x))))

(define (parasol n)
  (define (triangle i)
    (if (< i n)
      (begin
        (display-n (- n i 1) " ")
        (display-n (+ (* 2 i) 1) "*")
        (newline)
        (triangle (+ i 1)))))

  (define (stick i)
    (if (< i 3)
      (begin
        (display-n (- n 1) " ")
        (display "*")(newline)
        (stick (+ i 1)))))

  (triangle 0)
  (stick 0))

(parasol 10)
(equal? result
    '(newline
       "*"
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       newline
       "*"
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       newline
       "*"
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       newline
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       newline
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       " "
       newline
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       " "
       " "
       newline
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       " "
       " "
       " "
       newline
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       " "
       " "
       " "
       " "
       newline
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       " "
       " "
       " "
       " "
       " "
       newline
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       "*"
       " "
       " "
       " "
       " "
       " "
       " "
       newline
       "*"
       "*"
       "*"
       "*"
       "*"
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       newline
       "*"
       "*"
       "*"
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       newline
       "*"
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       " "
       " "))

; 4.1
(define result '())
(define display (lambda (i) (set! result (cons i result))))
(define newline (lambda () (set! result (cons 'newline result))))

(define (print-abc a b c)
  (display a) (display " ")
  (display b) (display " ")
  (display c) (newline))

(define (foo a b c)
  (print-abc a b c)
  (let ((a 4)
         (c 5)
         (b c))
    (print-abc a b c)
    (let ((b 6)
           (c a))
      (print-abc a b c))
    (let ((a b)
           (c a))
      (print-abc a b c)))
  (print-abc a b c))

(foo 1 2 3)
(equal? result '(newline 3 " " 2 " " 1 newline 4 " " 3 " " 3 newline 4 " " 6 " " 4 newline 5 " " 3 " " 4 newline 3 " " 2 " " 1))

; 4.8
(define (incr x) (+ x 1))
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

(define (simp-int f a b n)
  (let ((h (/ (- b a) n)))
    (define (y k) (f (+ a (* h k))))
    (define (term k)
      (* (if (or (= k 0)(= k n)) 1 (+ 2 (* 2 (modulo k 2))))
        (y k)))
    (/ (* h (sum term 0 incr n)) 3)))

(define r (sqrt 2))
(and (= (simp-int (lambda (x) x) 0 10 100) 50)
  (= (simp-int (lambda (x) (sqrt (- (* r r) (* x x)))) (- r) r 100) 3.1402925778303366))


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

; 7.2
(define (atom? x)
  (not (pair? x)))

(define (depth tree)
  (cond ((null? tree) 0)
    ((atom? tree) 0)
    (else (max (+ 1 (depth (car tree)))
            (depth (cdr tree))))))

(define (leaf-count tree)
  (cond
    ((null? tree) 0)
    ((atom? tree) 1)
    (else (+ (leaf-count (car tree))
            (leaf-count (cdr tree))))))

(define (depth-and-leaf-count tree)
  (define make-res cons)
  (define depth car)
  (define leaf-count cdr)

  (cond
    ((null? tree) (make-res 0 0))
    ((atom? tree) (make-res 0 1))
    (else (let ((res-car (depth-and-leaf-count (car tree)))
                 (res-cdr (depth-and-leaf-count (cdr tree))))
            (make-res (max (+ 1 (depth res-car))
                        (depth res-cdr))
              (+ (leaf-count res-car)
                (leaf-count res-cdr)))))))

(define l '((1 2) ((3 4) 5) (6 7)))
(and (= (depth l) 3)
  (= (leaf-count l) 7)
  (equal? (depth-and-leaf-count l) (cons 3 7)))

; 7.3
(define (atom? x)
  (not (pair? x)))

(define (fringe l)
  (cond ((null? l) '())
    ((atom? l) (list l))
    (else (append (fringe (car l))
            (fringe (cdr l))))))

(equal? (fringe '((1) ((((2)))) (3 (4 5) 6) ((7) 8 9))) '(1 2 3 4 5 6 7 8 9))


; &.4
(define (unfringe-1 l)
  (cond ((null? l) '())
    ((null? (cdr l)) (list (car l)))
    (else (list (car l)
            (unfringe-1 (cdr l))))))

(define (unfringe-2 l)
  (define (pair l)
    (cond ((null? l) '())
      ((null? (cdr l)) (list l))
      (else (cons (list (car l) (cadr l))
              (pair (cddr l))))))

  (let loop ((l l))
    (if (or (null? l)
          (null? (cdr l)))
      l
      (loop (pair l)))))

(and (equal? (unfringe-1 '(1 2 3 4 5 6 7 8 9)) '(1 (2 (3 (4 (5 (6 (7 (8 (9))))))))))
  (equal? (unfringe-2 '(1 2 3 4 5 6 7 8 9)) '(((((1 2) (3 4)) ((5 6) (7 8))) (((9)))))))

; 7.5
(define (atom? x)
  (not (pair? x)))

(define (same-structure? l1 l2)
  (cond ((and (atom? l1) (atom? l2)) #t)
    ((or  (atom? l1) (atom? l2)) #f)
    (else (and (same-structure? (car l1) (car l2))
            (same-structure? (cdr l1) (cdr l2))))))

(define (same-structure?-or l1 l2)
  (or (and (atom? l1) (atom? l2))
    (and (pair? l1)
      (pair? l2)
      (same-structure?-or (car l1) (car l2))
      (same-structure?-or (cdr l1) (cdr l2)))))

(and (same-structure? '((1 2) ((3 . 4) ((5 6) ((7 8) (9)))))
         '((a b) ((c . d) ((e f) ((g h) (i))))))
  (not (same-structure? '((1 2) ((3 4) ((5 6) ((7 8) (9)))))
           '((((1 2) (3 4)) ((5 6) (7 8))) 9))))

; 7.6
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

; 7.9
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

; 7.11
(define organigram
    '(directeur
       (hoofd-verkoop (verkoopsleider-vlaanderen)
         (verkoopsleider-brussel))
       (hoofd-productie (hoofd-inkoop (bediende1)
                          (bediende2)
                          (bediende3))
         (hoofd-fakturen))
       (hoofd-administratie (hoofd-personeel)
         (hoofd-boekhouding))))

(define (baas organigram) (car organigram))
(define (sub-organigrammen organigram) (cdr organigram))

(define (hierarchisch? p1 p2 organigram)
  (define (hierarchisch?-in path organigrammen)
    (if (null? organigrammen)
      #f
      (or (hierarchisch? path (car organigrammen))
        (hierarchisch?-in path (cdr organigrammen)))))

  (define (hierarchisch? path organigram)
    (cond
      ((and (eq? p1 (baas organigram)) (member p2 path)) #t)
      ((and (eq? p2 (baas organigram)) (member p1 path)) #t)
      (else (hierarchisch?-in (cons (baas organigram) path)
              (sub-organigrammen organigram)))))
  (hierarchisch? '() organigram))

(define (collegas p organigram)
  (define (collegas-in oversten organigrammen)
    (if (null? organigrammen)
      #f
      (or (collegas oversten (car organigrammen))
        (collegas-in oversten (cdr organigrammen)))))

  (define (werknemers-in organigrammen)
    (if (null? organigrammen)
        '()
      (append (werknemers (car organigrammen))
        (werknemers-in (cdr organigrammen)))))

  (define (werknemers organigram)
    (cons (baas organigram)
      (werknemers-in (sub-organigrammen organigram))))

  (define (collegas oversten organigram)
    (if (eq? p (baas organigram))
      (append oversten
        (werknemers-in (sub-organigrammen organigram)))
      (collegas-in (cons (baas organigram) oversten)
        (sub-organigrammen organigram))))
  (collegas '() organigram))

(and (hierarchisch? 'directeur 'verkoopsleider-brussel organigram)
  (hierarchisch? 'bediende1 'hoofd-productie organigram)
  (not (hierarchisch? 'hoofd-personeel 'bediende3 organigram))
  (equal? (collegas 'hoofd-inkoop organigram) '(hoofd-productie directeur bediende1 bediende2 bediende3)))

; 7.12
(define (atom? x)
  (not (pair? x)))

(define mijn-vuurwerk '(groen ((blauw (X (blauw (X X)) X X))
                                (rood ((groen (X X)) X))
                                X
                                (geel (X X)))))

(define (kleur vuurwerk) (car vuurwerk))
(define (takken vuurwerk) (cadr vuurwerk))
(define (low-energy? vuurwerk) (eq? vuurwerk 'X))

(define (tel-knallen vuurwerk)
  (cond ((null? vuurwerk) 0)
    ((low-energy? vuurwerk) 0)
    ((atom? vuurwerk) 1)
    (else (+ (tel-knallen (car vuurwerk))
            (tel-knallen (cdr vuurwerk))))))

(define (tel-low-energies v)
  (cond ((null? v) 0)
    ((low-energy? v) 1)
    ((atom? v) 0)
    (else (+ (tel-low-energies (car v))
            (tel-low-energies (cdr v))))))

(define (tel-einde-in takken een-kleur)
  (cond ((null? takken) 0)
    ((low-energy? (car takken)) 0)
    (else (+ (tel-einde (car takken) een-kleur)
            (tel-einde-in (cdr takken) een-kleur)))))

(define (tel-einde vuurwerk een-kleur)
  (if (eq? (kleur vuurwerk) een-kleur)
    (tel-low-energies (takken vuurwerk))
    (tel-einde-in (takken vuurwerk) een-kleur)))

(define (ster? vuurwerk)
  (not (member 'X (takken vuurwerk))))

(and (eq? (kleur mijn-vuurwerk) 'groen)
  (equal? (takken mijn-vuurwerk)
      '((blauw (X (blauw (X X)) X X)) (rood ((groen (X X)) X)) X (geel (X X))))
  (not (low-energy? mijn-vuurwerk))
  (low-energy? 'X)
  (= (tel-knallen mijn-vuurwerk) 6)
  (= (tel-einde mijn-vuurwerk 'blauw) 5)
  (not (ster? mijn-vuurwerk)))

; 7.13
(define result '())
(define display2 (lambda (i) (set! result (cons i result))))
(define newline2 (lambda () (set! result (cons 'newline result))))

(define VUBOrganigram
    '(VUB (academisch (rectoraat)
            (faculteiten
              (rechten (bachelor (ba-rechten)
                         (ba-criminologie))
                (master (ma-rechten)
                  (ma-criminologie)))
              (economie)
              (wetenschappen (bachelor (ba-wiskunde)
                               (ba-fysica)
                               (ba-cw))
                (master (ma-wiskunde)
                  (ma-fysica)
                  (ma-cw)))))
       (administratief (personeel) (financien))))

(define (display-n n d)
  (if (> n 0)(begin (display2 d)(display-n (- n 1) d))))

(define (print-lijn aantalblanco tekst)
  (display-n aantalblanco " ")
  (display2 tekst)
  (newline2))

(define (label organigram)  (car organigram))
(define (takken organigram) (cdr organigram))

(define (organigram-member-in een-label organigrammen)
  (if (null? organigrammen)
    #f
    (or (organigram-member een-label (car organigrammen))
      (organigram-member-in een-label (cdr organigrammen)))))

(define (organigram-member een-label organigram)
  (if (eq? een-label (label organigram))
    organigram
    (organigram-member-in een-label (takken organigram))))

(define (print organigram)
  (define (print diepte organigram)
    (print-lijn diepte (label organigram))
    (for-each (lambda (organigram)
                (print (+ diepte 1) organigram))
      (takken organigram)))
  (print 0 organigram))

(define (print-vanaf organigram label)
  (let ((res (organigram-member label organigram)))
    (if res
      (print res)
      #f)))

(print-vanaf VUBOrganigram 'rechten)

(define (print-tot organigram niveau)
  (define (print-tot organigram niveau max-niveau)
    (cond ((<= niveau max-niveau)
            (print-lijn niveau (label organigram))
            (for-each
              (lambda (organigram)
                (print-tot organigram (+ niveau 1) max-niveau))
              (takken organigram)))))
  (print-tot organigram 0 niveau))

(print-tot VUBOrganigram 2)
(equal? result
    '(newline
       financien
       " "
       " "
       newline
       personeel
       " "
       " "
       newline
       administratief
       " "
       newline
       faculteiten
       " "
       " "
       newline
       rectoraat
       " "
       " "
       newline
       academisch
       " "
       newline
       VUB
       newline
       ma-criminologie
       " "
       " "
       newline
       ma-rechten
       " "
       " "
       newline
       master
       " "
       newline
       ba-criminologie
       " "
       " "
       newline
       ba-rechten
       " "
       " "
       newline
       bachelor
       " "
       newline
       rechten))

; 7.14
(define (atom? x)
  (not (pair? x)))

(define (maak-dier naam eigenschappen)
  (list naam eigenschappen))

(define (naam dier) (car dier))
(define (eigenschappen dier) (cadr dier))

(define (dier? dier)
  (and (pair? dier)
    (atom? (naam dier))
    (pair? (eigenschappen dier))))

(define (maak-boom knoop deelbomen)
  (list knoop deelbomen))

(define (knoop boom) (car boom))
(define (deelbomen boom) (cadr boom))
(define (leeg? boom) (null? boom))
(define (knoop? boom) (dier? boom))


(define classificatieboom
  (maak-boom (maak-dier 'dier '(kan-ademen kan-bewegen))
    (list
      (maak-boom
        (maak-dier 'vis
            '(kan-zwemmen heeft-schubben heeft-vinnen))
        (list
          (maak-dier 'ballonvis
              '(kan-zwellen is-geel))))
      (maak-boom
        (maak-dier 'landdier
            '(heeft-huid kan-lopen heeft-poten))
        (list (maak-dier 'olifant
                  '(is-groot))))
      (maak-boom
        (maak-dier 'vogel
            '(kan-vliegen heeft-vleugels heeft-veren))
        (list
          (maak-dier 'kanarie
              '(kan-zingen is-geel))
          (maak-dier 'arend
              '(is-groot)))))))

(define (all-kinds boom)
  (cond ((leeg? boom) '())
    ((dier? boom) (list (naam boom)))
    ((dier? (knoop boom))
      (append (list (naam (knoop boom)))
        (all-kinds-in (deelbomen boom))))
    (else (all-kinds-in (deelbomen boom)))))

(define (all-kinds-in lst)
  (if (null? lst)
      '()
    (append (all-kinds (car lst))
      (all-kinds-in (cdr lst)))))

(define (geef-eigenschappen boom soort)
  (define (geef-eig boom eig)
    (cond ((dier? boom)
            (if (eq? (naam boom) soort)
              (append eig
                (list (eigenschappen boom)))
              #f))
      ((and (dier? (knoop boom))
         (eq? (naam (knoop boom)) soort))
        (append eig (eigenschappen (knoop boom))))
      (else (geef-eig-in (deelbomen boom)
              (append eig
                (eigenschappen (knoop boom)))))))

  (define (geef-eig-in lst eig)
    (cond ((null? lst) #f)
      (else (or (geef-eig (car lst) eig)
              (geef-eig-in (cdr lst) eig)))))
  (geef-eig boom '()))

(define (ask? boom soort eig)
  (let ((eigenschappen (geef-eigenschappen boom soort)))
    (pair? (memq eig eigenschappen))))

(and (equal? (all-kinds classificatieboom)
         '(dier vis ballonvis landdier olifant vogel kanarie arend))
  (ask? classificatieboom 'landdier  'kan-lopen)
  (ask? classificatieboom 'ballonvis 'heeft-vinnen)
  (not (ask? classificatieboom 'olifant   'kan-vliegen)))

; 7.15
(define (atom? x)
  (not (pair? x)))

(define (maak-blad type) type)
(define (geef-type blad) blad)

(define (maak-knoop deelbomen) deelbomen)
(define (geef-deelbomen boom) boom)

(define (maak-hybride-tak knopen) knopen)
(define (geef-knopen tak) tak)

(define (leeg? boom) (null? boom))
(define (knoop? boom) (pair? boom))
(define (blad? boom) (atom? boom))

(define hybride-tak
  (maak-hybride-tak
    (list
      (maak-knoop
        (list
          (maak-knoop (list (maak-blad 'appel)
                        (maak-blad 'appel)
                        (maak-blad 'blad)))
          (maak-blad 'peer)))
      (maak-knoop (list (maak-blad 'blad)
                    (maak-blad 'peer)))
      (maak-knoop (list (maak-blad 'appel)
                    (maak-knoop (list (maak-blad 'appel)
                                  (maak-blad 'blad))))))))

(define tak
  (maak-hybride-tak
    (list
      (maak-knoop
        (list (maak-knoop (list (maak-blad 'appel)
                            (maak-blad 'appel)
                            (maak-blad 'blad)))
          (maak-blad 'peer)))
      (maak-knoop (list (maak-blad 'blad)
                    (maak-blad 'peer)
                    (maak-blad 'appel)))
      (maak-knoop (list (maak-blad 'appel)
                    (maak-knoop (list (maak-blad 'appel)
                                  (maak-blad 'blad))))))))

(define (tel boom)
  (define (combine-results l1 l2)
    (list (+ (car l1) (car l2))
      (+ (cadr l1) (cadr l2))
      (+ (caddr l1) (caddr l2))))

  (define (tel-hulp boom)
    (cond ((leeg? boom) (list 0 0 0))
      ((and (blad? boom) (eq? boom 'appel))
        (list 1 0 0))
      ((and (blad? boom) (eq? boom 'peer))
        (list 0 1 0))
      ((blad? boom) (list 0 0 1))
      (else (tel-hulp-in (geef-knopen boom)))))

  (define (tel-hulp-in lst)
    (if (null? lst)
      (list 0 0 0)
      (combine-results (tel-hulp (car lst))
        (tel-hulp-in (cdr lst)))))
  (tel-hulp boom))

(define (member? x lst)
  (pair? (memq x lst)))

(define (normaal? knoop)
  (let ((types (map (lambda (x) (if (pair? x) 'tak x)) knoop)))
    (not (and (member? 'appel types) (member? 'peer types)))))

(define (check-normaal boom)
  (cond ((leeg? boom) #t)
    ((blad? boom) #t)
    ((knoop? boom)
      (and (normaal? boom)
        (check-normaal-in (geef-knopen boom))))
    (else (check-normaal-in (geef-knopen boom)))))

(define (check-normaal-in lst)
  (if (null? lst)
    #t
    (and (check-normaal (car lst))
      (check-normaal-in (cdr lst)))))

(and (equal? (tel hybride-tak) '(4 2 3))
  (check-normaal hybride-tak))

; 7.16
(define foldr ; replace (apply + ...) by (foldr + 0 ...)
  (lambda (f base lst)
    (define foldr-aux
      (lambda (lst)
        (if (null? lst)
          base
          (f (car lst) (foldr-aux (cdr lst))))))
    (foldr-aux lst)))

(define (atom? x)
  (not (pair? x)))

(define Coca-Cola-NV
    '(Coca-Cola-NV (Frisdranken
                     (Coca-Cola
                       (Regular-Coca-Cola (Coke (10000000)))
                       (light-Coca-Cola (Coke-Light (800000))
                         (Coke-Zero (200000))))
                     (Fanta (Fanta-Orange (800000))
                       (Fanta-Lemon (200000)))
                     (Sprite (Sprite-Zero (1000000))))
       (Sappen
         (Minute-Maid (Minute-Maid-Sinaas (2000000))
           (Minute-Maid-Tomaat (1000000))))))

(define (omzetcijfer categorie)
  (caadr categorie))

(define (heeft-omzetcijfer categorie)
  (and (pair? categorie)
    (pair? (cadr categorie))
    (atom? (caadr categorie))
    (number? (caadr categorie))))

(define (deel-categorien categorie)
  (cdr categorie))

(define (hoofdcategorie categorie)
  (car categorie))

(define (bereken lst)
  (cond ((null? lst) 0)
    ((atom? lst) 0)
    ((number? (car lst)) (car lst))
    (else (+ (bereken (car lst))
            (bereken (cdr lst))))))

(define (omzet bedrijf categorie)
  (if (eq? (hoofdcategorie bedrijf) categorie)
    (bereken bedrijf)
    (omzet-in (deel-categorien bedrijf) categorie)))

(define (omzet-in lst categorie)
  (if (null? lst)
    #f
    (or (omzet (car lst) categorie)
      (omzet-in (cdr lst) categorie))))

(define (collect-pairs bedrijf)
  (cond ((heeft-omzetcijfer bedrijf)
          (list (list (hoofdcategorie bedrijf)
                  (omzetcijfer bedrijf))))
    (else (collect-pairs-in (deel-categorien bedrijf)))))

(define (collect-pairs-in lst)
  (if (null? lst)
      '()
    (append (collect-pairs (car lst))
      (collect-pairs-in (cdr lst)))))

(define (verdeel-democratisch bedrijf budget)
  (let* ((pairs (collect-pairs bedrijf))
          (total (foldr + 0 (map cadr pairs)))
          (factor (/ budget total)))
    (map (lambda (x) (list (car x) (* factor (cadr x))))
      pairs)))

(define (verdeel bedrijf budget)
  (if (heeft-omzetcijfer bedrijf)
    (list (hoofdcategorie bedrijf) budget)
    (let* ((rest (deel-categorien bedrijf))
            (new-budget (/ budget (length rest))))
      (cons (hoofdcategorie bedrijf)
        (verdeel-in rest new-budget)))))

(define (verdeel-in lst budget)
  (if (null? lst)
      '()
    (cons (verdeel (car lst) budget)
      (verdeel-in (cdr lst) budget))))

(and (= (omzet Coca-Cola-NV 'Coca-Cola) 11000000)
  (= (omzet Coca-Cola-NV 'Sprite) 1000000)
  (= (omzet Coca-Cola-NV 'Minute-Maid) 3000000)
  (equal? (verdeel-democratisch Coca-Cola-NV 128000000) '((Coke 80000000) (Coke-Light 6400000) (Coke-Zero 1600000) (Fanta-Orange 6400000) (Fanta-Lemon 1600000) (Sprite-Zero 8000000) (Minute-Maid-Sinaas 16000000) (Minute-Maid-Tomaat 8000000)))
  (equal? (verdeel Coca-Cola-NV 1200000) '(Coca-Cola-NV (Frisdranken (Coca-Cola (Regular-Coca-Cola (Coke 100000)) (light-Coca-Cola (Coke-Light 50000) (Coke-Zero 50000))) (Fanta (Fanta-Orange 100000) (Fanta-Lemon 100000)) (Sprite (Sprite-Zero 200000))) (Sappen (Minute-Maid (Minute-Maid-Sinaas 300000) (Minute-Maid-Tomaat 300000))))))

; 7.17
(define familieboom '(jan (piet (frans (tom)
                                  (roel))
                            (mie))
                       (bram (inge (bert (ina)
                                     (ilse))
                               (bart))
                         (iris))
                       (joost (else (ilse)))))



(define (familiehoofd fam) (car fam))
(define (kinderen fam) (cdr fam))
(define (laatste-nakomeling? fam)
  (null? (kinderen fam)))

(define (verdeel-democratisch boom budget)
  (define (verdeel boom)
    (if (laatste-nakomeling? boom)
      1
      (+ 1 (verdeel-in (kinderen boom)))))

  (define (verdeel-in lst)
    (if (null? lst)
      0
      (+ (verdeel (car lst))
        (verdeel-in (cdr lst)))))
  (/ budget (verdeel-in (kinderen boom))))

(define (budget boom budget-list)
  (define (budget-hulp boom budget-list)
    (+ (car budget-list)
      (budget-hulp-in (kinderen boom) (cdr budget-list))))

  (define (budget-hulp-in bomen budget-list)
    (if (or (null? bomen)(null? budget-list))
      0
      (+ (budget-hulp    (car bomen) budget-list)
        (budget-hulp-in (cdr bomen) budget-list))))
  (budget-hulp-in (kinderen boom) budget-list))

(define (verdeel boom budget)
  (cond ((laatste-nakomeling? boom)
          (list (list (familiehoofd boom) budget)))
    (else (let* ((rest (kinderen boom))
                  (new-budget (/ budget (length rest))))
            (verdeel-in rest new-budget)))))

(define (verdeel-in bomen budget)
  (if (null? bomen)
      '()
    (append (verdeel    (car bomen) budget)
      (verdeel-in (cdr bomen) budget))))

(and (= (verdeel-democratisch familieboom 1500) 100)
  (= (budget familieboom '(100 50 20)) 650)
  (equal? (verdeel familieboom 3000) '((tom 250) (roel 250) (mie 500) (ina 125) (ilse 125) (bart 250) (iris 500) (ilse 1000))))

; 7.18
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

; 8.1.1
(define flip
  (let ((state 0))
    (lambda ()
      (if (= state 0)
        (set! state 1)
        (set! state 0))
      state)))

(and (= (flip) 1)
  (= (flip) 0)
  (= (flip) 1)
  (= (flip) 0))

; 8.1.3
(define (make-flip)
  (let ((state 0))
    (lambda ()
      (if (= state 0)
        (set! state 1)
        (set! state 0))
      state)))

(define flip (make-flip))

(and (= (flip) 1)
  (= (flip) 0)
  (= (flip) 1)
  (= (flip) 0))

; 8.5
(define (make-point x y)

  (define (dispatch msg)
    (cond ((eq? msg 'x-value) x)
      ((eq? msg 'y-value) y)
      (else (error "wrong message"))))
  dispatch)

(define (make-segment start end)

  (define (midpoint)
    (make-point (/ (+ (start 'x-value) (end 'x-value)) 2)
      (/ (+ (start 'y-value) (end 'y-value)) 2)))

  (define (dispatch msg)
    (cond ((eq? msg 'start-point) start)
      ((eq? msg 'end-point) end)
      ((eq? msg 'midpoint) (midpoint))
      (else (error "wrong message"))))
  dispatch)


(define (make-w-vector . args)

  (define (dimension)
    (length args))

  (define (coordinate n)
    (if (or (< n 1) (> n (dimension)))
      (error "coordinate is out of range")
      (list-ref args (- n 1))))

  (define (add w-vector)
    (define (loop ctr res)
      (if (= ctr 0)
        (apply make-w-vector res)
        (loop (- ctr 1) (cons (+ (coordinate ctr)
                                ((w-vector 'coordinate) ctr))
                          res))))
    (loop (dimension) '()))



  (define (dispatch msg)
    (cond ((eq? msg 'dimension) (dimension))
      ((eq? msg 'coordinate) coordinate)
      ((eq? msg 'add) add)
      (else (error "wrong message"))))
  dispatch)


(define (make-polynome . coefficients)
  (let ((polynome (apply make-w-vector coefficients)))

    (define (coefficient index)
      ((polynome 'coordinate) index))

    (define (order)
      (- (polynome 'dimension) 1))

    (define (dispatch msg)
      (cond ((eq? msg 'order) (order))
        ((eq? msg 'coefficient) coefficient)
        (else (error "wrong message"))))
    dispatch))

(define point1 (make-point 6 10))
(define point2 (make-point 10 20))
(define segment (make-segment point1 point2))
(define midpoint (segment 'midpoint))
(define w-vector1 (make-w-vector 1 2 3))
(define w-vector2 (make-w-vector 4 5 6))
(define polynome (make-polynome 1 2 3))

(and (= (point1 'x-value) 6)
  (= ((segment 'start-point) 'y-value) 10)
  (= (midpoint 'x-value) 8)
  (= ((w-vector1 'coordinate) 2) 2)
  (= ((w-vector2 'coordinate) 1) 4)
  (= ((((w-vector1 'add) w-vector2) 'coordinate) 1) 5)
  (= (polynome 'order) 2)
  (= ((polynome 'coefficient) 2) 2))

; 8.6
(define result '())
(define display (lambda (i) (set! result (cons i result))))

(define hulp 2)
(define (haha x)
  (let ((hulp (* x hulp)))
    (display hulp))
  (display hulp)
  (set! hulp 4))

(haha 2)
(haha 3)
(equal? result '(4 12 2 4))

; 8.10
(define result '())
(define display (lambda (i) (set! result (cons i result))))
(define newline (lambda () (set! result (cons 'newline result))))

(define (create-counter)
  (let ((value 0))

    (define (reset)
      (set! value 0)
        'ok)

    (define (next)
      (set! value (+ 1 value))
        'ok)

    (define (increase x)
      (set! value (+ value x)))

    (define (dispatch msg)
      (cond ((eq? msg 'reset) reset)
        ((eq? msg 'next) next)
        ((eq? msg 'read) value)
        ((eq? msg 'increase) increase)
        (else (error "wrong message: " msg))))
    dispatch))


(define (make-scorebord)
  (let ((c-home (create-counter))
         (c-visit (create-counter)))

    (define (reset)
      ((c-home 'reset))
      ((c-visit 'reset))
        'ok)

    (define (read)
      (let ((c1 (c-home 'read))
             (c2 (c-visit 'read)))
        (display c1)
        (display "-")
        (display c2)
        (newline)
          'ok))

    (define (score team n)
      (cond ((not (or (= n 1) (= n 2) (= n 3)))
              (newline)
              (display "De score kan slechts 1, 2 of 3 zijn!")
              (newline)
                'ok)
        ((eq? team 'home)
          ((c-home 'increase) n)
            'ok)
        ((eq? team 'visit)
          ((c-visit 'increase) n)
            'ok)
        (else (error "wrong team: " team))))

    (define (dispatch msg)
      (cond ((eq? msg 'reset) reset)
        ((eq? msg 'read) read)
        ((eq? msg 'score) score)
        (else (error "wrong message: " msg))))
    dispatch))

(define bord (make-scorebord))
((bord 'read))
((bord 'score) 'home 2)
((bord 'read))
((bord 'score) 'visit 5)
((bord 'read))
((bord 'reset))
((bord 'read))
(equal? result '(newline 0 "-" 0 newline 0 "-" 2 newline "De score kan slechts 1, 2 of 3 zijn!" newline newline 0 "-" 2 newline 0 "-" 0))

; 8.11
(define (create-counter initial)
  (define (increase) (set! initial (+ initial 1)))
  (define (decrease) (set! initial (- initial 1)))
  (define (read) initial)
  (define (dispatch m)
    (cond ((eq? m 'increase) (increase))
      ((eq? m 'decrease) (decrease))
      ((eq? m 'read) (read))
      (else (display "wrong message"))))
  dispatch)

(define (create-parking . capaciteiten)
  (let ((verdieping-ctrs (map create-counter capaciteiten))
         (nr-verdiepingen (length capaciteiten))
         (nbr-cars 0))

    (define (total-capacity)
      (apply + capaciteiten))

    (define (full?)
      (= nbr-cars (total-capacity)))

    (define (empty?)
      (= nbr-cars 0))

    (define (max-reached-level level idx)
      (>=  (level 'read) (list-ref capaciteiten (- idx 1))))

    (define (level-current)
      (define (loop lst index)
        (cond ((null? lst) #f)
          (else (let* ((level (car lst))
                        (capacity (level 'read)))
                  (if (> capacity 0)
                    index
                    (loop (cdr lst) (+ index 1)))))))
      (loop verdieping-ctrs 1))

    (define (level-to-leave)
      (define (loop lst index)
        (cond ((null? lst) #f)
          (else (let* ((level (car lst))
                        (capacity (level 'read)))
                  (if (and (not (max-reached-level level index)) (>= capacity 0))
                    index
                    (loop (cdr lst) (- index 1)))))))
      (loop (reverse verdieping-ctrs) nr-verdiepingen))

    (define (car-enters)
      (let ((level (level-current)))
        (if level
          (let ((verdieping-ctr (list-ref verdieping-ctrs
                                  (- level 1))))
            (set! nbr-cars (+ nbr-cars 1))
            (verdieping-ctr 'decrease))
          #f)))

    (define (car-leaves)
      (let ((level (level-to-leave)))

        (if level
          (let ((verdieping-ctr (list-ref verdieping-ctrs (- level 1))))
            (set! nbr-cars (- nbr-cars 1))
            (verdieping-ctr 'increase))
          (let ((verdieping-ctr (list-ref verdieping-ctrs(- nr-verdiepingen 1))))
            (set! nbr-cars (- nbr-cars 1))
            (verdieping-ctr 'increase)))))

    (define (dispatch msg)
      (cond ((eq? msg 'full?) (full?))
        ((eq? msg 'empty?) (empty?))
        ((eq? msg 'level) (level-current))
        ((eq? msg 'car-enters) (car-enters))
        ((eq? msg 'lst) verdieping-ctrs)
        ((eq? msg 'car-leaves) (car-leaves))
        (else (error "wrong message"))))
    dispatch))

(define parking (create-parking 3 5 2))
(and (= (parking 'level) 1)
  (not (parking 'full?))
  (= (begin (parking 'car-enters)
       (parking 'car-enters)
       (parking 'car-enters)
       (parking 'car-enters)
       (parking 'level))
    2)
  (not (parking 'empty?))
  (begin (parking 'car-enters)
    (parking 'car-enters)
    (parking 'car-enters)
    (parking 'car-enters)
    (parking 'car-enters)
    (parking 'car-enters)
    (parking 'full?))
  (not (parking 'car-enters))
  (= (begin (parking 'car-leaves)
       (parking 'car-leaves)
       (parking 'car-leaves)
       (parking 'level))
    2))

; 8.12
(define (maak-rechthoek l b)
  (define (oppervlakte) (* l b))
  (define (omtrek) (* 2 (+ l b)))
  (define (dispatch m)
    (cond ((eq? m 'oppervlakte) (oppervlakte))
      ((eq? m 'omtrek) (omtrek))))
  dispatch)

(define (maak-vierkant zijde)
  (define rechthoek (maak-rechthoek zijde zijde))
  (define (schaal! n) (set! zijde (* n zijde)))
  (define (dispatch m)
    (cond ((eq? m 'oppervlakte) (rechthoek 'oppervlakte))
      ((eq? m 'omtrek) (rechthoek 'omtrek))
      ((eq? m 'schaal!) schaal!)))
  dispatch)

(define test (maak-vierkant 5))
(and (= (test 'oppervlakte) 25)
  (= (test 'omtrek) 20)
  (= (begin ((test 'schaal!) 2)
       (test 'oppervlakte))
    25))

; 8.13
(define (MaakLampje aantal)
  (define state 'off)

  (define (on!) (set! state 'on))

  (define (off!) (set! state 'off))

  (define (broken!) (set! state 'broken))

  (define (on?) (eq? state 'on))

  (define (off?) (eq? state 'off))

  (define (broken?) (eq? state 'broken))

  (define (switch!)
    (set! aantal (- aantal 1))
    (cond ((< aantal 0) (broken!))
      ((off?) (on!))
      ((on?) (off!)))
    (not (broken?)))

  (define (change! nieuw)
    (off!)
    (set! aantal nieuw)
      'changed)

  (define (dispatch msg)
    (cond ((eq? msg 'switch!) (switch!))
      ((eq? msg 'on?) (on?))
      ((eq? msg 'off?) (off?))
      ((eq? msg 'test?) (broken?))
      ((eq? msg 'change!) change!)
      (else (error "Message not understood."))))
  dispatch)

(define philips (MaakLampje 5))
(and (not (philips 'test?))
  (not (philips 'on?))
  (philips 'off?)
  (philips 'switch!)
  (philips 'switch!)
  (philips 'switch!)
  (philips 'switch!)
  (philips 'switch!)
  (not (philips 'switch!))
  (philips 'test?)
  (begin ((philips 'change!) 10)
    (not (philips 'test?)))
  (philips 'off?))

; 8.14
(define (maak-teller)
  (let ((result 0))

    (define (toets bedrag)
      (set! result (+ result bedrag)))

    (define (reset)
      (set! result 0))

    (define (dispatch msg)
      (cond ((eq? msg 'toets) toets)
        ((eq? msg 'lees) result)
        ((eq? msg 'reset) (reset))
        (else (error "wrong message"))))
    dispatch))

(define (maak-winkelkassa)
  (let ((saldo (maak-teller))
         (te-betalen (maak-teller))
         (ingetoetst 'product)
         (ontvangen 0))

    (define (toets type bedrag)
      (set! ingetoetst type)
      (cond  ((eq? type 'product)
               ((te-betalen 'toets) bedrag))
        ((eq? type 'ontvangen)
          (set! ontvangen bedrag))
        (else (error "wrong type"))))

    (define (enter)
      (if (eq? ingetoetst 'product)
        (te-betalen 'lees)
        (let ((wisselgeld (- ontvangen (te-betalen 'lees))))
          ((saldo 'toets) (te-betalen 'lees))
          (te-betalen 'reset)
          wisselgeld)))

    (define (inhoud)
      (saldo 'lees))

    (define (afsluiten)
      (let ((teruggeven saldo))
        (set! saldo 0)
        teruggeven))

    (define (dispatch msg)
      (cond ((eq? msg 'toets) toets)
        ((eq? msg 'enter) (enter))
        ((eq? msg 'inhoud) (inhoud))
        ((eq? msg 'afsluiten) (afsluiten))
        (else (error "wrong message"))))
    dispatch))

(define teller (maak-teller))
(define winkelkassa (maak-winkelkassa))
((winkelkassa 'toets) 'product 20)
((teller 'toets) 20)
((winkelkassa 'toets) 'product 5)
(and (= (teller 'lees) 20)
  (begin (teller 'reset)
    (= (teller 'lees) 0))
  (= (winkelkassa 'enter) 25)
  (= (begin ((winkelkassa 'toets) 'product 10)
       (winkelkassa 'enter))
    35)
  (begin ((winkelkassa 'toets) 'ontvangen 50)
    (= (winkelkassa 'enter) 15))
  (= (winkelkassa 'inhoud) 35))

; 8.15
(define foldr
  (lambda (f base lst)
    (define foldr-aux
      (lambda (lst)
        (if (null? lst)
          base
          (f (car lst) (foldr-aux (cdr lst))))))
    (foldr-aux lst)))
(define result '())
(define display2 (lambda (i) (set! result (cons i result))))
(define newline2 (lambda () (set! result (cons 'newline result))))
(define error2 (lambda (e) (set! result (cons (list 'error e) result))))


(define (maak-buffer)
  (let ((inhoud '()))

    (define (newValue value)
      (set! inhoud (append inhoud (list value))))

    (define (returnSum)
      (foldr + 0 inhoud))

    (define (flush)
      (set! inhoud '()))

    (define (value pos)
      (list-ref inhoud pos))

    (define (dispatch msg)
      (cond ((eq? msg 'newValue) newValue)
        ((eq? msg 'return) inhoud)
        ((eq? msg 'returnSum) (returnSum))
        ((eq? msg 'flush) (flush))
        ((eq? msg 'value) value)
        ((eq? msg 'size) (length inhoud))
        (else (error "wrong message"))))
    dispatch))

(define buffer (maak-buffer))
((buffer 'newValue) 3)
((buffer 'newValue) 9)
(define res1 (and (= (buffer 'returnSum) 12)
               (equal? (buffer 'return) '(3 9))
               (begin (buffer 'flush))
               (null? (buffer 'return))))

(define (make-counter)
  (let ((state 0))
    (define (increment) (set! state (+ state 1)))
    (define (read) state)
    (define (reset) (set! state 0))
    (define (dispatch msg)
      (cond ((eq? msg 'increment) (increment))
        ((eq? msg 'read) (read))
        ((eq? msg 'reset) (reset))
        (else (error "wrong message"))))
    dispatch))

(define (maak-verkeersteller)
  (let ((voorbijgereden (make-counter))
         (buffer (maak-buffer)))

    (define (newCar)
      (voorbijgereden 'increment))

    (define (newHour)
      ((buffer 'newValue) (voorbijgereden 'read))
      (voorbijgereden 'reset))

    (define (newDay)
      (define (loop start end)
        (cond ((= start end) (newline))
          (else (display2 "Tussen ") (display2 start)
            (display2 " en ") (display2 (+ start 1))
            (display2 " uur : ")
            (display2 ((buffer 'value) start))
            (display2 " auto's")
            (newline2)
            (loop (+ start 1) end))))
      (if (= (buffer 'size) 24)
        (begin (loop 0 24)
          (buffer 'flush)
          (voorbijgereden 'reset))
        (error2 "no 24 hours have passed")))

    (define (dispatch msg)
      (cond ((eq? msg 'newCar) (newCar))
        ((eq? msg 'newHour) (newHour))
        ((eq? msg 'newDay) (newDay))
        (else (error2 "wrong message"))))
    dispatch))

(define verkeersteller (maak-verkeersteller))
(verkeersteller 'newCar)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newCar)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newDay)

(verkeersteller 'newHour)

(verkeersteller 'newDay)

(equal? result '(newline
                  newline
                  " auto's"
                  1
                  " uur : "
                  24
                  " en "
                  23
                  "Tussen "
                  newline
                  " auto's"
                  1
                  " uur : "
                  23
                  " en "
                  22
                  "Tussen "
                  newline
                  " auto's"
                  2
                  " uur : "
                  22
                  " en "
                  21
                  "Tussen "
                  newline
                  " auto's"
                  0
                  " uur : "
                  21
                  " en "
                  20
                  "Tussen "
                  newline
                  " auto's"
                  1
                  " uur : "
                  20
                  " en "
                  19
                  "Tussen "
                  newline
                  " auto's"
                  0
                  " uur : "
                  19
                  " en "
                  18
                  "Tussen "
                  newline
                  " auto's"
                  1
                  " uur : "
                  18
                  " en "
                  17
                  "Tussen "
                  newline
                  " auto's"
                  0
                  " uur : "
                  17
                  " en "
                  16
                  "Tussen "
                  newline
                  " auto's"
                  1
                  " uur : "
                  16
                  " en "
                  15
                  "Tussen "
                  newline
                  " auto's"
                  1
                  " uur : "
                  15
                  " en "
                  14
                  "Tussen "
                  newline
                  " auto's"
                  0
                  " uur : "
                  14
                  " en "
                  13
                  "Tussen "
                  newline
                  " auto's"
                  0
                  " uur : "
                  13
                  " en "
                  12
                  "Tussen "
                  newline
                  " auto's"
                  0
                  " uur : "
                  12
                  " en "
                  11
                  "Tussen "
                  newline
                  " auto's"
                  1
                  " uur : "
                  11
                  " en "
                  10
                  "Tussen "
                  newline
                  " auto's"
                  2
                  " uur : "
                  10
                  " en "
                  9
                  "Tussen "
                  newline
                  " auto's"
                  2
                  " uur : "
                  9
                  " en "
                  8
                  "Tussen "
                  newline
                  " auto's"
                  0
                  " uur : "
                  8
                  " en "
                  7
                  "Tussen "
                  newline
                  " auto's"
                  0
                  " uur : "
                  7
                  " en "
                  6
                  "Tussen "
                  newline
                  " auto's"
                  1
                  " uur : "
                  6
                  " en "
                  5
                  "Tussen "
                  newline
                  " auto's"
                  0
                  " uur : "
                  5
                  " en "
                  4
                  "Tussen "
                  newline
                  " auto's"
                  0
                  " uur : "
                  4
                  " en "
                  3
                  "Tussen "
                  newline
                  " auto's"
                  3
                  " uur : "
                  3
                  " en "
                  2
                  "Tussen "
                  newline
                  " auto's"
                  0
                  " uur : "
                  2
                  " en "
                  1
                  "Tussen "
                  newline
                  " auto's"
                  2
                  " uur : "
                  1
                  " en "
                  0
                  "Tussen "
                  (error2 "no 24 hours have passed")))

; 8.16
(define result '())
(define display (lambda (i) (set! result (cons i result))))
(define newline (lambda () (set! result (cons 'newline result))))

(define (display-all . args)
  (for-each display args))
(define (display-all-sep args)
  (for-each (lambda (arg) (display arg) (display " "))
    args))

(define (make-tweet username text tags)

  (define (display-tweet)
    (display-all "Tweet from " username "\n" text "\nTags: ")
    (display-all-sep tags)
    (newline))

  (define (dispatch msg)
    (cond ((eq? msg 'text) text)
      ((eq? msg 'tags) tags)
      ((eq? msg 'username) username)
      ((eq? msg 'display) display-tweet)
      (else (display "error - wrong msg ") (display msg))))

  (if (> (string-length text) 140)
    #f
    dispatch))

(define (make-account name username)
  (let ((followers '())
         (tweets '())
         (tweet-wall '()))

    (define (follow account)
      ((account 'add-follower) dispatch))

    (define (add-follower account)
      (set! followers (cons account followers)))

    (define (tweet text . tags)
      (let ((tweet-obj (make-tweet username text tags)))
        (set! tweets (cons tweet-obj tweets))
        (set! tweet-wall (cons tweet-obj tweet-wall))
        (for-each (lambda (follower)
                    ((follower 'add-tweet-to-wall) tweet-obj))
          followers)))

    (define (add-tweet-to-wall tweet)
      (set! tweet-wall (cons tweet tweet-wall)))

    (define (display-account symbol)
      (cond ((eq? symbol 'wall) (display-wall))
        ((eq? symbol 'followers) (display-followers))
        ((eq? symbol 'account) (display-entire-account))
        (else (display "wrong symbol given"))))

    (define (display-wall)
      (display "TWEET WALL") (newline)
      (for-each (lambda (tweet) ((tweet 'display)) (newline))
        tweet-wall))

    (define (display-followers)
      (display "FOLLOWERS") (newline)
      (for-each (lambda (follower)
                  (display (follower 'username)) (display " "))
        followers))

    (define (display-entire-account)
      (display-all "Twitter name " username "\n"
        "Name " name "\n")
      (display-wall)
      (display-followers)
      (newline) (newline))

    (define (dispatch msg)
      (cond ((eq? msg 'name)                           name)
        ((eq? msg 'username)                   username)
        ((eq? msg 'display)             display-account)
        ((eq? msg 'follow)                       follow)
        ((eq? msg 'add-follower)           add-follower)
        ((eq? msg 'tweet)                         tweet)
        ((eq? msg 'add-tweet-to-wall) add-tweet-to-wall)
        (else (display "error - wrong msg ") (display msg))))
    dispatch))

(define my-tweet (make-tweet "madewael" "Racket is cool!" (list "#Racket" "#Scheme")))
(define res1 (equal? (my-tweet 'username) "madewael"))
((my-tweet 'display))

(define (make-account name username)
  (let ((followers '())
         (tweets '())
         (tweet-wall '()))

    (define (follow account)
      ((account 'add-follower) dispatch))

    (define (add-follower account)
      (set! followers (cons account followers)))

    (define (tweet text . tags)
      (let ((tweet-obj (make-tweet username text tags)))
        (set! tweets (cons tweet-obj tweets))
        (set! tweet-wall (cons tweet-obj tweet-wall))
        (for-each (lambda (follower)
                    ((follower 'add-tweet-to-wall) tweet-obj))
          followers)))

    (define (add-tweet-to-wall tweet)
      (set! tweet-wall (cons tweet tweet-wall)))

    (define (display-account symbol)
      (cond ((eq? symbol 'wall) (display-wall))
        ((eq? symbol 'followers) (display-followers))
        ((eq? symbol 'account) (display-entire-account))
        (else (display "wrong symbol given"))))

    (define (display-wall)
      (display "TWEET WALL") (newline)
      (for-each (lambda (tweet) ((tweet 'display)) (newline))
        tweet-wall))

    (define (display-followers)
      (display "FOLLOWERS") (newline)
      (for-each (lambda (follower)
                  (display (follower 'username)) (display " "))
        followers))

    (define (display-entire-account)
      (display-all "Twitter name " username "\n"
        "Name " name "\n")
      (display-wall)
      (display-followers)
      (newline) (newline))

    (define (dispatch msg)
      (cond ((eq? msg 'name)                           name)
        ((eq? msg 'username)                   username)
        ((eq? msg 'display)             display-account)
        ((eq? msg 'follow)                       follow)
        ((eq? msg 'add-follower)           add-follower)
        ((eq? msg 'tweet)                         tweet)
        ((eq? msg 'add-tweet-to-wall) add-tweet-to-wall)
        (else (display "error - wrong msg ") (display msg))))
    dispatch))

(define accountE (make-account "Eline Philips" "ephilips"))
(define accountM (make-account "Mattias De Wael" "madewael"))
((accountE 'follow) accountM)
((accountM 'tweet) "Racket is cool!" "#Racket" "#Scheme")
((accountE 'tweet) "Hello World!")
((accountE 'display) 'account)
((accountM 'display) 'account)
(and res1
  (equal? result '(newline
                    newline
                    " "
                    "ephilips"
                    newline
                    "FOLLOWERS"
                    newline
                    newline
                    " "
                    "#Scheme"
                    " "
                    "#Racket"
                    "\nTags: "
                    "Racket is cool!"
                    "\n"
                    "madewael"
                    "Tweet from "
                    newline
                    "TWEET WALL"
                    "\n"
                    "Mattias De Wael"
                    "Name "
                    "\n"
                    "madewael"
                    "Twitter name "
                    newline
                    newline
                    newline
                    "FOLLOWERS"
                    newline
                    newline
                    " "
                    "#Scheme"
                    " "
                    "#Racket"
                    "\nTags: "
                    "Racket is cool!"
                    "\n"
                    "madewael"
                    "Tweet from "
                    newline
                    newline
                    "\nTags: "
                    "Hello World!"
                    "\n"
                    "ephilips"
                    "Tweet from "
                    newline
                    "TWEET WALL"
                    "\n"
                    "Eline Philips"
                    "Name "
                    "\n"
                    "ephilips"
                    "Twitter name "
                    newline
                    " "
                    "#Scheme"
                    " "
                    "#Racket"
                    "\nTags: "
                    "Racket is cool!"
                    "\n"
                    "madewael"
                    "Tweet from ")))

; 9.2

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
      (count-pairs (cdr x))
      1)))


(define ret3 (cons 'a (cons 'b (cons 'c '()))))
(define ret4
  (let ((last (cons 'c '())))
    (cons last (cons 'b last))))
(define ret7
  (let* ((last (cons 'c '()))
          (middle (cons last last)))
    (cons middle middle)))

(and (= (count-pairs ret3) 3)
  (= (count-pairs ret4) 4)
  (= (count-pairs ret7) 7))

; 9.3
(define result '())
(define display (lambda (i) (set! result (cons i result))))

(define (make-ring n)
  (let ((last (cons 0 '())))
    (define (build-list n)
      (if (= n 0)
        last
        (cons n (build-list (- n 1)))))
    (let ((ring (build-list n)))
      (set-cdr! last ring)
      ring)))

(define (print-ring r)
  (define (aux l)
    (if (not (null? l))
      (if (eq? (cdr l) r)
        (begin (display " ")
          (display (car l))
          (display "..."))
        (begin (display " ")
          (display (car l))
          (aux (cdr l))))))
  (aux r)
  #t)


(define r (make-ring 3))
(print-ring r)
(print-ring (cdr r))
(equal? result '("..." 3 " " 0 " " 1 " " 2 " " "..." 0 " " 1 " " 2 " " 3 " "))

; 9.5
(define result '())
(define display (lambda (i) (set! result (cons i result))))

(define (make-ring n)
  (let ((last (cons 0 '())))
    (define (build-list n)
      (if (= n 0)
        last
        (cons n (build-list (- n 1)))))
    (let ((ring (build-list n)))
      (set-cdr! last ring)
      ring)))

(define (print-ring r)
  (define (aux l)
    (if (not (null? l))
      (if (eq? (cdr l) r)
        (begin (display " ")
          (display (car l))
          (display "..."))
        (begin (display " ")
          (display (car l))
          (aux (cdr l))))))
  (aux r)
  #t)

(define (right-rotate r)
  (define (iter l)
    (if (eq? (cdr l) r)
      l
      (iter (cdr l))))
  (iter r))

(define r (make-ring 3))
(print-ring (right-rotate r))
(equal? result '("..." 1 " " 2 " " 3 " " 0 " "))

; 9.6
(define ret4
  (let ((last (cons 'c '())))
    (cons last (cons 'b last))))
(define ret7
  (let* ((last (cons 'c '()))
          (middle (cons last last)))
    (cons middle middle)))
(define retno
  (let* ((last (cons 'c '()))
          (lst (cons 'a (cons 'b last))))
    (set-cdr! last lst)
    lst))

(define (cycles? lst)
  (define (find-cycles? current path)
    (cond
      ((null? current) #f)
      ((memq current path) #t)
      (else (find-cycles? (cdr current)
              (cons current path)))))
  (find-cycles? lst '()))

(and (not (cycles? '()))
  (not (cycles? '(1 2 3)))
  (not (cycles? ret4))
  (cycles? retno)
  (not (cycles? ret7))
  (cycles? (cons 'a (cons 'b retno))))

; 9.7
(define result '())
(define display (lambda (i) (set! result (cons i result))))

(define (make-ring n)
  (let ((last (cons 0 '())))
    (define (build-list n)
      (if (= n 0)
        last
        (cons n (build-list (- n 1)))))
    (let ((ring (build-list n)))
      (set-cdr! last ring)
      ring)))

(define (print-ring r)
  (define (aux l)
    (if (not (null? l))
      (if (eq? (cdr l) r)
        (begin (display " ")
          (display (car l))
          (display "..."))
        (begin (display " ")
          (display (car l))
          (aux (cdr l))))))
  (aux r)
  #t)

(define (copy-ring r)
  (define last '())
  (define (aux l)
    (cond ((eq? (cdr l) r)
            (set! last (cons (car l) '()))
            last)
      (else (cons (car l) (aux (cdr l))))))

  (let ((first (aux r)))
    (set-cdr! last first)
    first))

(define r (make-ring 3))
(define s (copy-ring r))
(print-ring s)
(set-car! s 999)
(print-ring s)
(print-ring r)
(equal? result '("..." 0 " " 1 " " 2 " " 3 " " "..." 0 " " 1 " " 2 " " 999 " " "..." 0 " " 1 " " 2 " " 3 " "))

; 9.8
(define result '())
(define display (lambda (i) (set! result (cons i result))))

(define (make-ring n)
  (let ((last (cons 0 '())))
    (define (build-list n)
      (if (= n 0)
        last
        (cons n (build-list (- n 1)))))
    (let ((ring (build-list n)))
      (set-cdr! last ring)
      ring)))

(define (print-ring r)
  (define (aux l)
    (if (not (null? l))
      (if (eq? (cdr l) r)
        (begin (display " ")
          (display (car l))
          (display "..."))
        (begin (display " ")
          (display (car l))
          (aux (cdr l))))))
  (aux r)
  #t)

(define (copy-ring r)
  (define last '())
  (define (aux l)
    (cond ((eq? (cdr l) r)
            (set! last (cons (car l) '()))
            last)
      (else (cons (car l) (aux (cdr l))))))

  (let ((first (aux r)))
    (set-cdr! last first)
    first))

(define (Josephus r n)
  (define (remove-nth! l n)
    (if (<= n 2)
      (begin (set-cdr! l (cddr l))
        (cdr l))
      (remove-nth! (cdr l) (- n 1))))

  (define (iter l)
    (print-ring l)
    (if (eq? l (cdr l))
      (car l)
      (iter (remove-nth! l n))))

  (if (= n 1)
    (car (right-rotate r))
    (iter (copy-ring r))))

(define ring (make-ring 5))
(Josephus ring 5)
(print-ring ring)
(equal? result '("..." 0 " " 1 " " 2 " " 3 " " 4 " " 5 " " "..." 5 " " "..." 5 " " 3 " " "..." 3 " " 4 " " 5 " " "..." 3 " " 4 " " 5 " " 0 " " "..." 2 " " 3 " " 4 " " 5 " " 0 " " "..." 0 " " 1 " " 2 " " 3 " " 4 " " 5 " "))

; 9.9
(define (count-pairs lst)
  (let ((path '()))
    (define (count current)
      (cond
        ((null? current) 0)
        ((not (pair? current)) 0)
        ((memq current path) 0)
        (else
          (set! path (cons current path))
          (+ 1 (count (car current))
            (count (cdr current))))))
    (count lst)))

(define ret3 (cons 'a (cons 'b (cons 'c '()))))
(define ret4
  (let ((last (cons 'c '())))
    (cons last (cons 'b last))))
(define ret7
  (let* ((last (cons 'c '()))
          (middle (cons last last)))
    (cons middle middle)))
(define retno
  (let* ((last (cons 'c '()))
          (lst (cons 'a (cons 'b last))))
    (set-cdr! last lst)
    lst))

(= 3 (count-pairs ret3) (count-pairs ret4) (count-pairs ret7) (count-pairs retno))

; 9.12
(define (find-last lijst)
  (if (null? lijst)
    (error "find-last -- lijst heeft geen laatste element")
    (let ((next (cdr lijst)))
      (if (null? next)
        lijst
        (find-last next)))))

(define (flatten! lijst)
  (if (null? lijst)
      '()
    (let* ((sublist (car lijst))
            (restlist (flatten! (cdr lijst))))
      (if (null? sublist)
        restlist
        (let ((last (find-last sublist)))
          (set-cdr! last restlist)
          sublist)))))

(define (atom? x) (not (pair? x)))

(define (flatten2! lijst)
  (let ((hulpcel (cons 'dummy lijst)))
    (define (flatten-aux! prev current)
      (cond ((null? current) (cdr hulpcel))
        ((null? (car current))
          (set-cdr! prev (cdr current))
          (flatten-aux! prev (cdr current)))
        ((pair? (car current))
          (set-cdr! prev (flatten2! (car current)))
          (flatten-aux! (find-last prev) (cdr current)))
        ((null? (cdr prev))
          (set-cdr! prev current)
          (flatten-aux! (cdr prev) (cdr current)))
        ((atom? (car current))
          (flatten-aux! (cdr prev) (cdr current)))))
    (flatten-aux! hulpcel lijst)
    (cdr hulpcel)))

(and (equal? (flatten! '((1 2) (3 4 5) (6) (7 8))) '(1 2 3 4 5 6 7 8))
  (equal? (flatten! '(() (1 2) (3 4 5) () (6) (7 8))) '(1 2 3 4 5 6 7 8))
  (equal? (flatten2! '((1 (2 3) 4) 5 6 (7 8))) '(1 2 3 4 5 6 7 8))
  (equal? (flatten2! '((1 2) (3 4 5) (6) (7 8))) '(1 2 3 4 5 6 7 8))
  (equal? (flatten2! '(() (1 2) (3 4 5) () (6) (7 8))) '(1 2 3 4 5 6 7 8))
  (equal? (flatten2! '(1 2 (3 (4 5) 6 (7 8) 9) 10)) '(1 2 3 4 5 6 7 8 9 10)))

; 9.13
(define result '())
(define display (lambda (i) (set! result (cons i result))))

(define (kw-lijst lst)
  (define (loop l)
    (let ((rest (cdr l))
           (n (list (* (car l) (car l)))))
      (set-cdr! l n)
      (set-cdr! n rest)
      (if (not (eq? rest lst))
        (loop rest))))
  (loop lst)
  lst)

(define (print-ring r)
  (define (aux l)
    (if (not (null? l))
      (if (eq? (cdr l) r)
        (begin (display " ")
          (display (car l))
          (display "..."))
        (begin (display " ")
          (display (car l))
          (aux (cdr l))))))
  (aux r)
  #t)

(define last-cons (cons 3 '()))
(define test-lst (cons 1 (cons 4 last-cons)))
(set-cdr! last-cons test-lst)
(print-ring (kw-lijst test-lst))
(equal? result '("..." 9 " " 3 " " 16 " " 4 " " 1 " " 1 " "))

; 9.14
(define (schuif-in! l1 l2)
  (cond ((null? (cdr l1)) (set-cdr! l1 l2) 'ok)
    ((null? l2) 'ok)
    (else
      (let ((rest1 (cdr l1)) (rest2 (cdr l2)))
        (set-cdr! l1 l2)
        (set-cdr! l2 rest1)
        (schuif-in! rest1 rest2)))))

(define lijst1 '(1 3 5))
(define lijst2 '(2 4 6 8))
(schuif-in! lijst1 lijst2)
(equal? lijst1 '(1 2 3 4 5 6 8))

; 9.15
(define (ontdubbel! lijst)
  (let ((deEven '())
         (deOneven '()))
    (define (ontdubbel-iter prevE prevO restLijst)
      (cond ((null? restLijst) (set-cdr! prevE '())
              (set-cdr! prevO '())
              (cons deEven deOneven))
        ((even? (car restLijst))
          (if (null? prevE)
            (set! deEven restLijst)
            (set-cdr! prevE restLijst))
          (ontdubbel-iter restLijst prevO (cdr restLijst)))
        (else (if (null? prevO)
                (set! deOneven restLijst)
                (set-cdr! prevO restLijst))
          (ontdubbel-iter prevE restLijst (cdr restLijst)))))
    (ontdubbel-iter deEven deOneven lijst)))

(equal? (ontdubbel! '(1 2 3 4 5 6 7 8 9 10)) '((2 4 6 8 10) 1 3 5 7 9))

; 9.16
(define (insert-aux! lst lst2)
  (set-cdr! lst2 '())
  (if (null? (cdr lst))
    (set-cdr! lst lst2)
    (insert-aux! (cdr lst) lst2))
  lst)

(define (insert! lst1 lst2)
  (if (not (null? lst1))
    (begin
      (insert! (cdr lst1) (cdr lst2))
      (insert-aux! (car lst1) lst2)
      lst1)))

(and (equal? (insert-aux! '(a 12 q) '(v w x y z)) '(a 12 q v))
  (equal? (insert! '((a 12 q) (b 13) (c 14 r s) (f 18) (j 22 t)) '(v w x y z))
      '((a 12 q v) (b 13 w) (c 14 r s x) (f 18 y) (j 22 t z))))

; 9.17
(define (all-but-interval lst min max)
  (define (aux last-smaller-cons aux-lst)
    (cond
      ((null? aux-lst)
        (set-cdr! last-smaller-cons '()))
      ((< (car aux-lst) min)
        (aux aux-lst (cdr aux-lst)))
      ((> (car aux-lst) max)
        (set-cdr! last-smaller-cons aux-lst))
      (else
        (aux last-smaller-cons (cdr aux-lst)))))
  (aux lst lst)
  lst)

(and (equal? (all-but-interval '(1 2 3 4 5 6) 2 4) '(1 5 6))
  (equal? (all-but-interval '(1 2 3 4 5) 2 2) '(1 3 4 5))
  (equal? (all-but-interval '(1 2 5 6 7) 3 9) '(1 2)))

; 9.18
(define (first-el best)
  (if (not (null? best))
    (caar best)
    #f))

(define (smaller? el1 el2)
  (string<? (symbol->string el1) (symbol->string el2)))

(define (same? el1 el2)
  (equal? el1 el2))


(define (merge best1 best2)
  (define (merge-in curr1 curr2 prev)
    (cond ((null? curr1) (set-cdr! prev curr2))
      ((null? curr2) (set-cdr! prev curr1))
      ((same? (first-el curr1) (first-el curr2))
        (set-cdr! prev curr1)
        (merge-in (cdr curr1) (cdr curr2) curr1))
      ((smaller? (first-el curr1) (first-el curr2))
        (set-cdr! prev curr1)
        (merge-in (cdr curr1) curr2 curr1))
      (else
        (set-cdr! prev curr2)
        (merge-in curr1 (cdr curr2) curr2))))

  (let* ((result (if (smaller? (first-el best1) (first-el best2))
                   best1
                   best2))
          (curr1 (if (eq? result best1) (cdr best1) best1))
          (curr2 (if (eq? result best2) (cdr best2) best2)))
    (merge-in curr1 curr2 result)
    result))

(define best1 '((ann (meiboomstraat 12 1820 Eppegem))
                 (bert (populierendreef 7 1050 Brussel))
                 (kurt (Mechelsesteenweg 50 1800 Vilvoorde))))

(define best2 '((bert (populierendreef 7 1050 Brussel))
                 (jan (eikestraat 1 9000 Gent))
                 (sofie (boerendreef 5  2800 Mechelen))))

(equal? (merge best1 best2)
    '((ann (meiboomstraat 12 1820 Eppegem)) (bert (populierendreef 7 1050 Brussel)) (jan (eikestraat 1 9000 Gent)) (kurt (Mechelsesteenweg 50 1800 Vilvoorde)) (sofie (boerendreef 5 2800 Mechelen))))