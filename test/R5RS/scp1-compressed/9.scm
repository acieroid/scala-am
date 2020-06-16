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
(define result2 '())
(define display2 (lambda (i) (set! result2 (cons i result2))))

(define (make-ring2 n)
  (let ((last2 (cons 0 '())))
    (define (build-list2 n)
      (if (= n 0)
        last2
        (cons n (build-list2 (- n 1)))))
    (let ((ring2 (build-list2 n)))
      (set-cdr! last2 ring2)
      ring2)))

(define (print-ring2 r)
  (define (aux l)
    (if (not (null? l))
      (if (eq? (cdr l) r)
        (begin (display2 " ")
          (display2 (car l))
          (display2 "..."))
        (begin (display2 " ")
          (display2 (car l))
          (aux (cdr l))))))
  (aux r)
  #t)


(define r2 (make-ring2 3))
(print-ring2 r2)
(print-ring2 (cdr r2))
(equal? result2 '("..." 3 " " 0 " " 1 " " 2 " " "..." 0 " " 1 " " 2 " " 3 " "))

; 9.5
(define result3 '())
(define display3 (lambda (i) (set! result3 (cons i result3))))

(define (make-ring3 n)
  (let ((last3 (cons 0 '())))
    (define (build-list3 n)
      (if (= n 0)
        last3
        (cons n (build-list3 (- n 1)))))
    (let ((ring3 (build-list3 n)))
      (set-cdr! last3 ring3)
      ring3)))

(define (print-ring3 r)
  (define (aux l)
    (if (not (null? l))
      (if (eq? (cdr l) r)
        (begin (display3 " ")
          (display3 (car l))
          (display3 "..."))
        (begin (display " ")
          (display3 (car l))
          (aux (cdr l))))))
  (aux r)
  #t)

(define (right-rotate r)
  (define (iter l)
    (if (eq? (cdr l) r)
      l
      (iter (cdr l))))
  (iter r))

(define r3 (make-ring3 3))
(print-ring3 (right-rotate r3))
(equal? result3 '("..." 1 " " 2 " " 3 " " 0 " "))

; 9.6
(define ret4-2
  (let ((last (cons 'c '())))
    (cons last (cons 'b last))))
(define ret7-2
  (let* ((last (cons 'c '()))
          (middle (cons last last)))
    (cons middle middle)))
(define retno-2
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
  (not (cycles? ret4-2))
  (cycles? retno-2)
  (not (cycles? ret7-2))
  (cycles? (cons 'a (cons 'b retno-2))))

; 9.7
(define result4 '())
(define display4 (lambda (i) (set! result4 (cons i result4))))

(define (make-ring4 n)
  (let ((last4 (cons 0 '())))
    (define (build-list4 n)
      (if (= n 0)
        last4
        (cons n (build-list4 (- n 1)))))
    (let ((ring4 (build-list4 n)))
      (set-cdr! last4 ring4)
      ring4)))

(define (print-ring4 r)
  (define (aux l)
    (if (not (null? l))
      (if (eq? (cdr l) r)
        (begin (display4 " ")
          (display4 (car l))
          (display4 "..."))
        (begin (display4 " ")
          (display4 (car l))
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

(define r4 (make-ring4 3))
(define s4 (copy-ring r4))
(print-ring4 s4)
(set-car! s4 999)
(print-ring4 s4)
(print-ring4 r4)
(equal? result4 '("..." 0 " " 1 " " 2 " " 3 " " "..." 0 " " 1 " " 2 " " 999 " " "..." 0 " " 1 " " 2 " " 3 " "))

; 9.8
(define result5 '())
(define display5 (lambda (i) (set! result5 (cons i result5))))

(define (make-ring5 n)
  (let ((last5 (cons 0 '())))
    (define (build-list5 n)
      (if (= n 0)
        last5
        (cons n (build-list5 (- n 1)))))
    (let ((ring5 (build-list5 n)))
      (set-cdr! last5 ring5)
      ring5)))

(define (print-ring5 r)
  (define (aux l)
    (if (not (null? l))
      (if (eq? (cdr l) r)
        (begin (display5 " ")
          (display5 (car l))
          (display5 "..."))
        (begin (display5 " ")
          (display5 (car l))
          (aux (cdr l))))))
  (aux r)
  #t)

(define (copy-ring5 r)
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
    (print-ring5 l)
    (if (eq? l (cdr l))
      (car l)
      (iter (remove-nth! l n))))

  (if (= n 1)
    (car (right-rotate r))
    (iter (copy-ring5 r))))

(define ring5 (make-ring5 5))
(Josephus ring5 5)
(print-ring5 ring5)
(equal? result5 '("..." 0 " " 1 " " 2 " " 3 " " 4 " " 5 " " "..." 5 " " "..." 5 " " 3 " " "..." 3 " " 4 " " 5 " " "..." 3 " " 4 " " 5 " " 0 " " "..." 2 " " 3 " " 4 " " 5 " " 0 " " "..." 0 " " 1 " " 2 " " 3 " " 4 " " 5 " "))

; 9.9
(define (count-pairs2 lst)
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

(define ret3-3 (cons 'a (cons 'b (cons 'c '()))))
(define ret4-3
  (let ((last (cons 'c '())))
    (cons last (cons 'b last))))
(define ret7-3
  (let* ((last (cons 'c '()))
          (middle (cons last last)))
    (cons middle middle)))
(define retno-3
  (let* ((last (cons 'c '()))
          (lst (cons 'a (cons 'b last))))
    (set-cdr! last lst)
    lst))

(= 3 (count-pairs2 ret3-3) (count-pairs2 ret4-3) (count-pairs2 ret7-3) (count-pairs2 retno-3))

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
(define result6 '())
(define display6 (lambda (i) (set! result6 (cons i result6))))

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

(define (print-ring6 r)
  (define (aux l)
    (if (not (null? l))
      (if (eq? (cdr l) r)
        (begin (display6 " ")
          (display6 (car l))
          (display6 "..."))
        (begin (display6 " ")
          (display6 (car l))
          (aux (cdr l))))))
  (aux r)
  #t)

(define last-cons (cons 3 '()))
(define test-lst (cons 1 (cons 4 last-cons)))
(set-cdr! last-cons test-lst)
(print-ring6 (kw-lijst test-lst))
(equal? result6 '("..." 9 " " 3 " " 16 " " 4 " " 1 " " 1 " "))

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
