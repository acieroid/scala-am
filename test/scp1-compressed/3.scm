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
(define display2 (lambda (i) (set! result (cons i result))))

(define (count1 x)
  (cond ((= 0 x) (display2 x))
    (else (display2 x)
      (count1 (- x 1)))))

(define (count2 x)
  (cond ((= 0 x) (display2 x))
    (else (count2 (- x 1))
      (display2 x))))

(count1 4)
(count2 4)
(equal? result '(4 3 2 1 0 0 1 2 3 4))

; 3.6
(define result '())
(define display3 (lambda (i) (set! result (cons i result))))

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
      (for-each display3 (list min "\t" (depth-weird min) "\n"))
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
(define display4 (lambda (i) (set! result (cons i result))))
(define newline4 (lambda () (set! result (cons 'newline result))))

(define (display-n n x)
  (if (> n 0)
    (begin
      (display4 x)
      (display-n (- n 1) x))))

(define (parasol n)
  (define (triangle i)
    (if (< i n)
      (begin
        (display-n (- n i 1) " ")
        (display-n (+ (* 2 i) 1) "*")
        (newline4)
        (triangle (+ i 1)))))

  (define (stick i)
    (if (< i 3)
      (begin
        (display-n (- n 1) " ")
        (display4 "*")(newline4)
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
