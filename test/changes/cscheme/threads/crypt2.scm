;; Example adapted from https://rosettacode.org/wiki/Vigen%C3%A8re_cipher/Cryptanalysis#Racket
(define (range a b)
  (letrec ((loop (lambda (i acc)
                   (if (< i a)
                     acc
                     (loop (- i 1) (cons i acc))))))
    (loop (- b 1) '())))

(define (range-offset a b offset)
  (letrec ((loop (lambda (i acc)
                   (if (>= i b)
                     (reverse acc)
                     (loop (+ i offset) (cons i acc))))))
    (loop a '())))

(define foldl
  (lambda (f base lst)

    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
          base
          (foldl-aux (f base (car lst)) (cdr lst)))))

    (foldl-aux base lst)))

(define max-keylen 30)

(define first-char (char->integer #\A))
(define charsn (- (char->integer #\Z) first-char -1))

(define freqs                        ; english letter frequencies from wikipedia
  (vector (/ 8167 100000.0) (/ 1492 100000.0) (/ 2782 100000.0) (/ 4253 100000.0) (/ 12702 100000.0) (/ 2228 100000.0) (/ 2015 100000.0) (/ 6094 100000.0) (/ 6966 100000.0) (/ 153 100000.0) (/ 772 100000.0) (/ 4025 100000.0) (/ 2406 100000.0)
    (/ 6749 100000.0) (/ 7507 100000.0) (/ 1929 100000.0) (/ 95 100000.0) (/ 5987 100000.0) (/ 6327 100000.0) (/ 9056 100000.0) (/ 2758 100000.0) (/ 978 100000.0) (/ 2360 100000.0) (/ 150 100000.0) (/ 1974 100000.0) (/ 74 100000.0)))

(define (n*n-1 n) (* n (- n 1)))

(define text* (vector
                12 14 12 20 3 4 10 0 15 21 19 16 4 5 12 14 4 21 7 15 0 9 12 8 8 2 3 2 19 8 5 6 24
                0 6 9 18 15 23 24 0 11 20 24 12 13 18 12 24 7 21 20 23 9 4 11 4 15 23 9 5 23 6 2 12
                9 7 10 3 25 17 24 8 2 20 7 24 15 20 18 15 6 8 6 12 14 8 24 7 5 22 7 19 2 16 10 12 11
                17 3 8 19 11 23 25 11 9 5 21 16 6 7 14 11 22 2 20 7 11 14 12 3 18 14 4 10 19 0 11 2
                0 21 24 11 13 25 17 5 6 1 23 15 7 21 6 0 11 22 16 8 18 5 6 17 15 7 9 14 14 5 22 6 20
                1 24 8 11 0 15 11 0 11 2 0 5 0 0 12 10 11 6 2 4 19 3 22 21 14 4 11 9 8 10 6 9 1 23
                15 7 21 6 0 11 22 16 2 18 13 22 1 20 1 24 7 2 20 7 10 14 2 4 23 9 4 24 10 1 16 10 21
                24 10 8 8 4 7 6 17 11 6 7 23 4 14 11 22 0 22 5 14 9 8 11 14 21 21 17 7 15 10 3 22 8
                7 10 13 0 19 20 7 13 21 17 24 0 16 3 8 21 7 23 5 7 17 25 21 16 22 12 22 21 11 6 18
                7 13 13 11 21 25 18 9 11 0 10 8 5 7 23 20 5 23 9 11 23 12 19 1 11 16 21 17 23 23 7 1
                7 5 25 23 6 21 11 17 0 9 8 4 23 15 17 21 14 18 12 13 15 10 4 15 3 19 11 15 17 22 12
                9 0 25 15 10 11 16 20 25 0 0 11 6 25 23 6 21 11 10 11 6 9 19 20 8 8 19 3 18 20 17 4
                25 23 9 4 17 23 25 18 7 12 15 18 19 12 19 4 14 4 15 0 15 9 7 18 12 5 13 1 24 21 16 2
                0 25 0 0 11 6 0 24 3 13 12 15 0 16 14 22 19 20 7 3 1 21 19 18 12 20 4 20 8 12 21 7 1
                6 6 21 17 22 0 4 5 18 15 4 12 15 21 4 15 10 23 25 24 22 11 10 9 0 6 22 0 11 19 21 24
                24 14 1 24 8 23 14 10 8 7 15 3 18 4 21 11 4 21 17 21 18 6 1 9 14 6 24 22 5 7 10 1 1
                1 6 11 23 24 0 12 21 10 8 18 10 8 4 7 24 8 12 0 15 23 20 14 8 18 10 15 21 0 6 13 12
                25 7 15 22 19 19 25 15 21 23 5 2 2 3 19 20 7 9 7 22 11 0 15 5 24 20 11 19 1 20 23 9
                11 13 18 8 9 21 21 24 14 21 3 9 18 14 11 23 6 19 6 17 21 14 18 5 17 8 8 2 19 12 10 1
                4 9 5 2 16 5 10 19 8 13 16 1 22 21 7 6 19 4 13 11 7 7 14 6 2 18 15 18 5 15 21 6 9 14
                10 12 18 8 5 15 17 25 15 0 0 18 0 19 15 19 25 5 19 15 15 3 15 14 17 17 5 19 0 23 25
                15 10 0 11 16 0 22 12 8 20 3 1 22 13 2 19 11 4 5 10 14 25 16 3 11 23 1 20 23 9 11 0
                18 8 12 17 15 13 12 1 5 25 2 24 11 21 22 0 15 21 5 16 17 7 25 21 25 6 25 4 5 10 1 2
                4 8 14 14 5 23 24 4 21 14 22 6 1 1 23 21 2 1 23 1 0 22 6 11 16 10 2 12 8 2 17 17 23
                12 0 2 20 14 8 10 7 16 20 0 9 4 6 11 14 8 9 7 7 23 15 21 25 22 9 4 22 1 0 5 22 0 12
                11 25 25 17 23 9 4 10 0 7 21 5 0 18 12 20 11 21 21 20 19 19 6 10
              ))

(define (display-text text)
  (for-each (lambda (i)
              (let ((c (vector-ref text i)))
                (display (integer->char (+ c first-char)))))
    (range 0 (vector-length text)))
  (newline))

(display-text text*)

(define N (vector-length text*))
(define (get-col-length+freqs width offset)
  (define text (map (lambda (i) (vector-ref text* i)) (range-offset offset N width)))
  (define cN (vector-length text*))
  (define freqs (make-vector charsn 0))
  (for-each (lambda (c) (vector-set! freqs c (+ 1 (vector-ref freqs c))))
    text)
  (cons cN freqs))

(define expected-IC (* charsn (foldl + 0 (map (lambda (i)
                                                (let ((x (vector-ref freqs i)))
                                                  (* x x)))
                                           (range 0 (vector-length freqs))))))

;; maps key lengths to average index of coincidence
(define keylen->ICs
  (map (lambda (len)
         (foldl + 0 (map (lambda (ofs)
                           (let* ((vs (get-col-length+freqs len ofs))
                                   (cN (car vs))
                                   (cfreqs (cdr vs)))
                             (/ (foldl + 0
                                  (map (lambda (i)
                                         (n*n-1 (vector-ref cfreqs i)))
                                    (range 0 charsn)))
                               (/ (n*n-1 cN) charsn len 1.0))))
                      (range 0 len))))
    (range 1 (+ 1 (* max-keylen 2)))))

(define (argmin f l)
  (<change> ; <=========================================================================================================
    (begin
      (define (helper l minv minelem)
        (if (null? l)
          minelem
          (let* ((el (car l))
                  (v (f (car l))))
            (if (< v minv)
              (helper (cdr l) v el)
              (helper (cdr l) minv minelem)))))
      (if (null? l)
        (error "argmin over empty list")
        (helper (cdr l) (f (car l)) (car l))))
    (if (null? l)
      (error "argmin over empty list")
      (cdr (foldl (lambda (acc v)
                    (let ((vl (f v)))
                      (if (< vl (car acc))
                          (cons vl v)
                          acc)))
                  (cons (f (car l)) (car l))
                  (cdr l))))))

(define mycar car)
;; given a key length find the key that minimizes errors from alphabet freqs,
;; return (cons average-error key)
(define (guess-key len)
  (define guesses
    (map (lambda (ofs)
           (let* ((vs (get-col-length+freqs len ofs))
                   (cN (car vs))
                   (cfreqs (cdr vs)))
             (for-each (lambda (i) (vector-set! cfreqs i (/ (vector-ref cfreqs i) cN)))
               (range 0 charsn))
             (argmin car
               (map (lambda (d)
                      (cons (foldl + 0
                              (map (lambda (i)
                                     (expt (- (vector-ref freqs i)
                                             (vector-ref cfreqs (modulo (+ i d) charsn)))
                                       2))
                                (range 0 charsn)))
                        d))
                 (range 0 charsn)))))
      (range 0 len)))
  (cons (/ (foldl + 0 (map car guesses)) len) (map cdr guesses)))

;; look for a key length that minimizes error from expected-IC, with some
;; stupid consideration of multiples of the length (which should also have low
;; errors), for each one guess a key, then find the one that minimizes both (in
;; a way that looks like it works, but undoubtedly is wrong in all kinds of
;; ways) and return the winner key
(define best-key
  (let ((extract-best (lambda (l)
                        (cdr (argmin car l))))
         (local-guess (lambda (i)
                        (define with-multiples
                          (letrec ((loopj (lambda (j acc)
                                            (if (>= j (* max-keylen 2))
                                              acc
                                              (letrec ((loopdiv (lambda (div acc)
                                                                  (if (= div N)
                                                                    acc
                                                                    (loopdiv (+ div 1)
                                                                      (cons (cons (/ (abs (- (list-ref keylen->ICs j) expected-IC))
                                                                                    expected-IC)
                                                                              (/ (+ 1 div)))
                                                                        acc))))))
                                                (loopj (+ j (+ 1 i)) (loopdiv 0 acc)))))))
                            (loopj 0 '())))
                        (define total (/ (foldl + 0 (map (lambda (x) (* (car x) (cdr x))) with-multiples))
                                        (foldl + 0 (map cdr with-multiples))))
                        (define guess (guess-key (+ 1 i)))
                        (define guess*total (* total (car guess) (car guess)))
                        (cons guess*total (cdr guess)))))
    (extract-best
      (map (lambda (t) (join t))
        (map  (lambda (i) (fork (local-guess i)))
          (range 0 (* max-keylen 2)))))))

(display "Best key found: ")
(for-each (lambda (c) (display (integer->char (+ c first-char)))) best-key)
(newline)

(display "Decoded text:\n")
(define decode-num
  (let ((cur '()))
    (lambda (n)
      (if (null? cur) (set! cur best-key) #t)
      (let ((res (modulo (- n (car cur)) charsn)))
        (set! cur (cdr cur))
        res))))
(for-each (lambda (i)
            (let ((n (vector-ref text* i)))
              (display (integer->char (+ first-char (decode-num n))))))
  (range 0 (vector-length text*)))
(newline)