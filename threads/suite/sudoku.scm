



;; Sudoku checker
(define board
  (vector
   (vector 6 2 4 5 3 9 1 8 7)
   (vector 5 1 9 7 2 8 6 3 4)
   (vector 8 3 7 6 1 4 2 9 5)
   (vector 1 4 3 8 6 5 7 2 9)
   (vector 9 5 8 2 4 7 3 6 1)
   (vector 7 6 2 3 9 1 4 5 8)
   (vector 3 7 1 9 5 6 8 4 2)
   (vector 4 9 6 1 8 2 5 7 3)
   (vector 2 8 5 4 7 3 9 1 6)))

(define (walk-row i)
  (letrec ((loop (lambda (j seen)
                   (if (< j 9)
                       (if (member (vector-ref (vector-ref board i) j) seen)
                           #f
                           (loop (+ j 1) (cons (vector-ref (vector-ref board i) j) seen)))
                       #t))))
    (loop 0 '())))

(define (walk-rows)
  (let ((wr1 (t/spawn (walk-row 0)))
        (wr2 (t/spawn (walk-row 1)))
        (wr3 (t/spawn (walk-row 2)))
        (wr4 (t/spawn (walk-row 3)))
        (wr5 (t/spawn (walk-row 4)))
        (wr6 (t/spawn (walk-row 5)))
        (wr7 (t/spawn (walk-row 6)))
        (wr8 (t/spawn (walk-row 7)))
        (wr9 (t/spawn (walk-row 8))))
    (and (t/join wr1) (t/join wr2) (t/join wr3)
         (t/join wr4) (t/join wr5) (t/join wr6)
         (t/join wr7) (t/join wr8) (t/join wr9))))

(define (walk-col j)
  (letrec ((loop (lambda (i seen)
                   (if (< i 9)
                       (if (member (vector-ref (vector-ref board i) j) seen)
                           #f
                           (loop (+ i 1) (cons (vector-ref (vector-ref board i) j) seen)))
                       #t))))
    (loop 0 '())))

(define (walk-cols)
  (let ((wc1 (t/spawn (walk-col 0)))
        (wc2 (t/spawn (walk-col 1)))
        (wc3 (t/spawn (walk-col 2)))
        (wc4 (t/spawn (walk-col 3)))
        (wc5 (t/spawn (walk-col 4)))
        (wc6 (t/spawn (walk-col 5)))
        (wc7 (t/spawn (walk-col 6)))
        (wc8 (t/spawn (walk-col 7)))
        (wc9 (t/spawn (walk-col 8))))
    (and (t/join wc1) (t/join wc2) (t/join wc3)
         (t/join wc4) (t/join wc5) (t/join wc6)
         (t/join wc7) (t/join wc8) (t/join wc9))))

(define (check-square starti startj)
  (letrec ((loop1 (lambda (i seen)
                    (if (< i (+ starti 3))
                        (letrec ((loop2 (lambda (j seen)
                                           (if (< j (+ startj 3))
                                               (if (member (vector-ref (vector-ref board i) j) seen)
                                                   #f
                                                   (loop2 (+ j 1) (cons (vector-ref (vector-ref board i) j) seen)))
                                               seen))))
                          (let ((loop2res (loop2 startj seen)))
                            (if loop2res
                                (loop1 (+ i 1) loop2res)
                                #f)))
                        #t))))
    (loop1 starti '())))

(define all-rows (t/spawn (walk-rows)))
(define all-cols (t/spawn (walk-cols)))
(define square1 (t/spawn (check-square 0 0)))
(define square2 (t/spawn (check-square 0 3)))
(define square3 (t/spawn (check-square 0 6)))
(define square4 (t/spawn (check-square 3 0)))
(define square5 (t/spawn (check-square 3 3)))
(define square6 (t/spawn (check-square 3 6)))
(define square7 (t/spawn (check-square 6 0)))
(define square8 (t/spawn (check-square 6 3)))
(define square9 (t/spawn (check-square 6 6)))

(and
 (t/join all-rows) (t/join all-cols)
 (t/join square1) (t/join square2) (t/join square3)
 (t/join square4) (t/join square5) (t/join square6)
 (t/join square7) (t/join square8) (t/join square9))
