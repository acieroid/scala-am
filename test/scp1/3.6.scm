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