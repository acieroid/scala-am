;;; SUMLOOP -- One of the Kernighan and Van Wyk benchmarks.

(define sum 0)

(define (tail-rec-aux i n)
  (if (< i n)
      (begin (set! sum (+ sum 1)) (tail-rec-aux (+ i 1) n))
      sum))

(define (tail-rec-loop n)
  (set! sum 0)
  (tail-rec-aux 0 n)
  sum)

(define (do-loop n)
  (set! sum 0)
  (do ((i 0 (+ i 1)))
      ((>= i n) sum)
    (set! sum (+ sum 1))))

(= (do-loop 100000000) 100000000)