;;; SUM -- Compute sum of integers from 0 to 10000

(define (run n)
  (let loop ((i n) (sum 0))
    (if (< i 0)
      sum
      (loop (- i 1) (+ i sum)))))

(= (run 10000) 50005000)