;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;;
;;; contributed by Anthony Borla

; (define (nil) '())

(define (main)
  (let* ((n 5000)

        (alt 1) (d2 0) (d3 0) (ds 0) (dc 0)
        (s0 0) (s1 0) (s2 0) (s3 0) (s4 0) (s5 0) (s6 0) (s7 0) (s8 0)

    (print-numbers (lambda ()
      (display s0) (newline)
      (display s1) (newline)
      (display s2) (newline)
      (display s3) (newline)
      (display s4) (newline)
      (display s5) (newline)
      (display s6) (newline)
      (display s7) (newline)
      (display s8) (newline)))

    (set-numbers (lambda (d1)
      (set! d2 (* d1 d1))
      (set! d3 (* d2 d1))
      (set! ds d1)
      (set! dc d1)
      (set! s0 (+ s0 0))
      (set! s1 (+ s1 (/ 1 d1)))
      (set! s2 (+ s2 (/ 1 (* d1 (+ d1 1)))))
      (set! s3 (+ s3 (/ 1 (* d3 (* ds ds)))))
      (set! s4 (+ s4 (/ 1 (* d3 (* dc dc)))))
      (set! s5 (+ s5 (/ 1 d1)))
      (set! s6 (+ s6 (/ 1 d2)))
      (set! s7 (+ s7 (/ alt d1)))
      (set! s8 (+ s8 (/ alt (- (* 2 d1) 1))))
      (set! alt (- alt)))))

    (letrec ((loop1 (lambda (d1)
                      (if (>= n d1)
                          (begin (set-numbers d1)
                                 (loop1 (+ d1 1)))
                          (print-numbers)))))
      (loop1 1))))

(main)

