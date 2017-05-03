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