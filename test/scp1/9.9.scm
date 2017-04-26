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