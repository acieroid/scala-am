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
     (not (cycles? ret4))
     (cycles? retno)
     (not (cycles? ret7))
     (cycles? (cons 'a (cons 'b retno))))