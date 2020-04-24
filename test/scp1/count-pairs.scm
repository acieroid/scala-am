(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


(define ret3 (cons 'a (cons 'b (cons 'c '()))))
(define ret4
  (let ((last (cons 'c '())))
    (cons last (cons 'b last))))
(define ret7
  (let* ((last (cons 'c '()))
         (middle (cons last last)))
    (cons middle middle)))

(and (= (count-pairs ret3) 3)
     (= (count-pairs ret4) 4)
     (= (count-pairs ret7) 7))