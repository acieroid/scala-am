(define (schuif-in! l1 l2)
  (cond ((null? (cdr l1)) (set-cdr! l1 l2) 'ok)
        ((null? l2) 'ok)
        (else
         (let ((rest1 (cdr l1)) (rest2 (cdr l2)))
           (set-cdr! l1 l2)
           (set-cdr! l2 rest1)
           (schuif-in! rest1 rest2)))))

(define lijst1 '(1 3 5))
(define lijst2 '(2 4 6 8))
(schuif-in! lijst1 lijst2)
(equal? lijst1 '(1 2 3 4 5 6 8))