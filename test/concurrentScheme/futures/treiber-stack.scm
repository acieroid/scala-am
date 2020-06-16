; Treiber stack as an example of a lock-free data structure using atoms.

(define (new-stack)
 (atom '()))

(define (push stk el)
 (let loop ((top (read stk)))
  (if (not (compare-and-set! stk top (cons el top)))
   (loop (read stk)))))

(define (pop stk)
 (let loop ((top (read stk)))
  (cond ((null? top) #f)
        ((compare-and-set! stk top (cdr top)) (car top))
        (else  (loop (read stk))))))

(define (loop stk n f)
 (if (> n 0)
  (let ((next (f stk n)))
       (loop stk next f))))

(define stack (new-stack))
(define f1 (future (loop stack 25 (lambda (s n) (push s n) (- n 1)))))
(define f2 (future (loop stack 25 (lambda (s n) (if (pop s) (- n 1) n)))))

(deref f1)
(deref f2)
(not (pop stack))