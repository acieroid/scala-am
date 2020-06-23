(define foldl
  (lambda (f base lst)

    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
            base
            (foldl-aux (f base (car lst)) (cdr lst)))))

    (foldl-aux base lst)))

(define N (+ 1 (int-top)))
(define FragmentSize 10)

(define (split from to)
  (let ((half (quotient (- to from) 2)))
    (list (cons from (+ from half))
             (cons (+ from half 1) to))))

(define (product from to)
  (if (= from to)
      from
      (* from (product (+ from 1) to))))

(define (fact-thread from to)
  (if (<= (- to from) FragmentSize)
      (product from to)
      (let ((steps (split from to)))
        (foldl * 1
               (map (lambda (t) (join t))
                    (map (lambda (bounds)
                           (fork
                            (fact-thread (car bounds) (cdr bounds))))
                         steps))))))

(define (fact-thread-ref from to result result-lock)
  (if (<= (- to from) FragmentSize)
      (let ((partial-fact (product from to)))
        (t/acquire result-lock)
        (t/ref-set result (* (t/deref result) partial-fact))
        (t/release result-lock))
      (let ((steps (split from to)))
        (map (lambda (t) (join t))
             (map (lambda (bounds)
                    (fork
                     (fact-thread-ref (car bounds) (cdr bounds)
                                      result result-lock)))
                  steps)))))

(define (fact n)
  (let* ((t1 (fork (fact-thread 1 n)))
         (result (t/ref 1))
         (result-lock (t/new-lock))
         (t2 (fork (fact-thread-ref 1 n result result-lock)))
         (res1 (join t1))
         (res2 (begin (join t2) (t/deref result))))
    (if (= res1 res2)
        (display res1)
        (display "factorials don't match..."))
    (newline)))

(fact N)
