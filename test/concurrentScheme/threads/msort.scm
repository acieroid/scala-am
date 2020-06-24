;; Multithreaded merge-sort
(define (merge-lists xs ys)
  (if (null? xs)
      ys
      (if (null? ys)
          xs
          (if (< (car xs) (car ys))
              (cons (car xs) (merge-lists (cdr xs) ys))
              (cons (car ys) (merge-lists xs (cdr ys)))))))

(define (take l n)
  (if (or (= n 0) (null? l))
      '()
      (cons (car l) (take (cdr l) (- n 1)))))

(define (drop l n)
  (if (or (= n 0) (null? l))
      l
      (drop (cdr l) (- n 1))))

(define (merge-sort l)
  (let ((len (length l)))
    (if (< len 2)
        l
        (if (= len 2)
            (merge-lists (list (car l)) (list (cadr l)))
            (let ((first-half (fork (merge-sort (take l (quotient len 2)))))
                  (second-half (fork (merge-sort (drop l (quotient len 2))))))
              (merge-lists (join first-half) (join second-half)))))))

(define (sorted? l)
  (if (or (null? l) (null? (cdr l)))
      #t
      (if (<= (car l) (cadr l))
          (sorted? (cdr l))
          #f)))

(define (generate-list size)
  (if (= size 0)
      '()
      (cons (random 100) (generate-list (- size 1)))))

(define N (+ 100 (random 42)))
(define L (generate-list N))
(sorted? (merge-sort L))
