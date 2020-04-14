(define (append x y)
    (if (null? x)
        y
        (cons (car x)
              (append (cdr x) y))))

(define (naughty-fun n)
    (if (= n 1000)
        '()
        (append (list n) (naughty-fun (+ n 1)))))

(define (good-fun) 
    (append '(1 2 3) '(4 5)))

(naughty-fun 0)
(good-fun)