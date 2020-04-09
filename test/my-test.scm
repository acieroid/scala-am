(define (f x)
    (if (random-bool)
        x
        (f (cons 1 x))))

(define (random-bool)
    (= (random 2) 1))

(f '())