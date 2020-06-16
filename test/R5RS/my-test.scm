(define (random-bool)
    (= (random 2) 0))

(define (f x)
    (if (random-bool)
        x
        (g (cons 'f x))))

(define (g x)
    (if (random-bool)
        x
        (f (cons 'g x))))

(f '())