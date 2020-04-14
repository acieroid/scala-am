(define (sanity-check x) (eq? x x)) ;should always be #t
(define (random-bool) 
    (= (random 2) 0)) ;50/50 change to get #t or #f here (to simulate some non-determinism)
(sanity-check (random-bool)) 