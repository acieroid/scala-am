(and (equal? (append '(1 2 3) '(4 5)) '(1 2 3 4 5))
     (equal? (append '(1 2 3) '()) '(1 2 3))
     (equal? (append '() '(1 2 3)) '(1 2 3))
     (null? (append '() '())))
