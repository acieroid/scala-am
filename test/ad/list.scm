(define (create-list . size)
  (let* ((list-size (if (null? size) 10 (car size)))
         (content  (make-vector list-size))
         (first 0)
         (list-length 0))
    (define (last)
      (remainder (+ first list-length) list-size))
    (define (next-index index)
      (remainder (+ index 1) list-size))
    (define (prev-index index)
      (remainder (+ index -1 list-size) list-size))
    (define (list-index position)
      (remainder (+ first position -1) list-size))
    (define (shift-right start end)
      (define (shift-iter index)
        (cond
          ((= index start)
           (vector-set! content (next-index start) (vector-ref content start)))
          (else
           (vector-set! content (next-index index) (vector-ref content index))
           (shift-iter (prev-index index)))))
      (shift-iter end))
    (define (shift-left start end)
      (define (shift-iter index)
        (cond
          ((= index end)
           (vector-set! content (prev-index end) (vector-ref content end)))
          (else
           (vector-set! content (prev-index index) (vector-ref content index))
           (shift-iter (next-index index)))))
      (shift-iter start))
    (define (empty?)
      (zero? list-length))
    (define (retrieve position)
      (if (< list-length position)
          #f
          (vector-ref content (list-index position))))
    (define (insert position element)
      (cond
        ((< position 1) #f)
        ((>= list-length list-size) #f)
        ((> position (+ list-length 1)) #f)
        (else
         (set! list-length (+ 1 list-length))
         (if (< position (- list-length position))
             (begin
               (set! first (prev-index first))
               (shift-left first (list-index position)))
             (shift-right (list-index position) (last)))
         (vector-set! content (list-index position) element)
         #t)))
    (define (delete position)
      (cond
        ((< list-length position) #f)
        (else
         (set! list-length (- list-length 1))
         (if (< position (- list-length position))
             (begin
               (set! first (next-index first))
               (shift-right first (list-index position)))
             (shift-left (list-index position) (last)))
         #t)))
    (define (replace position element)
      (cond
        ((< list-length position) #f)
        (else
         (vector-set! content (list-index position) element)
         #t)))
    (define (dispatch m . args)
      (cond
        ((eq? m 'empty?) (empty?))
        ((eq? m 'insert) (insert (car args) (cadr args)))
        ((eq? m 'delete) (delete (car args)))
        ((eq? m 'retrieve) (retrieve (car args)))
        ((eq? m 'replace) (replace (car args) (cadr args)))
        (else
         (error "unknown request -- create-list" m))))
    dispatch))

(define L (create-list 6))
(equal? (list(L 'insert 1 7)(L 'insert 1 99)(L 'retrieve 1)(L 'retrieve 2)(L 'delete 2)(L 'replace 1 111)(L 'retrieve 1)(L 'empty?)(L 'delete 1)(L 'empty?))
        '(#t #t 99 7 #t #t 111 #f #t #t))