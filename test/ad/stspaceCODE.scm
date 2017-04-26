(define false #f)
(define true #t)

(define (create-stack eq-fnct)
  (let ((content '()))
    (define (empty?)
      (null? content))
    (define (push element)
      (set! content (cons element content))
      #t)
    (define (pop)
      (if (null? content)
          #f
          (let ((temp (car content)))
            (set! content (cdr content))
            temp)))
    (define (top)
      (if (null? content)
          #f
          (car content)))
    (define (is-in element)
      (if (member element content)
          #t
          #f))
    (define (dispatch m)
      (cond
        ((eq? m 'empty?) empty?)
        ((eq? m 'push) push)
        ((eq? m 'pop) pop)
        ((eq? m 'top) top)
        ((eq? m 'is-in) is-in)
        (else (error "unknown request
                     -- create-stack" m))))
    dispatch))

(define (create-set . same)
  (let ((content '())
        (same? (if (null? same) eq? (car same))))
    (define (empty?)
      (null? content))
    (define (is-in? item)
      (define (find-iter current)
        (cond ((null? current) false)
              ((same? item (car current)) true)
              (else (find-iter (cdr current)))))
      (find-iter content))
    (define (insert item)
      (if (not (is-in? item))
          (set! content (cons item content)))
      true)
    (define (delete item)
      (define (remove-iter current prev)
        (cond
          ((null? current) false)
          ((same item (car current))
           (if (null? prev)
               (set! content (cdr content))
               (set-cdr! prev (cdr current)))
           true)
          (else (remove-iter (cdr current) current))))
      (remove-iter content '()))
    (define (map a-function)
      (define (map-iter current result)
        (if (null? current)
            (reverse result)
            (map-iter (cdr current)
                      (cons (a-function (car current))
                            result))))
      (map-iter content '()))
    (define (foreach a-action)
      (define (foreach-iter current)
        (cond
          ((null? current) true)
          (else (a-action (car current))
                (foreach-iter (cdr current)))))
      (foreach-iter content)
      true)
    (define (dispatch m)
      (cond
        ((eq? m 'empty?) empty?)
        ((eq? m 'is-in) is-in?)
        ((eq? m 'insert) insert)
        ((eq? m 'delete) delete)
        ((eq? m 'map) map)
        ((eq? m 'foreach) foreach)
        (else
          (error "unknown request
                 -- create-set" m))))
    dispatch))



(define (create-operator pre op) (list pre op))
(define (precondition operator) (car operator))
(define (operation operator) (cadr operator))


(define (depth-first-search start-state  operators
                            goal-reached? good-state? eq-state?
                            present-goal
                            open-trace-action
                            closed-trace-action)
  (let ((open (create-stack eq-state?))
        (closed (create-set eq-state?)))
    (define (expand state)
      (define (iter rest)
        (cond ((null? rest) false)
              (else
                (let ((operator (car rest)))
                  (if ((precondition operator) state)
                      (let ((new-state ((operation operator) state)))
                        (if (good-state? new-state)
                            (if (goal-reached? new-state)
                                (begin
                                  (if open-trace-action
                                      (open-trace-action new-state))
                                  (present-goal new-state)
                                  true)
                                (begin
                                  (if (and (not ((open 'is-in) new-state))
                                           (not ((closed 'is-in) new-state)))
                                      (begin
                                        ((open 'push) new-state)
                                        (if open-trace-action
                                            (open-trace-action new-state))))
                                  (iter (cdr rest))))
                            (iter (cdr rest))))
                      (iter (cdr rest)))))))
      (iter operators))
    (define (loop)
      (cond (((open 'empty?)) false)
            (else (let ((state ((open 'pop))))
                    ((closed 'insert) state)
                    (if closed-trace-action
                        (closed-trace-action state))
                    (let ((solution (expand state)))
                      (if solution
                          solution
                          (loop)))))))
    ((open 'push) start-state)
    (loop)))

