(define (apply proc args)
  (cond
    ((null? args) (proc))
    ((null? (cdr args)) (proc (car args)))
    ((null? (cddr args)) (proc (car args) (cadr args)))
    ((null? (cdddr args)) (proc (car args) (cadr args) (caddr args)))
    ((null? (cddddr args)) (proc (car args) (cadr args) (caddr args) (cadddr args)))
    ((null? (cdr (cddddr args))) (proc (car args) (cadr args) (caddr args) (cadddr args) (car (cddddr args))))
    (else (error "Unsupported call."))))

(define (make-point x y)

  (define (dispatch msg)
    (cond ((eq? msg 'x-value) x)
          ((eq? msg 'y-value) y)
          (else (error "wrong message"))))
  dispatch)

(define (make-segment start end)

  (define (midpoint)
    (make-point (/ (+ (start 'x-value) (end 'x-value)) 2)
                (/ (+ (start 'y-value) (end 'y-value)) 2)))

  (define (dispatch msg)
    (cond ((eq? msg 'start-point) start)
          ((eq? msg 'end-point) end)
          ((eq? msg 'midpoint) (midpoint))
          (else (error "wrong message"))))
  dispatch)


(define (make-w-vector . args)

  (define (dimension)
    (length args))

  (define (coordinate n)
    (if (or (< n 1) (> n (dimension)))
        (error "coordinate is out of range")
        (list-ref args (- n 1))))

  (define (add w-vector)
    (define (loop ctr res)
      (if (= ctr 0)
          (apply make-w-vector res)
          (loop (- ctr 1) (cons (+ (coordinate ctr)
                                   ((w-vector 'coordinate) ctr))
                                res))))
    (loop (dimension) '()))



  (define (dispatch msg)
    (cond ((eq? msg 'dimension) (dimension))
          ((eq? msg 'coordinate) coordinate)
          ((eq? msg 'add) add)
          (else (error "wrong message"))))
  dispatch)


(define (make-polynome . coefficients)
  (let ((polynome (apply make-w-vector coefficients)))

    (define (coefficient index)
      ((polynome 'coordinate) index))

    (define (order)
      (- (polynome 'dimension) 1))

    (define (dispatch msg)
      (cond ((eq? msg 'order) (order))
            ((eq? msg 'coefficient) coefficient)
            (else (error "wrong message"))))
    dispatch))

(define point1 (make-point 6 10))
(define point2 (make-point 10 20))
(define segment (make-segment point1 point2))
(define midpoint (segment 'midpoint))
(define w-vector1 (make-w-vector 1 2 3))
(define w-vector2 (make-w-vector 4 5 6))
(define polynome (make-polynome 1 2 3))

(and (= (point1 'x-value) 6)
     (= ((segment 'start-point) 'y-value) 10)
     (= (midpoint 'x-value) 8)
     (= ((w-vector1 'coordinate) 2) 2)
     (= ((w-vector2 'coordinate) 1) 4)
     (= ((((w-vector1 'add) w-vector2) 'coordinate) 1) 5)
     (= (polynome 'order) 2)
     (= ((polynome 'coefficient) 2) 2))