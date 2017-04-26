(define result '())
(define display (lambda (i) (set! result (cons i result))))
(define newline (lambda () (set! result (cons 'newline result))))
(define error (lambda (e) (set! result (cons (list 'error e) result))))


(define (maak-buffer)
  (let ((inhoud '()))

    (define (newValue value)
      (set! inhoud (append inhoud (list value))))

    (define (returnSum)
      (apply + inhoud))

    (define (flush)
      (set! inhoud '()))

    (define (value pos)
      (list-ref inhoud pos))

    (define (dispatch msg)
      (cond ((eq? msg 'newValue) newValue)
            ((eq? msg 'return) inhoud)
            ((eq? msg 'returnSum) (returnSum))
            ((eq? msg 'flush) (flush))
            ((eq? msg 'value) value)
            ((eq? msg 'size) (length inhoud))
            (else (error "wrong message"))))
    dispatch))

(define buffer (maak-buffer))
((buffer 'newValue) 3)
((buffer 'newValue) 9)
(define res1 (and (= (buffer 'returnSum) 12)
                  (equal? (buffer 'return) '(3 9))
                  (begin (buffer 'flush))
                  (null? (buffer 'return))))

(define (make-counter)
  (let ((state 0))
    (define (increment) (set! state (+ state 1)))
    (define (read) state)
    (define (reset) (set! state 0))
    (define (dispatch msg)
      (cond ((eq? msg 'increment) (increment))
            ((eq? msg 'read) (read))
            ((eq? msg 'reset) (reset))
            (else (error "wrong message"))))
    dispatch))

(define (maak-verkeersteller)
  (let ((voorbijgereden (make-counter))
        (buffer (maak-buffer)))

    (define (newCar)
      (voorbijgereden 'increment))

    (define (newHour)
      ((buffer 'newValue) (voorbijgereden 'read))
      (voorbijgereden 'reset))

    (define (newDay)
      (define (loop start end)
        (cond ((= start end) (newline))
              (else (display "Tussen ") (display start)
                    (display " en ") (display (+ start 1))
                    (display " uur : ")
                    (display ((buffer 'value) start))
                    (display " auto's")
                    (newline)
                    (loop (+ start 1) end))))
      (if (= (buffer 'size) 24)
          (begin (loop 0 24)
                 (buffer 'flush)
                 (voorbijgereden 'reset))
          (error "no 24 hours have passed")))

    (define (dispatch msg)
      (cond ((eq? msg 'newCar) (newCar))
            ((eq? msg 'newHour) (newHour))
            ((eq? msg 'newDay) (newDay))
            (else (error "wrong message"))))
    dispatch))

(define verkeersteller (maak-verkeersteller))
(verkeersteller 'newCar)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newCar)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newHour)

(verkeersteller 'newCar)

(verkeersteller 'newDay)

(verkeersteller 'newHour)

(verkeersteller 'newDay)

(equal? result '(newline
                 newline
                 " auto's"
                 1
                 " uur : "
                 24
                 " en "
                 23
                 "Tussen "
                 newline
                 " auto's"
                 1
                 " uur : "
                 23
                 " en "
                 22
                 "Tussen "
                 newline
                 " auto's"
                 2
                 " uur : "
                 22
                 " en "
                 21
                 "Tussen "
                 newline
                 " auto's"
                 0
                 " uur : "
                 21
                 " en "
                 20
                 "Tussen "
                 newline
                 " auto's"
                 1
                 " uur : "
                 20
                 " en "
                 19
                 "Tussen "
                 newline
                 " auto's"
                 0
                 " uur : "
                 19
                 " en "
                 18
                 "Tussen "
                 newline
                 " auto's"
                 1
                 " uur : "
                 18
                 " en "
                 17
                 "Tussen "
                 newline
                 " auto's"
                 0
                 " uur : "
                 17
                 " en "
                 16
                 "Tussen "
                 newline
                 " auto's"
                 1
                 " uur : "
                 16
                 " en "
                 15
                 "Tussen "
                 newline
                 " auto's"
                 1
                 " uur : "
                 15
                 " en "
                 14
                 "Tussen "
                 newline
                 " auto's"
                 0
                 " uur : "
                 14
                 " en "
                 13
                 "Tussen "
                 newline
                 " auto's"
                 0
                 " uur : "
                 13
                 " en "
                 12
                 "Tussen "
                 newline
                 " auto's"
                 0
                 " uur : "
                 12
                 " en "
                 11
                 "Tussen "
                 newline
                 " auto's"
                 1
                 " uur : "
                 11
                 " en "
                 10
                 "Tussen "
                 newline
                 " auto's"
                 2
                 " uur : "
                 10
                 " en "
                 9
                 "Tussen "
                 newline
                 " auto's"
                 2
                 " uur : "
                 9
                 " en "
                 8
                 "Tussen "
                 newline
                 " auto's"
                 0
                 " uur : "
                 8
                 " en "
                 7
                 "Tussen "
                 newline
                 " auto's"
                 0
                 " uur : "
                 7
                 " en "
                 6
                 "Tussen "
                 newline
                 " auto's"
                 1
                 " uur : "
                 6
                 " en "
                 5
                 "Tussen "
                 newline
                 " auto's"
                 0
                 " uur : "
                 5
                 " en "
                 4
                 "Tussen "
                 newline
                 " auto's"
                 0
                 " uur : "
                 4
                 " en "
                 3
                 "Tussen "
                 newline
                 " auto's"
                 3
                 " uur : "
                 3
                 " en "
                 2
                 "Tussen "
                 newline
                 " auto's"
                 0
                 " uur : "
                 2
                 " en "
                 1
                 "Tussen "
                 newline
                 " auto's"
                 2
                 " uur : "
                 1
                 " en "
                 0
                 "Tussen "
                 (error "no 24 hours have passed")))