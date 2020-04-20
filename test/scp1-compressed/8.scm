; 8.1.1
(define flip
  (let ((state 0))
    (lambda ()
      (if (= state 0)
        (set! state 1)
        (set! state 0))
      state)))

(and (= (flip) 1)
  (= (flip) 0)
  (= (flip) 1)
  (= (flip) 0))

; 8.1.3
(define (make-flip)
  (let ((state 0))
    (lambda ()
      (if (= state 0)
        (set! state 1)
        (set! state 0))
      state)))

(define flip (make-flip))

(and (= (flip) 1)
  (= (flip) 0)
  (= (flip) 1)
  (= (flip) 0))

; 8.5
;(define (make-point x y)
;
;  (define (dispatch msg)
;    (cond ((eq? msg 'x-value) x)
;      ((eq? msg 'y-value) y)
;      (else (error "wrong message"))))
;  dispatch)
;
;(define (make-segment start end)
;
;  (define (midpoint)
;    (make-point (/ (+ (start 'x-value) (end 'x-value)) 2)
;      (/ (+ (start 'y-value) (end 'y-value)) 2)))
;
;  (define (dispatch msg)
;    (cond ((eq? msg 'start-point) start)
;      ((eq? msg 'end-point) end)
;      ((eq? msg 'midpoint) (midpoint))
;      (else (error "wrong message"))))
;  dispatch)
;
;
;(define (make-w-vector . args)
;
;  (define (dimension)
;    (length args))
;
;  (define (coordinate n)
;    (if (or (< n 1) (> n (dimension)))
;      (error "coordinate is out of range")
;      (list-ref args (- n 1))))
;
;  (define (add w-vector)
;    (define (loop ctr res)
;      (if (= ctr 0)
;        (apply make-w-vector res)
;        (loop (- ctr 1) (cons (+ (coordinate ctr)
;                                ((w-vector 'coordinate) ctr))
;                          res))))
;    (loop (dimension) '()))
;
;
;
;  (define (dispatch msg)
;    (cond ((eq? msg 'dimension) (dimension))
;      ((eq? msg 'coordinate) coordinate)
;      ((eq? msg 'add) add)
;      (else (error "wrong message"))))
;  dispatch)
;
;
;(define (make-polynome . coefficients)
;  (let ((polynome (apply make-w-vector coefficients)))
;
;    (define (coefficient index)
;      ((polynome 'coordinate) index))
;
;    (define (order)
;      (- (polynome 'dimension) 1))
;
;    (define (dispatch msg)
;      (cond ((eq? msg 'order) (order))
;        ((eq? msg 'coefficient) coefficient)
;        (else (error "wrong message"))))
;    dispatch))
;
;(define point1 (make-point 6 10))
;(define point2 (make-point 10 20))
;(define segment (make-segment point1 point2))
;(define midpoint (segment 'midpoint))
;(define w-vector1 (make-w-vector 1 2 3))
;(define w-vector2 (make-w-vector 4 5 6))
;(define polynome (make-polynome 1 2 3))
;
;(and (= (point1 'x-value) 6)
;  (= ((segment 'start-point) 'y-value) 10)
;  (= (midpoint 'x-value) 8)
;  (= ((w-vector1 'coordinate) 2) 2)
;  (= ((w-vector2 'coordinate) 1) 4)
;  (= ((((w-vector1 'add) w-vector2) 'coordinate) 1) 5)
;  (= (polynome 'order) 2)
;  (= ((polynome 'coefficient) 2) 2))

; 8.6
(define result '())
(define display (lambda (i) (set! result (cons i result))))

(define hulp 2)
(define (haha x)
  (let ((hulp (* x hulp)))
    (display hulp))
  (display hulp)
  (set! hulp 4))

(haha 2)
(haha 3)
(equal? result '(4 12 2 4))

; 8.10
(define result '())
(define display (lambda (i) (set! result (cons i result))))
(define newline (lambda () (set! result (cons 'newline result))))

(define (create-counter)
  (let ((value 0))

    (define (reset)
      (set! value 0)
        'ok)

    (define (next)
      (set! value (+ 1 value))
        'ok)

    (define (increase x)
      (set! value (+ value x)))

    (define (dispatch msg)
      (cond ((eq? msg 'reset) reset)
        ((eq? msg 'next) next)
        ((eq? msg 'read) value)
        ((eq? msg 'increase) increase)
        (else (error "wrong message: " msg))))
    dispatch))


(define (make-scorebord)
  (let ((c-home (create-counter))
         (c-visit (create-counter)))

    (define (reset)
      ((c-home 'reset))
      ((c-visit 'reset))
        'ok)

    (define (read)
      (let ((c1 (c-home 'read))
             (c2 (c-visit 'read)))
        (display c1)
        (display "-")
        (display c2)
        (newline)
          'ok))

    (define (score team n)
      (cond ((not (or (= n 1) (= n 2) (= n 3)))
              (newline)
              (display "De score kan slechts 1, 2 of 3 zijn!")
              (newline)
                'ok)
        ((eq? team 'home)
          ((c-home 'increase) n)
            'ok)
        ((eq? team 'visit)
          ((c-visit 'increase) n)
            'ok)
        (else (error "wrong team: " team))))

    (define (dispatch msg)
      (cond ((eq? msg 'reset) reset)
        ((eq? msg 'read) read)
        ((eq? msg 'score) score)
        (else (error "wrong message: " msg))))
    dispatch))

(define bord (make-scorebord))
((bord 'read))
((bord 'score) 'home 2)
((bord 'read))
((bord 'score) 'visit 5)
((bord 'read))
((bord 'reset))
((bord 'read))
(equal? result '(newline 0 "-" 0 newline 0 "-" 2 newline "De score kan slechts 1, 2 of 3 zijn!" newline newline 0 "-" 2 newline 0 "-" 0))

; 8.11
;(define (create-counter initial)
;  (define (increase) (set! initial (+ initial 1)))
;  (define (decrease) (set! initial (- initial 1)))
;  (define (read) initial)
;  (define (dispatch m)
;    (cond ((eq? m 'increase) (increase))
;      ((eq? m 'decrease) (decrease))
;      ((eq? m 'read) (read))
;      (else (display "wrong message"))))
;  dispatch)
;
;(define (create-parking . capaciteiten)
;  (let ((verdieping-ctrs (map create-counter capaciteiten))
;         (nr-verdiepingen (length capaciteiten))
;         (nbr-cars 0))
;
;    (define (total-capacity)
;      (apply + capaciteiten))
;
;    (define (full?)
;      (= nbr-cars (total-capacity)))
;
;    (define (empty?)
;      (= nbr-cars 0))
;
;    (define (max-reached-level level idx)
;      (>=  (level 'read) (list-ref capaciteiten (- idx 1))))
;
;    (define (level-current)
;      (define (loop lst index)
;        (cond ((null? lst) #f)
;          (else (let* ((level (car lst))
;                        (capacity (level 'read)))
;                  (if (> capacity 0)
;                    index
;                    (loop (cdr lst) (+ index 1)))))))
;      (loop verdieping-ctrs 1))
;
;    (define (level-to-leave)
;      (define (loop lst index)
;        (cond ((null? lst) #f)
;          (else (let* ((level (car lst))
;                        (capacity (level 'read)))
;                  (if (and (not (max-reached-level level index)) (>= capacity 0))
;                    index
;                    (loop (cdr lst) (- index 1)))))))
;      (loop (reverse verdieping-ctrs) nr-verdiepingen))
;
;    (define (car-enters)
;      (let ((level (level-current)))
;        (if level
;          (let ((verdieping-ctr (list-ref verdieping-ctrs
;                                  (- level 1))))
;            (set! nbr-cars (+ nbr-cars 1))
;            (verdieping-ctr 'decrease))
;          #f)))
;
;    (define (car-leaves)
;      (let ((level (level-to-leave)))
;
;        (if level
;          (let ((verdieping-ctr (list-ref verdieping-ctrs (- level 1))))
;            (set! nbr-cars (- nbr-cars 1))
;            (verdieping-ctr 'increase))
;          (let ((verdieping-ctr (list-ref verdieping-ctrs(- nr-verdiepingen 1))))
;            (set! nbr-cars (- nbr-cars 1))
;            (verdieping-ctr 'increase)))))
;
;    (define (dispatch msg)
;      (cond ((eq? msg 'full?) (full?))
;        ((eq? msg 'empty?) (empty?))
;        ((eq? msg 'level) (level-current))
;        ((eq? msg 'car-enters) (car-enters))
;        ((eq? msg 'lst) verdieping-ctrs)
;        ((eq? msg 'car-leaves) (car-leaves))
;        (else (error "wrong message"))))
;    dispatch))
;
;(define parking (create-parking 3 5 2))
;(and (= (parking 'level) 1)
;  (not (parking 'full?))
;  (= (begin (parking 'car-enters)
;       (parking 'car-enters)
;       (parking 'car-enters)
;       (parking 'car-enters)
;       (parking 'level))
;    2)
;  (not (parking 'empty?))
;  (begin (parking 'car-enters)
;    (parking 'car-enters)
;    (parking 'car-enters)
;    (parking 'car-enters)
;    (parking 'car-enters)
;    (parking 'car-enters)
;    (parking 'full?))
;  (not (parking 'car-enters))
;  (= (begin (parking 'car-leaves)
;       (parking 'car-leaves)
;       (parking 'car-leaves)
;       (parking 'level))
;    2))

; 8.12
(define (maak-rechthoek l b)
  (define (oppervlakte) (* l b))
  (define (omtrek) (* 2 (+ l b)))
  (define (dispatch m)
    (cond ((eq? m 'oppervlakte) (oppervlakte))
      ((eq? m 'omtrek) (omtrek))))
  dispatch)

(define (maak-vierkant zijde)
  (define rechthoek (maak-rechthoek zijde zijde))
  (define (schaal! n) (set! zijde (* n zijde)))
  (define (dispatch m)
    (cond ((eq? m 'oppervlakte) (rechthoek 'oppervlakte))
      ((eq? m 'omtrek) (rechthoek 'omtrek))
      ((eq? m 'schaal!) schaal!)))
  dispatch)

(define test (maak-vierkant 5))
(and (= (test 'oppervlakte) 25)
  (= (test 'omtrek) 20)
  (= (begin ((test 'schaal!) 2)
       (test 'oppervlakte))
    25))

; 8.13
(define (MaakLampje aantal)
  (define state 'off)

  (define (on!) (set! state 'on))

  (define (off!) (set! state 'off))

  (define (broken!) (set! state 'broken))

  (define (on?) (eq? state 'on))

  (define (off?) (eq? state 'off))

  (define (broken?) (eq? state 'broken))

  (define (switch!)
    (set! aantal (- aantal 1))
    (cond ((< aantal 0) (broken!))
      ((off?) (on!))
      ((on?) (off!)))
    (not (broken?)))

  (define (change! nieuw)
    (off!)
    (set! aantal nieuw)
      'changed)

  (define (dispatch msg)
    (cond ((eq? msg 'switch!) (switch!))
      ((eq? msg 'on?) (on?))
      ((eq? msg 'off?) (off?))
      ((eq? msg 'test?) (broken?))
      ((eq? msg 'change!) change!)
      (else (error "Message not understood."))))
  dispatch)

(define philips (MaakLampje 5))
(and (not (philips 'test?))
  (not (philips 'on?))
  (philips 'off?)
  (philips 'switch!)
  (philips 'switch!)
  (philips 'switch!)
  (philips 'switch!)
  (philips 'switch!)
  (not (philips 'switch!))
  (philips 'test?)
  (begin ((philips 'change!) 10)
    (not (philips 'test?)))
  (philips 'off?))

; 8.14
(define (maak-teller)
  (let ((result 0))

    (define (toets bedrag)
      (set! result (+ result bedrag)))

    (define (reset)
      (set! result 0))

    (define (dispatch msg)
      (cond ((eq? msg 'toets) toets)
        ((eq? msg 'lees) result)
        ((eq? msg 'reset) (reset))
        (else (error "wrong message"))))
    dispatch))

(define (maak-winkelkassa)
  (let ((saldo (maak-teller))
         (te-betalen (maak-teller))
         (ingetoetst 'product)
         (ontvangen 0))

    (define (toets type bedrag)
      (set! ingetoetst type)
      (cond  ((eq? type 'product)
               ((te-betalen 'toets) bedrag))
        ((eq? type 'ontvangen)
          (set! ontvangen bedrag))
        (else (error "wrong type"))))

    (define (enter)
      (if (eq? ingetoetst 'product)
        (te-betalen 'lees)
        (let ((wisselgeld (- ontvangen (te-betalen 'lees))))
          ((saldo 'toets) (te-betalen 'lees))
          (te-betalen 'reset)
          wisselgeld)))

    (define (inhoud)
      (saldo 'lees))

    (define (afsluiten)
      (let ((teruggeven saldo))
        (set! saldo 0)
        teruggeven))

    (define (dispatch msg)
      (cond ((eq? msg 'toets) toets)
        ((eq? msg 'enter) (enter))
        ((eq? msg 'inhoud) (inhoud))
        ((eq? msg 'afsluiten) (afsluiten))
        (else (error "wrong message"))))
    dispatch))

(define teller (maak-teller))
(define winkelkassa (maak-winkelkassa))
((winkelkassa 'toets) 'product 20)
((teller 'toets) 20)
((winkelkassa 'toets) 'product 5)
(and (= (teller 'lees) 20)
  (begin (teller 'reset)
    (= (teller 'lees) 0))
  (= (winkelkassa 'enter) 25)
  (= (begin ((winkelkassa 'toets) 'product 10)
       (winkelkassa 'enter))
    35)
  (begin ((winkelkassa 'toets) 'ontvangen 50)
    (= (winkelkassa 'enter) 15))
  (= (winkelkassa 'inhoud) 35))

; 8.15
(define foldr
  (lambda (f base lst)
    (define foldr-aux
      (lambda (lst)
        (if (null? lst)
          base
          (f (car lst) (foldr-aux (cdr lst))))))
    (foldr-aux lst)))
(define result '())
(define display2 (lambda (i) (set! result (cons i result))))
(define newline2 (lambda () (set! result (cons 'newline result))))
(define error2 (lambda (e) (set! result (cons (list 'error e) result))))


(define (maak-buffer)
  (let ((inhoud '()))

    (define (newValue value)
      (set! inhoud (append inhoud (list value))))

    (define (returnSum)
      (foldr + 0 inhoud))

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
          (else (display2 "Tussen ") (display2 start)
            (display2 " en ") (display2 (+ start 1))
            (display2 " uur : ")
            (display2 ((buffer 'value) start))
            (display2 " auto's")
            (newline2)
            (loop (+ start 1) end))))
      (if (= (buffer 'size) 24)
        (begin (loop 0 24)
          (buffer 'flush)
          (voorbijgereden 'reset))
        (error2 "no 24 hours have passed")))

    (define (dispatch msg)
      (cond ((eq? msg 'newCar) (newCar))
        ((eq? msg 'newHour) (newHour))
        ((eq? msg 'newDay) (newDay))
        (else (error2 "wrong message"))))
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
                  (error2 "no 24 hours have passed")))

; 8.16
(define result '())
(define display (lambda (i) (set! result (cons i result))))
(define newline (lambda () (set! result (cons 'newline result))))

(define (display-all . args)
  (for-each display args))
(define (display-all-sep args)
  (for-each (lambda (arg) (display arg) (display " "))
    args))

(define (make-tweet username text tags)

  (define (display-tweet)
    (display-all "Tweet from " username "\n" text "\nTags: ")
    (display-all-sep tags)
    (newline))

  (define (dispatch msg)
    (cond ((eq? msg 'text) text)
      ((eq? msg 'tags) tags)
      ((eq? msg 'username) username)
      ((eq? msg 'display) display-tweet)
      (else (display "error - wrong msg ") (display msg))))

  (if (> (string-length text) 140)
    #f
    dispatch))

(define (make-account name username)
  (let ((followers '())
         (tweets '())
         (tweet-wall '()))

    (define (follow account)
      ((account 'add-follower) dispatch))

    (define (add-follower account)
      (set! followers (cons account followers)))

    (define (tweet text . tags)
      (let ((tweet-obj (make-tweet username text tags)))
        (set! tweets (cons tweet-obj tweets))
        (set! tweet-wall (cons tweet-obj tweet-wall))
        (for-each (lambda (follower)
                    ((follower 'add-tweet-to-wall) tweet-obj))
          followers)))

    (define (add-tweet-to-wall tweet)
      (set! tweet-wall (cons tweet tweet-wall)))

    (define (display-account symbol)
      (cond ((eq? symbol 'wall) (display-wall))
        ((eq? symbol 'followers) (display-followers))
        ((eq? symbol 'account) (display-entire-account))
        (else (display "wrong symbol given"))))

    (define (display-wall)
      (display "TWEET WALL") (newline)
      (for-each (lambda (tweet) ((tweet 'display)) (newline))
        tweet-wall))

    (define (display-followers)
      (display "FOLLOWERS") (newline)
      (for-each (lambda (follower)
                  (display (follower 'username)) (display " "))
        followers))

    (define (display-entire-account)
      (display-all "Twitter name " username "\n"
        "Name " name "\n")
      (display-wall)
      (display-followers)
      (newline) (newline))

    (define (dispatch msg)
      (cond ((eq? msg 'name)                           name)
        ((eq? msg 'username)                   username)
        ((eq? msg 'display)             display-account)
        ((eq? msg 'follow)                       follow)
        ((eq? msg 'add-follower)           add-follower)
        ((eq? msg 'tweet)                         tweet)
        ((eq? msg 'add-tweet-to-wall) add-tweet-to-wall)
        (else (display "error - wrong msg ") (display msg))))
    dispatch))

(define my-tweet (make-tweet "madewael" "Racket is cool!" (list "#Racket" "#Scheme")))
(define res1 (equal? (my-tweet 'username) "madewael"))
((my-tweet 'display))

(define (make-account name username)
  (let ((followers '())
         (tweets '())
         (tweet-wall '()))

    (define (follow account)
      ((account 'add-follower) dispatch))

    (define (add-follower account)
      (set! followers (cons account followers)))

    (define (tweet text . tags)
      (let ((tweet-obj (make-tweet username text tags)))
        (set! tweets (cons tweet-obj tweets))
        (set! tweet-wall (cons tweet-obj tweet-wall))
        (for-each (lambda (follower)
                    ((follower 'add-tweet-to-wall) tweet-obj))
          followers)))

    (define (add-tweet-to-wall tweet)
      (set! tweet-wall (cons tweet tweet-wall)))

    (define (display-account symbol)
      (cond ((eq? symbol 'wall) (display-wall))
        ((eq? symbol 'followers) (display-followers))
        ((eq? symbol 'account) (display-entire-account))
        (else (display "wrong symbol given"))))

    (define (display-wall)
      (display "TWEET WALL") (newline)
      (for-each (lambda (tweet) ((tweet 'display)) (newline))
        tweet-wall))

    (define (display-followers)
      (display "FOLLOWERS") (newline)
      (for-each (lambda (follower)
                  (display (follower 'username)) (display " "))
        followers))

    (define (display-entire-account)
      (display-all "Twitter name " username "\n"
        "Name " name "\n")
      (display-wall)
      (display-followers)
      (newline) (newline))

    (define (dispatch msg)
      (cond ((eq? msg 'name)                           name)
        ((eq? msg 'username)                   username)
        ((eq? msg 'display)             display-account)
        ((eq? msg 'follow)                       follow)
        ((eq? msg 'add-follower)           add-follower)
        ((eq? msg 'tweet)                         tweet)
        ((eq? msg 'add-tweet-to-wall) add-tweet-to-wall)
        (else (display "error - wrong msg ") (display msg))))
    dispatch))

(define accountE (make-account "Eline Philips" "ephilips"))
(define accountM (make-account "Mattias De Wael" "madewael"))
((accountE 'follow) accountM)
((accountM 'tweet) "Racket is cool!" "#Racket" "#Scheme")
((accountE 'tweet) "Hello World!")
((accountE 'display) 'account)
((accountM 'display) 'account)
(and res1
  (equal? result '(newline
                    newline
                    " "
                    "ephilips"
                    newline
                    "FOLLOWERS"
                    newline
                    newline
                    " "
                    "#Scheme"
                    " "
                    "#Racket"
                    "\nTags: "
                    "Racket is cool!"
                    "\n"
                    "madewael"
                    "Tweet from "
                    newline
                    "TWEET WALL"
                    "\n"
                    "Mattias De Wael"
                    "Name "
                    "\n"
                    "madewael"
                    "Twitter name "
                    newline
                    newline
                    newline
                    "FOLLOWERS"
                    newline
                    newline
                    " "
                    "#Scheme"
                    " "
                    "#Racket"
                    "\nTags: "
                    "Racket is cool!"
                    "\n"
                    "madewael"
                    "Tweet from "
                    newline
                    newline
                    "\nTags: "
                    "Hello World!"
                    "\n"
                    "ephilips"
                    "Tweet from "
                    newline
                    "TWEET WALL"
                    "\n"
                    "Eline Philips"
                    "Name "
                    "\n"
                    "ephilips"
                    "Twitter name "
                    newline
                    " "
                    "#Scheme"
                    " "
                    "#Racket"
                    "\nTags: "
                    "Racket is cool!"
                    "\n"
                    "madewael"
                    "Tweet from ")))