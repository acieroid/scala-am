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