;;
;; zie deel 6 p5
;;
(define BIAS     1000)
(define NMBR-prefix 1)
(define PAIR-prefix 2)
(define NULL-prefix 3)
(define MASK-prefix 4)

(define NMBR (* NMBR-prefix BIAS))
(define PAIR (* PAIR-prefix BIAS))
(define NULL (* NULL-prefix BIAS))
(define MASK (* MASK-prefix BIAS))

(define (PREFIX val)
  (quotient (abs val) BIAS))

(define (VALUE val)
  (remainder (abs val) BIAS))

(define (MAKE prefix val)
  (+ prefix val))

;;
;; zie deel 6 p7
;;
(define FREE 0)
(define SIZE 16)

(define THE-CARS (make-vector SIZE NULL))
(define THE-CDRS (make-vector SIZE NULL))

;;
;; zie deel 6 p8
;;
(define (car cons-val)
  (if (pair? cons-val)
      (vector-ref THE-CARS (VALUE cons-val))
      (error "pair expected")))

(define (cdr cons-val)
  (if (pair? cons-val)
      (vector-ref THE-CDRS (VALUE cons-val))
      (error "pair expected")))

(define (set-car! cons-val car-val)
  (if (pair? cons-val)
      (vector-set! THE-CARS (VALUE cons-val) car-val)
      (error "pair expected")))

(define (set-cdr! cons-val cdr-val)
  (if (pair? cons-val)
      (vector-set! THE-CDRS (VALUE cons-val) cdr-val)
      (error "pair expected")))

(define (number? val)
  (eq? (PREFIX val) NMBR-prefix))

(define (pair? val)
  (eq? (PREFIX val) PAIR-prefix))

(define (null? val)
  (eq? (PREFIX val) NULL-prefix))

(define (MASK? val)
  (eq? (PREFIX val) MASK-prefix))

;;
;; zie deel 6 p9
;;
(define (cons car-val cdr-val)
  (if (= FREE SIZE)
      (RECLAIM))
  (let
      ((hold FREE))
    (set! FREE (+ FREE 1))
    (vector-set! THE-CARS hold car-val)
    (vector-set! THE-CDRS hold cdr-val)
    (MAKE PAIR hold)))

(define ROOT NULL)

(define NEW-CARS (make-vector SIZE NULL))
(define NEW-CDRS (make-vector SIZE NULL))

;;
;; zie deel 6 p23-24
;;
(define (RECLAIM)
  (define (relocate-old-result-in-new val)
    (if (pair? val)
        (let* 
            ((old (VALUE val))
             (old-car (vector-ref THE-CARS old))
             (old-cdr (vector-ref THE-CDRS old)))
          (if (MASK? old-car)
              old-cdr
              (let*
                  ((new FREE)
                   (pair (MAKE PAIR new))) 
                (set! FREE (+ FREE 1))
                (vector-set! NEW-CARS new old-car)
                (vector-set! NEW-CDRS new old-cdr)
                (vector-set! THE-CARS old MASK)
                (vector-set! THE-CDRS old pair)
                pair)))
        val))

  (define (gc-flip)
    (let ((temp THE-CDRS))
      (set! THE-CDRS NEW-CDRS)
      (set! NEW-CDRS temp))
    (let ((temp THE-CARS))
      (set! THE-CARS NEW-CARS)
      (set! NEW-CARS temp)))
  
  (define (gc-loop scan)
    (if (= scan FREE)
        (gc-flip)
        (let 
            ((old-car (vector-ref NEW-CARS scan))
             (old-cdr (vector-ref NEW-CDRS scan)))
          (vector-set! NEW-CARS scan 
                       (relocate-old-result-in-new old-car))
          (vector-set! NEW-CDRS scan 
                       (relocate-old-result-in-new old-cdr))
          (gc-loop (+ scan 1)))))
  
  (set! FREE 0)
  (set! ROOT (relocate-old-result-in-new ROOT))
  (gc-loop 0))




;;
;; voorstelling van natuurlijk getal in geheugen 
;;
(define (_ val)
  (MAKE NMBR val))

(define _11 (_ 11))
(define _12 (_ 12))
(define _13 (_ 13))
(define _14 (_ 14))
(define _15 (_ 15))
(define _21 (_ 21))
(define _22 (_ 22))
(define _31 (_ 31))
(define _32 (_ 32))
(define _33 (_ 33))
(define _34 (_ 34))
(define _35 (_ 35))
(define _36 (_ 36))
(define _41 (_ 41))
(define _42 (_ 42))
(define _43 (_ 43))
(define _51 (_ 51))
(define _52 (_ 52))
(define _61 (_ 61))
(define _62 (_ 62))

(define (show)
  (display "cars > ") (display THE-CARS) (newline)
  (display "cdrs > ") (display THE-CDRS) (newline)
  (display "ROOT > ") (display ROOT) (display "  FREE > ") (display FREE) (newline))


(set! ROOT (cons (cons (cons _11 _12) (cons _13 (cons _14 _15))) ROOT))
(cons _21 _22)
(set! ROOT (cons (cons _31 (cons (cons _32 _33) (cons _34 (cons _35 _36)))) ROOT))
(set-cdr! (cdr (car ROOT)) ROOT)
(cons _41 (cons _42 _43))
(set! ROOT (cons (cons _51 _52) ROOT))
(set-car! (car ROOT) (cdr (car (cdr ROOT))))
;; displays the-cars and the-cdrs vectors
(show)
;; will trigger a garbage collect as there are no more free cells to allocate the pair to
(cons _61 _62)
;; displays the-cars and the-cdrs vectors
(show)

