(define (atom? x)
  (not (pair? x)))

(define (maak-blad type) type)
(define (geef-type blad) blad)

(define (maak-knoop deelbomen) deelbomen)
(define (geef-deelbomen boom) boom)

(define (maak-hybride-tak knopen) knopen)
(define (geef-knopen tak) tak)

(define (leeg? boom) (null? boom))
(define (knoop? boom) (pair? boom))
(define (blad? boom) (atom? boom))

(define hybride-tak
  (maak-hybride-tak
   (list
    (maak-knoop
     (list
      (maak-knoop (list (maak-blad 'appel)
                        (maak-blad 'appel)
                        (maak-blad 'blad)))
      (maak-blad 'peer)))
    (maak-knoop (list (maak-blad 'blad)
                      (maak-blad 'peer)))
    (maak-knoop (list (maak-blad 'appel)
                      (maak-knoop (list (maak-blad 'appel)
                                        (maak-blad 'blad))))))))

(define tak
  (maak-hybride-tak
   (list
    (maak-knoop
     (list (maak-knoop (list (maak-blad 'appel)
                             (maak-blad 'appel)
                             (maak-blad 'blad)))
           (maak-blad 'peer)))
    (maak-knoop (list (maak-blad 'blad)
                      (maak-blad 'peer)
                      (maak-blad 'appel)))
    (maak-knoop (list (maak-blad 'appel)
                      (maak-knoop (list (maak-blad 'appel)
                                        (maak-blad 'blad))))))))

(define (tel boom)
  (define (combine-results l1 l2)
    (list (+ (car l1) (car l2))
          (+ (cadr l1) (cadr l2))
          (+ (caddr l1) (caddr l2))))

  (define (tel-hulp boom)
    (cond ((leeg? boom) (list 0 0 0))
          ((and (blad? boom) (eq? boom 'appel))
           (list 1 0 0))
          ((and (blad? boom) (eq? boom 'peer))
           (list 0 1 0))
          ((blad? boom) (list 0 0 1))
          (else (tel-hulp-in (geef-knopen boom)))))

  (define (tel-hulp-in lst)
    (if (null? lst)
        (list 0 0 0)
        (combine-results (tel-hulp (car lst))
                         (tel-hulp-in (cdr lst)))))
  (tel-hulp boom))

(define (member? x lst)
  (pair? (memq x lst)))

(define (normaal? knoop)
  (let ((types (map (lambda (x) (if (pair? x) 'tak x)) knoop)))
    (not (and (member? 'appel types) (member? 'peer types)))))

(define (check-normaal boom)
  (cond ((leeg? boom) #t)
        ((blad? boom) #t)
        ((knoop? boom)
         (and (normaal? boom)
              (check-normaal-in (geef-knopen boom))))
        (else (check-normaal-in (geef-knopen boom)))))

(define (check-normaal-in lst)
  (if (null? lst)
      #t
      (and (check-normaal (car lst))
           (check-normaal-in (cdr lst)))))

(and (equal? (tel hybride-tak) '(4 2 3))
     (check-normaal hybride-tak))
