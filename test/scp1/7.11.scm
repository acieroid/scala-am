(define organigram
  '(directeur
    (hoofd-verkoop (verkoopsleider-vlaanderen)
                   (verkoopsleider-brussel))
    (hoofd-productie (hoofd-inkoop (bediende1)
                                   (bediende2)
                                   (bediende3))
                     (hoofd-fakturen))
    (hoofd-administratie (hoofd-personeel)
                         (hoofd-boekhouding))))

(define (baas organigram) (car organigram))
(define (sub-organigrammen organigram) (cdr organigram))

(define (hierarchisch? p1 p2 organigram)
  (define (hierarchisch?-in path organigrammen)
    (if (null? organigrammen)
        #f
        (or (hierarchisch? path (car organigrammen))
            (hierarchisch?-in path (cdr organigrammen)))))

  (define (hierarchisch? path organigram)
    (cond
      ((and (eq? p1 (baas organigram)) (member p2 path)) #t)
      ((and (eq? p2 (baas organigram)) (member p1 path)) #t)
      (else (hierarchisch?-in (cons (baas organigram) path)
                              (sub-organigrammen organigram)))))
  (hierarchisch? '() organigram))

(define (collegas p organigram)
  (define (collegas-in oversten organigrammen)
    (if (null? organigrammen)
        #f
        (or (collegas oversten (car organigrammen))
            (collegas-in oversten (cdr organigrammen)))))

  (define (werknemers-in organigrammen)
    (if (null? organigrammen)
        '()
        (append (werknemers (car organigrammen))
                (werknemers-in (cdr organigrammen)))))

  (define (werknemers organigram)
    (cons (baas organigram)
          (werknemers-in (sub-organigrammen organigram))))

  (define (collegas oversten organigram)
    (if (eq? p (baas organigram))
        (append oversten
                (werknemers-in (sub-organigrammen organigram)))
        (collegas-in (cons (baas organigram) oversten)
                     (sub-organigrammen organigram))))
(collegas '() organigram))

(and (hierarchisch? 'directeur 'verkoopsleider-brussel organigram)
     (hierarchisch? 'bediende1 'hoofd-productie organigram)
     (not (hierarchisch? 'hoofd-personeel 'bediende3 organigram))
     (equal? (collegas 'hoofd-inkoop organigram) '(hoofd-productie directeur bediende1 bediende2 bediende3)))
