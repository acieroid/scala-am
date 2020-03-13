(define organigram
    '(directeur
         (hoofd-inkoop)
         (hoofd-fakturen)))

(define (collegas p organigram)
  (define (collegas-in oversten organigrammen)
    (if (null? organigrammen)
      #f
      (or (collegas oversten (car organigrammen))
        (collegas-in oversten (cdr organigrammen)))))

  (define (werknemers-in organigrammen)
    (if (null? organigrammen)
        '()
      (append (car organigrammen)
        (werknemers-in (cdr organigrammen)))))

  (define (collegas oversten organigram)
    (if (eq? p (car organigram))
      (append oversten
        (werknemers-in (cdr organigram)))
      (collegas-in (cons (car organigram) oversten)
        (cdr organigram))))
  (collegas '() organigram))

(collegas 'hoofd-inkoop organigram)
