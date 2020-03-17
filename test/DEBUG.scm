(define organigram
    '(directeur
       (hoofd-inkoop)))
(define (collegas-in p oversten organigrammen)
  (collegas p oversten (car organigrammen)))
(define (werknemers-in organigrammen)
  (if (null? organigrammen)
      '()
    (append (car organigrammen)
      (werknemers-in (cdr organigrammen)))))
(define (collegas p oversten organigram)
  (if (eq? p (car organigram))
    (append oversten
      (werknemers-in (cdr organigram)))
    (collegas-in p
      (cons (car organigram) oversten) (cdr organigram))))
(collegas 'hoofd-inkoop '() organigram)