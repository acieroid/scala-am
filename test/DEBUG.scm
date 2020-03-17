(define organigram
    '(directeur
       (hoofd-inkoop)))
(define (collegas p oversten organigram)
  (if (eq? p (car organigram))
    (append oversten (append '(1) '(2)))
    (collegas p
      (car organigram) (cadr organigram))))
(collegas 'hoofd-inkoop '() organigram)