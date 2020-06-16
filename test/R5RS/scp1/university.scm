(define result '())
(define display2 (lambda (i) (set! result (cons i result))))
(define newline2 (lambda () (set! result (cons 'newline result))))

(define VUBOrganigram
  '(VUB (academisch (rectoraat)
                    (faculteiten
                     (rechten (bachelor (ba-rechten)
                                        (ba-criminologie))
                              (master (ma-rechten)
                                      (ma-criminologie)))
                     (economie)
                     (wetenschappen (bachelor (ba-wiskunde)
                                              (ba-fysica)
                                              (ba-cw))
                                    (master (ma-wiskunde)
                                            (ma-fysica)
                                            (ma-cw)))))
        (administratief (personeel) (financien))))

(define (display-n n d)
  (if (> n 0)(begin (display2 d)(display-n (- n 1) d))))

(define (print-lijn aantalblanco tekst)
  (display-n aantalblanco " ")
  (display2 tekst)
  (newline2))

(define (label organigram)  (car organigram))
(define (takken organigram) (cdr organigram))

(define (organigram-member-in een-label organigrammen)
  (if (null? organigrammen)
      #f
      (or (organigram-member een-label (car organigrammen))
          (organigram-member-in een-label (cdr organigrammen)))))

(define (organigram-member een-label organigram)
  (if (eq? een-label (label organigram))
      organigram
      (organigram-member-in een-label (takken organigram))))

(define (print organigram)
  (define (print diepte organigram)
    (print-lijn diepte (label organigram))
    (for-each (lambda (organigram)
                (print (+ diepte 1) organigram))
              (takken organigram)))
  (print 0 organigram))

(define (print-vanaf organigram label)
  (let ((res (organigram-member label organigram)))
    (if res
        (print res)
        #f)))

(print-vanaf VUBOrganigram 'rechten)

(define (print-tot organigram niveau)
  (define (print-tot organigram niveau max-niveau)
    (cond ((<= niveau max-niveau)
           (print-lijn niveau (label organigram))
           (for-each
            (lambda (organigram)
              (print-tot organigram (+ niveau 1) max-niveau))
            (takken organigram)))))
  (print-tot organigram 0 niveau))

(print-tot VUBOrganigram 2)
(equal? result
        '(newline
          financien
          " "
          " "
          newline
          personeel
          " "
          " "
          newline
          administratief
          " "
          newline
          faculteiten
          " "
          " "
          newline
          rectoraat
          " "
          " "
          newline
          academisch
          " "
          newline
          VUB
          newline
          ma-criminologie
          " "
          " "
          newline
          ma-rechten
          " "
          " "
          newline
          master
          " "
          newline
          ba-criminologie
          " "
          " "
          newline
          ba-rechten
          " "
          " "
          newline
          bachelor
          " "
          newline
          rechten))
