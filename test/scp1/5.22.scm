(define (totaal aankopen kortingen)

  (define (zoek-korting kortingen artikel)
    (apply +
           (map
            (lambda (x) (if (eq? (car x) artikel)
                            (cadr x)
                            0))
            kortingen)))

  (if (null? aankopen)
      0
      (let* ((aankoop (car aankopen))
             (korting (zoek-korting kortingen (car aankoop)))
             (prijs (cadr aankoop)))
        (+ (- prijs (/ (* prijs korting) 100))
           (totaal (cdr aankopen) (cdr kortingen))))))

(define (totaal-iter aankopen kortingen)

  (define (zoek-korting kortingen artikel)
    (apply + (map
              (lambda (x) (if (eq? (car x) artikel)
                              (cadr x)
                              0))
              kortingen)))

  (define (loop lst res)
    (if (null? lst)
        res
        (let* ((aankoop (car lst))
               (korting (zoek-korting kortingen (car aankoop)))
               (prijs (cadr aankoop)))
          (loop (cdr lst)
                (+ res (- prijs
                          (/ (* prijs korting) 100)))))))
  (loop aankopen 0))

(define Z&Mkortingen '((jas 50) (kleed 50) (rok 30) (trui 20)))

(and (= (totaal '((jas 100) (trui 25) (rok 70) (t-shirt 20))
                '((jas 50) (kleed 50) (rok 30) (trui 20)))
        139)
     (= (totaal-iter '((jas 100) (trui 25) (rok 70) (t-shirt 20))
                     '((jas 50) (kleed 50) (rok 30) (trui 20)))
        139))