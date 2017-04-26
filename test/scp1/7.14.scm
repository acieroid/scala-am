(define (atom? x)
  (not (pair? x)))

(define (maak-dier naam eigenschappen)
  (list naam eigenschappen))

(define (naam dier) (car dier))
(define (eigenschappen dier) (cadr dier))

(define (dier? dier)
  (and (pair? dier)
       (atom? (naam dier))
       (pair? (eigenschappen dier))))

(define (maak-boom knoop deelbomen)
  (list knoop deelbomen))

(define (knoop boom) (car boom))
(define (deelbomen boom) (cadr boom))
(define (leeg? boom) (null? boom))
(define (knoop? boom) (dier? boom))


(define classificatieboom
  (maak-boom (maak-dier 'dier '(kan-ademen kan-bewegen))
             (list
              (maak-boom
               (maak-dier 'vis
                          '(kan-zwemmen heeft-schubben heeft-vinnen))
               (list
                (maak-dier 'ballonvis
                           '(kan-zwellen is-geel))))
              (maak-boom
               (maak-dier 'landdier
                          '(heeft-huid kan-lopen heeft-poten))
               (list (maak-dier 'olifant
                                '(is-groot))))
              (maak-boom
               (maak-dier 'vogel
                          '(kan-vliegen heeft-vleugels heeft-veren))
               (list
                (maak-dier 'kanarie
                           '(kan-zingen is-geel))
                (maak-dier 'arend
                           '(is-groot)))))))

(define (all-kinds boom)
  (cond ((leeg? boom) '())
        ((dier? boom) (list (naam boom)))
        ((dier? (knoop boom))
         (append (list (naam (knoop boom)))
                 (all-kinds-in (deelbomen boom))))
        (else (all-kinds-in (deelbomen boom)))))

(define (all-kinds-in lst)
    (if (null? lst)
        '()
        (append (all-kinds (car lst))
                (all-kinds-in (cdr lst)))))

(define (geef-eigenschappen boom soort)
  (define (geef-eig boom eig)
    (cond ((dier? boom)
           (if (eq? (naam boom) soort)
               (append eig
                       (list (eigenschappen boom)))
               #f))
          ((and (dier? (knoop boom))
                (eq? (naam (knoop boom)) soort))
           (append eig (eigenschappen (knoop boom))))
          (else (geef-eig-in (deelbomen boom)
                             (append eig
                                     (eigenschappen (knoop boom)))))))

  (define (geef-eig-in lst eig)
    (cond ((null? lst) #f)
          (else (or (geef-eig (car lst) eig)
                    (geef-eig-in (cdr lst) eig)))))
  (geef-eig boom '()))

(define (ask? boom soort eig)
  (let ((eigenschappen (geef-eigenschappen boom soort)))
    (pair? (memq eig eigenschappen))))

(and (equal? (all-kinds classificatieboom)
             '(dier vis ballonvis landdier olifant vogel kanarie arend))
     (ask? classificatieboom 'landdier  'kan-lopen)
     (ask? classificatieboom 'ballonvis 'heeft-vinnen)
     (not (ask? classificatieboom 'olifant   'kan-vliegen)))