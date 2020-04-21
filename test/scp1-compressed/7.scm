; 7.2
(define (atom? x)
  (not (pair? x)))

(define (depth tree)
  (cond ((null? tree) 0)
    ((atom? tree) 0)
    (else (max (+ 1 (depth (car tree)))
            (depth (cdr tree))))))

(define (leaf-count tree)
  (cond
    ((null? tree) 0)
    ((atom? tree) 1)
    (else (+ (leaf-count (car tree))
            (leaf-count (cdr tree))))))

(define (depth-and-leaf-count tree)
  (define make-res cons)
  (define depth car)
  (define leaf-count cdr)

  (cond
    ((null? tree) (make-res 0 0))
    ((atom? tree) (make-res 0 1))
    (else (let ((res-car (depth-and-leaf-count (car tree)))
                 (res-cdr (depth-and-leaf-count (cdr tree))))
            (make-res (max (+ 1 (depth res-car))
                        (depth res-cdr))
              (+ (leaf-count res-car)
                (leaf-count res-cdr)))))))

(define l '((1 2) ((3 4) 5) (6 7)))
(and (= (depth l) 3)
  (= (leaf-count l) 7)
  (equal? (depth-and-leaf-count l) (cons 3 7)))

; 7.3
;(define (atom? x)
;  (not (pair? x)))

(define (fringe l)
  (cond ((null? l) '())
    ((atom? l) (list l))
    (else (append (fringe (car l))
            (fringe (cdr l))))))

(equal? (fringe '((1) ((((2)))) (3 (4 5) 6) ((7) 8 9))) '(1 2 3 4 5 6 7 8 9))


; 7.4
(define (unfringe-1 l)
  (cond ((null? l) '())
    ((null? (cdr l)) (list (car l)))
    (else (list (car l)
            (unfringe-1 (cdr l))))))

(define (unfringe-2 l)
  (define (pair l)
    (cond ((null? l) '())
      ((null? (cdr l)) (list l))
      (else (cons (list (car l) (cadr l))
              (pair (cddr l))))))

  (let loop ((l l))
    (if (or (null? l)
          (null? (cdr l)))
      l
      (loop (pair l)))))

(and (equal? (unfringe-1 '(1 2 3 4 5 6 7 8 9)) '(1 (2 (3 (4 (5 (6 (7 (8 (9))))))))))
  (equal? (unfringe-2 '(1 2 3 4 5 6 7 8 9)) '(((((1 2) (3 4)) ((5 6) (7 8))) (((9)))))))

; 7.5
;(define (atom? x)
;  (not (pair? x)))

(define (same-structure? l1 l2)
  (cond ((and (atom? l1) (atom? l2)) #t)
    ((or  (atom? l1) (atom? l2)) #f)
    (else (and (same-structure? (car l1) (car l2))
            (same-structure? (cdr l1) (cdr l2))))))

(define (same-structure?-or l1 l2)
  (or (and (atom? l1) (atom? l2))
    (and (pair? l1)
      (pair? l2)
      (same-structure?-or (car l1) (car l2))
      (same-structure?-or (cdr l1) (cdr l2)))))

(and (same-structure? '((1 2) ((3 . 4) ((5 6) ((7 8) (9)))))
         '((a b) ((c . d) ((e f) ((g h) (i))))))
  (not (same-structure? '((1 2) ((3 4) ((5 6) ((7 8) (9)))))
           '((((1 2) (3 4)) ((5 6) (7 8))) 9))))

; 7.6
;(define (atom? x)
;  (not (pair? x)))

(define (deep-combine combiner null-value l)
  (cond ((null? l) null-value)
    ((atom? l) l)
    (else (combiner (deep-combine combiner
                      null-value
                      (car l))
            (deep-combine combiner
              null-value
              (cdr l))))))

(define (deep-map f l)
  (cond
    ((null? l) '())
    ((atom? l) (f l))
    (else (cons (deep-map f (car l))
            (deep-map f (cdr l))))))

(and (= (deep-combine + 0 '((((1 2) (3 4)) ((5 6) (7 8))) 9)) 45)
  (equal? (deep-map (lambda (x) (* x x)) '((((1 . 2) (3 4)) ((5 6) (7 8))) . 9))
      '((((1 . 4) (9 16)) ((25 36) (49 64))) . 81)))

; 7.9
(define boom
    '((blad (appel . golden))
       (blad (appel . granny))
       (((appel . golden) blad) blad (appel . cox))))

(define (blad? boom)
  (eq? boom 'blad))

(define (appel? boom)
  (and (pair? boom) (eq? (car boom) 'appel)))

(define (type appel) (cdr appel))

(define (leafs boom)
  (cond  ((null? boom) 0)
    ((blad? boom) 1)
    ((appel? boom) 0)
    (else (+ (leafs (car boom))
            (leafs (cdr boom))))))

(define (all-apples boom)
  (cond ((null? boom) '())
    ((blad? boom) '())
    ((appel? boom) (list (type boom)))
    (else (append (all-apples (car boom))
            (all-apples (cdr boom))))))

(define (conditional-append l1 l2)
  (cond
    ((null? l1) l2)
    ((member (car l1) l2)(conditional-append (cdr l1) l2))
    (else (cons (car l1)(conditional-append (cdr l1) l2)))))

(define (apple-types boom)
  (cond ((null? boom) '())
    ((blad? boom) '())
    ((appel? boom) (list (type boom)))
    (else (conditional-append (apple-types (car boom))
            (apple-types (cdr boom))))))

(define (bewerk-boom boom doe-blad doe-appel combiner init)
  (cond
    ((null? boom) init)
    ((blad? boom) (doe-blad boom))
    ((appel? boom) (doe-appel boom))
    (else (combiner
            (bewerk-boom (car boom) doe-blad doe-appel combiner init)
            (bewerk-boom (cdr boom) doe-blad doe-appel combiner init)))))

(define (leafs-dmv-bewerk boom)
  (bewerk-boom boom
    (lambda (blad) 1)
    (lambda (appel) 0)
    +
    0))

(define (all-apples-dmv-bewerk boom)
  (bewerk-boom boom
    (lambda(blad) '())
    (lambda(appel) (list (type appel)))
    append
      '()))

(define (apple-types-dmv-bewerk boom)
  (bewerk-boom boom
    (lambda(blad) '())
    (lambda(appel) (list(type  appel)))
    conditional-append
      '()))

(and (= (leafs boom) 4)
  (equal? (all-apples boom) '(golden granny golden cox))
  (equal? (apple-types boom) '(granny golden cox))
  (= (leafs-dmv-bewerk boom) 4)
  (equal? (all-apples-dmv-bewerk boom) '(golden granny golden cox))
  (equal? (apple-types-dmv-bewerk boom) '(granny golden cox)))

; 7.11
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

; 7.12
;(define (atom? x)
;  (not (pair? x)))

;; (define mijn-vuurwerk '(groen ((blauw (X (blauw (X X)) X X))
;;                                 (rood ((groen (X X)) X))
;;                                 X
;;                                 (geel (X X)))))
;; 
;; (define (kleur vuurwerk) (car vuurwerk))
;; (define (takken vuurwerk) (cadr vuurwerk))
;; (define (low-energy? vuurwerk) (eq? vuurwerk 'X))
;; 
;; (define (tel-knallen vuurwerk)
;;   (cond ((null? vuurwerk) 0)
;;     ((low-energy? vuurwerk) 0)
;;     ((atom? vuurwerk) 1)
;;     (else (+ (tel-knallen (car vuurwerk))
;;             (tel-knallen (cdr vuurwerk))))))
;; 
;; (define (tel-low-energies v)
;;   (cond ((null? v) 0)
;;     ((low-energy? v) 1)
;;     ((atom? v) 0)
;;     (else (+ (tel-low-energies (car v))
;;             (tel-low-energies (cdr v))))))
;; 
;; (define (tel-einde-in takken een-kleur)
;;   (cond ((null? takken) 0)
;;     ((low-energy? (car takken)) 0)
;;     (else (+ (tel-einde (car takken) een-kleur)
;;             (tel-einde-in (cdr takken) een-kleur)))))
;; 
;; (define (tel-einde vuurwerk een-kleur)
;;   (if (eq? (kleur vuurwerk) een-kleur)
;;     (tel-low-energies (takken vuurwerk))
;;     (tel-einde-in (takken vuurwerk) een-kleur)))
;; 
;; (define (ster? vuurwerk)
;;   (not (member 'X (takken vuurwerk))))
;; 
;; (and (eq? (kleur mijn-vuurwerk) 'groen)
;;   (equal? (takken mijn-vuurwerk)
;;       '((blauw (X (blauw (X X)) X X)) (rood ((groen (X X)) X)) X (geel (X X))))
;;   (not (low-energy? mijn-vuurwerk))
;;   (low-energy? 'X)
;;   (= (tel-knallen mijn-vuurwerk) 6)
;;   (= (tel-einde mijn-vuurwerk 'blauw) 5)
;;   (not (ster? mijn-vuurwerk)))

; 7.13
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

; 7.14
;(define (atom? x)
;  (not (pair? x)))

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
;(define (knoop? boom) (dier? boom))


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

; 7.15
;(define (atom? x)
;  (not (pair? x)))

(define (maak-blad type) type)
(define (geef-type blad) blad)

(define (maak-knoop deelbomen) deelbomen)
(define (geef-deelbomen boom) boom)

(define (maak-hybride-tak knopen) knopen)
(define (geef-knopen tak) tak)

;(define (leeg? boom) (null? boom))
(define (knoop? boom) (pair? boom))
(define (blad2? boom) (atom? boom))

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
      ((and (blad2? boom) (eq? boom 'appel))
        (list 1 0 0))
      ((and (blad2? boom) (eq? boom 'peer))
        (list 0 1 0))
      ((blad2? boom) (list 0 0 1))
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
    ((blad2? boom) #t)
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

; 7.16
(define foldr ; replace (apply + ...) by (foldr + 0 ...)
  (lambda (f base lst)
    (define foldr-aux
      (lambda (lst)
        (if (null? lst)
          base
          (f (car lst) (foldr-aux (cdr lst))))))
    (foldr-aux lst)))

;(define (atom? x)
;  (not (pair? x)))

(define Coca-Cola-NV
    '(Coca-Cola-NV (Frisdranken
                     (Coca-Cola
                       (Regular-Coca-Cola (Coke (10000000)))
                       (light-Coca-Cola (Coke-Light (800000))
                         (Coke-Zero (200000))))
                     (Fanta (Fanta-Orange (800000))
                       (Fanta-Lemon (200000)))
                     (Sprite (Sprite-Zero (1000000))))
       (Sappen
         (Minute-Maid (Minute-Maid-Sinaas (2000000))
           (Minute-Maid-Tomaat (1000000))))))

(define (omzetcijfer categorie)
  (caadr categorie))

(define (heeft-omzetcijfer categorie)
  (and (pair? categorie)
    (pair? (cadr categorie))
    (atom? (caadr categorie))
    (number? (caadr categorie))))

(define (deel-categorien categorie)
  (cdr categorie))

(define (hoofdcategorie categorie)
  (car categorie))

(define (bereken lst)
  (cond ((null? lst) 0)
    ((atom? lst) 0)
    ((number? (car lst)) (car lst))
    (else (+ (bereken (car lst))
            (bereken (cdr lst))))))

(define (omzet bedrijf categorie)
  (if (eq? (hoofdcategorie bedrijf) categorie)
    (bereken bedrijf)
    (omzet-in (deel-categorien bedrijf) categorie)))

(define (omzet-in lst categorie)
  (if (null? lst)
    #f
    (or (omzet (car lst) categorie)
      (omzet-in (cdr lst) categorie))))

(define (collect-pairs bedrijf)
  (cond ((heeft-omzetcijfer bedrijf)
          (list (list (hoofdcategorie bedrijf)
                  (omzetcijfer bedrijf))))
    (else (collect-pairs-in (deel-categorien bedrijf)))))

(define (collect-pairs-in lst)
  (if (null? lst)
      '()
    (append (collect-pairs (car lst))
      (collect-pairs-in (cdr lst)))))

(define (verdeel-democratisch bedrijf budget)
  (let* ((pairs (collect-pairs bedrijf))
          (total (foldr + 0 (map cadr pairs)))
          (factor (/ budget total)))
    (map (lambda (x) (list (car x) (* factor (cadr x))))
      pairs)))

(define (verdeel bedrijf budget)
  (if (heeft-omzetcijfer bedrijf)
    (list (hoofdcategorie bedrijf) budget)
    (let* ((rest (deel-categorien bedrijf))
            (new-budget (/ budget (length rest))))
      (cons (hoofdcategorie bedrijf)
        (verdeel-in rest new-budget)))))

(define (verdeel-in lst budget)
  (if (null? lst)
      '()
    (cons (verdeel (car lst) budget)
      (verdeel-in (cdr lst) budget))))

(and (= (omzet Coca-Cola-NV 'Coca-Cola) 11000000)
  (= (omzet Coca-Cola-NV 'Sprite) 1000000)
  (= (omzet Coca-Cola-NV 'Minute-Maid) 3000000)
  (equal? (verdeel-democratisch Coca-Cola-NV 128000000) '((Coke 80000000) (Coke-Light 6400000) (Coke-Zero 1600000) (Fanta-Orange 6400000) (Fanta-Lemon 1600000) (Sprite-Zero 8000000) (Minute-Maid-Sinaas 16000000) (Minute-Maid-Tomaat 8000000)))
  (equal? (verdeel Coca-Cola-NV 1200000) '(Coca-Cola-NV (Frisdranken (Coca-Cola (Regular-Coca-Cola (Coke 100000)) (light-Coca-Cola (Coke-Light 50000) (Coke-Zero 50000))) (Fanta (Fanta-Orange 100000) (Fanta-Lemon 100000)) (Sprite (Sprite-Zero 200000))) (Sappen (Minute-Maid (Minute-Maid-Sinaas 300000) (Minute-Maid-Tomaat 300000))))))

; 7.17
(define familieboom '(jan (piet (frans (tom)
                                  (roel))
                            (mie))
                       (bram (inge (bert (ina)
                                     (ilse))
                               (bart))
                         (iris))
                       (joost (else (ilse)))))



(define (familiehoofd fam) (car fam))
(define (kinderen fam) (cdr fam))
(define (laatste-nakomeling? fam)
  (null? (kinderen fam)))

(define (verdeel-democratisch2 boom budget)
  (define (verdeel boom)
    (if (laatste-nakomeling? boom)
      1
      (+ 1 (verdeel-in (kinderen boom)))))

  (define (verdeel-in lst)
    (if (null? lst)
      0
      (+ (verdeel (car lst))
        (verdeel-in (cdr lst)))))
  (/ budget (verdeel-in (kinderen boom))))

(define (budget boom budget-list)
  (define (budget-hulp boom budget-list)
    (+ (car budget-list)
      (budget-hulp-in (kinderen boom) (cdr budget-list))))

  (define (budget-hulp-in bomen budget-list)
    (if (or (null? bomen)(null? budget-list))
      0
      (+ (budget-hulp    (car bomen) budget-list)
        (budget-hulp-in (cdr bomen) budget-list))))
  (budget-hulp-in (kinderen boom) budget-list))

(define (verdeel2 boom budget)
  (cond ((laatste-nakomeling? boom)
          (list (list (familiehoofd boom) budget)))
    (else (let* ((rest (kinderen boom))
                  (new-budget (/ budget (length rest))))
            (verdeel2-in rest new-budget)))))

(define (verdeel2-in bomen budget)
  (if (null? bomen)
      '()
    (append (verdeel2    (car bomen) budget)
      (verdeel2-in (cdr bomen) budget))))

(and (= (verdeel-democratisch2 familieboom 1500) 100)
  (= (budget familieboom '(100 50 20)) 650)
  (equal? (verdeel2 familieboom 3000) '((tom 250) (roel 250) (mie 500) (ina 125) (ilse 125) (bart 250) (iris 500) (ilse 1000))))

; 7.18
;(define (atom? x)
;  (not (pair? x)))

(define VUB-circus '(ann (mien (eef (bas)
                                 (bob))
                           (els (jan)
                             (jos))
                           (eva (tom)
                             (tim)))
                      (mies (ine (cas)
                              (cor))
                        (ils (rik)
                          (raf))
                        (ines (stef)
                          (staf)))))



(define (hoofdartiest piramide) (car piramide))
(define (artiesten piramide) (cdr piramide))
(define (artiest? piramide)
  (and (pair? piramide) (atom? (car piramide))))
(define (onderaan? piramide) (null? (cdr piramide)))

(define (jump piramide artiest)
  (define (jump-hulp piramide pad)
    (if (and (artiest? piramide)
          (eq? (hoofdartiest piramide) artiest))
      pad
      (jump-in (artiesten piramide)
        (cons (hoofdartiest piramide) pad))))

  (define (jump-in lst pad)
    (if (null? lst)
      #f
      (or (jump-hulp (car lst) pad)
        (jump-in   (cdr lst) pad))))
  (reverse (jump-hulp piramide '())))

(define (fall piramide artiest)
  (define (fall-hulp piramide pad)
    (if (and (artiest? piramide)
          (eq? (hoofdartiest piramide) artiest))
      (append pad
              (append (list (hoofdartiest piramide))
                      (map hoofdartiest (artiesten piramide)))))
    (fall-in (artiesten piramide)
      (append pad
        (list (hoofdartiest piramide)))))

  (define (fall-in lst pad)
    (if (null? lst)
      #f
      (or (fall-hulp (car lst) pad)
        (fall-in (cdr lst) pad))))
  (fall-hulp piramide '()))

(and (equal? (jump VUB-circus 'eva) '(ann mien))
  (equal? (jump VUB-circus 'stef) '(ann mies ines))
  (not (or (fall VUB-circus 'eva) (fall VUB-circus 'stef) (fall VUB-circus 'mies))))
