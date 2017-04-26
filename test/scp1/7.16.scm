(define (atom? x)
  (not (pair? x)))

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
         (total (apply + (map cadr pairs)))
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
