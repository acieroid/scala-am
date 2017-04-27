(define (member x list)
  (if (null? list)
      #f
      (if (equal? x (car list))
          #t
          (member x (cdr list)))))

(define (atom? x)
  (not (pair? x)))

(define mijn-vuurwerk '(groen ((blauw (X (blauw (X X)) X X))
                              (rood ((groen (X X)) X))
                              X
                              (geel (X X)))))

(define (kleur vuurwerk) (car vuurwerk))
(define (takken vuurwerk) (cadr vuurwerk))
(define (low-energy? vuurwerk) (eq? vuurwerk 'X))

(define (tel-knallen vuurwerk)
  (cond ((null? vuurwerk) 0)
        ((low-energy? vuurwerk) 0)
        ((atom? vuurwerk) 1)
        (else (+ (tel-knallen (car vuurwerk))
                 (tel-knallen (cdr vuurwerk))))))

(define (tel-low-energies v)
  (cond ((null? v) 0)
        ((low-energy? v) 1)
        ((atom? v) 0)
        (else (+ (tel-low-energies (car v))
                 (tel-low-energies (cdr v))))))

(define (tel-einde-in takken een-kleur)
  (cond ((null? takken) 0)
        ((low-energy? (car takken)) 0)
        (else (+ (tel-einde (car takken) een-kleur)
                 (tel-einde-in (cdr takken) een-kleur)))))

(define (tel-einde vuurwerk een-kleur)
  (if (eq? (kleur vuurwerk) een-kleur)
      (tel-low-energies (takken vuurwerk))
      (tel-einde-in (takken vuurwerk) een-kleur)))

(define (ster? vuurwerk)
  (not (member 'X (takken vuurwerk))))

(and (eq? (kleur mijn-vuurwerk) 'groen)
     (equal? (takken mijn-vuurwerk)
             '((blauw (X (blauw (X X)) X X)) (rood ((groen (X X)) X)) X (geel (X X))))
     (not (low-energy? mijn-vuurwerk))
     (low-energy? 'X)
     (= (tel-knallen mijn-vuurwerk) 6)
     (= (tel-einde mijn-vuurwerk 'blauw) 5)
     (not (ster? mijn-vuurwerk)))
