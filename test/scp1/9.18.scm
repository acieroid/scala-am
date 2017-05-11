(define (first-el best)
  (if (not (null? best))
      (caar best)
      #f))

(define (smaller? el1 el2)
  (string<? (symbol->string el1) (symbol->string el2)))

(define (same? el1 el2)
  (equal? el1 el2))


(define (merge best1 best2)
  (define (merge-in curr1 curr2 prev)
    (cond ((null? curr1) (set-cdr! prev curr2))
          ((null? curr2) (set-cdr! prev curr1))
          ((same? (first-el curr1) (first-el curr2))
           (set-cdr! prev curr1)
           (merge-in (cdr curr1) (cdr curr2) curr1))
          ((smaller? (first-el curr1) (first-el curr2))
           (set-cdr! prev curr1)
           (merge-in (cdr curr1) curr2 curr1))
          (else
           (set-cdr! prev curr2)
           (merge-in curr1 (cdr curr2) curr2))))

  (let* ((result (if (smaller? (first-el best1) (first-el best2))
                     best1
                     best2))
         (curr1 (if (eq? result best1) (cdr best1) best1))
         (curr2 (if (eq? result best2) (cdr best2) best2)))
    (merge-in curr1 curr2 result)
    result))

(define best1 '((ann (meiboomstraat 12 1820 Eppegem))
                (bert (populierendreef 7 1050 Brussel))
                (kurt (Mechelsesteenweg 50 1800 Vilvoorde))))

(define best2 '((bert (populierendreef 7 1050 Brussel))
                (jan (eikestraat 1 9000 Gent))
                (sofie (boerendreef 5  2800 Mechelen))))

(equal? (merge best1 best2)
        '((ann (meiboomstraat 12 1820 Eppegem)) (bert (populierendreef 7 1050 Brussel)) (jan (eikestraat 1 9000 Gent)) (kurt (Mechelsesteenweg 50 1800 Vilvoorde)) (sofie (boerendreef 5 2800 Mechelen))))
