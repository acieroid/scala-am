(define result '())
(define (display2 item)
  (set! result (cons item result)))
(define (newline2) (set! result (cons 'newline result)))

(define (make-row key name age wage)
  (vector key name age wage))

(define (key-ref row)
  (vector-ref row 0))

(define (name-ref row)
  (vector-ref row 1))

(define (age-ref row)
  (vector-ref row 2))

(define (wage-ref row)
  (vector-ref row 3))

(define (key-set! row value)
  (vector-set! row 0 value))

(define (name-set! row value)
  (vector-set! row 1 value))

(define (age-set! row value)
  (vector-set! row 2 value))

(define (wage-set! row value)
  (vector-set! row 3 value))

(define (show-row row)
  (display2 "[Sleutel:")(display2 (key-ref row))(display2 "]")
  (display2 "[Naam:")(display2 (name-ref row))(display2 "]")
  (display2 "[Leeftijd:")(display2 (age-ref row))(display2 "]")
  (display2 "[Salaris:")(display2 (wage-ref row))(display2 "]"))

(define (make-table rows)
  (make-vector rows 0))

(define (table-size table)
  (vector-length table))

(define (row-ref table pos)
  (if (< pos (table-size table))
      (vector-ref table pos)
      #f))

(define (row-set! table pos row)
  (if (< pos (table-size table))
      (vector-set! table pos row)
      #f))

(define (show-table table)
  (define (iter index)
    (cond ((= index (table-size table)) (newline2))
          (else
           (show-row (row-ref table index))
           (newline2)
           (iter (+ index 1)))))
  (iter 0))


(define table (make-table 10))
(row-set! table 0 (make-row 8 'Bernard 45 120000))
(row-set! table 1 (make-row 3 'Dirk 26 93000))
(row-set! table 2 (make-row 6 'George 48 130000))
(row-set! table 3 (make-row 6 'Greet 27 75000))
(row-set! table 4 (make-row 1 'Kaat 18 69000))
(row-set! table 5 (make-row 5 'Mauranne 21 69000))
(row-set! table 6 (make-row 4 'Peter 33 80000))
(row-set! table 7 (make-row 2 'Piet 25 96000))
(row-set! table 8 (make-row 9 'Tom 26 96000))
(row-set! table 9 (make-row 6 'Veronique 36 115000))

(define expected-result '(newline
                          newline
                          "]"
                          115000
                          "[Salaris:"
                          "]"
                          36
                          "[Leeftijd:"
                          "]"
                          Veronique
                          "[Naam:"
                          "]"
                          6
                          "[Sleutel:"
                          newline
                          "]"
                          96000
                          "[Salaris:"
                          "]"
                          26
                          "[Leeftijd:"
                          "]"
                          Tom
                          "[Naam:"
                          "]"
                          9
                          "[Sleutel:"
                          newline
                          "]"
                          96000
                          "[Salaris:"
                          "]"
                          25
                          "[Leeftijd:"
                          "]"
                          Piet
                          "[Naam:"
                          "]"
                          2
                          "[Sleutel:"
                          newline
                          "]"
                          80000
                          "[Salaris:"
                          "]"
                          33
                          "[Leeftijd:"
                          "]"
                          Peter
                          "[Naam:"
                          "]"
                          4
                          "[Sleutel:"
                          newline
                          "]"
                          69000
                          "[Salaris:"
                          "]"
                          21
                          "[Leeftijd:"
                          "]"
                          Mauranne
                          "[Naam:"
                          "]"
                          5
                          "[Sleutel:"
                          newline
                          "]"
                          69000
                          "[Salaris:"
                          "]"
                          18
                          "[Leeftijd:"
                          "]"
                          Kaat
                          "[Naam:"
                          "]"
                          1
                          "[Sleutel:"
                          newline
                          "]"
                          75000
                          "[Salaris:"
                          "]"
                          27
                          "[Leeftijd:"
                          "]"
                          Greet
                          "[Naam:"
                          "]"
                          6
                          "[Sleutel:"
                          newline
                          "]"
                          130000
                          "[Salaris:"
                          "]"
                          48
                          "[Leeftijd:"
                          "]"
                          George
                          "[Naam:"
                          "]"
                          6
                          "[Sleutel:"
                          newline
                          "]"
                          93000
                          "[Salaris:"
                          "]"
                          26
                          "[Leeftijd:"
                          "]"
                          Dirk
                          "[Naam:"
                          "]"
                          3
                          "[Sleutel:"
                          newline
                          "]"
                          120000
                          "[Salaris:"
                          "]"
                          45
                          "[Leeftijd:"
                          "]"
                          Bernard
                          "[Naam:"
                          "]"
                          8
                          "[Sleutel:"))

(show-table table)
(equal? expected-result result)
