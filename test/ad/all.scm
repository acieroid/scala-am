(define (bubble-sort vector)
  (define (swap vector index1 index2)
    (let ((temp (vector-ref vector index1)))
      (vector-set! vector index1 (vector-ref vector index2))
      (vector-set! vector index2 temp)))
  (define (bubble index)
    (define (bubble-iter index1 changed)
        (cond ((<= index1 index)
               (if(> (vector-ref vector index1)
                     (vector-ref vector (+ index1 1)))
		  (begin
                      (swap vector index1 (+ index1 1))
                      (set! changed #t)))
               (bubble-iter (+ index1 1) changed))
              (else changed)))
    (bubble-iter 0 #f))
  (define (bubble-sort-iter index)
    (if(>= index 0)
       (if (bubble index)
           (bubble-sort-iter (- index 1)))))
  (bubble-sort-iter (- (vector-length vector) 2)))

(define vect (vector 9 5 1 7 8 9 4 6 2 3 ))
(bubble-sort vect)
(equal? vect (vector 1 2 3 4 5 6 7 8 9 9))(define (selection-sort vector)
  (define (swap vector index1 index2)
    (let ((temp (vector-ref vector index1)))
      (vector-set! vector index1 (vector-ref vector index2))
      (vector-set! vector index2 temp)))

  (define (pos-of-min vector low high)
    (define (min-iter index pos-of-min-so-far)
      (if (<= index high)
	  (if (< (vector-ref vector index)
		 (vector-ref vector pos-of-min-so-far))
	      (min-iter (+ index 1) index)
	      (min-iter (+ index 1) pos-of-min-so-far))
	  pos-of-min-so-far))
    (min-iter (+ low 1) low))

  (let ((high (- (vector-length vector) 1)))
    (define (selection-sort-iter index)
      (if (< index high)
          (begin
	     (swap vector index (pos-of-min  vector index high))
             (selection-sort-iter (+ index 1)))))
    (selection-sort-iter 0)))


(define vect2  (vector 5 7 0 9 6 4 3 8 2 1))
(selection-sort vect2)
(equal? vect2 (vector 0 1 2 3 4 5 6 7 8 9))

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
(define (create-dictionary)
  (let ((content '()))
    (define (empty?)
      (null? content))
    (define (insert key info)
      (let ((temp (assoc key content)))
	(if temp
	    (set-cdr! temp info)
	    (set! content (cons (cons key info) content))))
      #t)
    (define (delete key)
      (define (remove-iter current prev)
	(cond
	 ((null? current) #f)
	 ((eq? key (caar current))
	  (if (null? prev)
	      (set! content (cdr content))
	      (set-cdr! prev (cdr current)))
	  #t)
	 (else (remove-iter (cdr current) current))))
      (remove-iter content '()))
    (define (lookup key)
      (let ((temp (assoc key content)))
	(if temp
	    (cdr temp)
	    #f)))
    (define (map a-function)
      (define (map-iter the-current result)
	(if (null? the-current)
	    (reverse result)
	    (map-iter (cdr the-current) 
		      (cons (a-function (caar the-current) (cdar the-current))
			    result))))
      (map-iter content '()))
    (define (foreach a-action)
      (define (foreach-iter the-current)
	(cond
	 ((null? the-current) #t)
	 (else
	  (a-action (caar the-current) (cdar the-current))
	  (foreach-iter (cdr the-current)))))
      (foreach-iter content)
      #t)
    (define (display-dict)
      (foreach (lambda (key info)
		 (display key)
		 (display " ")
		 (display info)
		 (newline))))
    (define (dispatch msg args)
      (cond
       ((eq? msg 'empty?) (empty?))
       ((eq? msg 'insert) (insert (car args) (cadr args)))
       ((eq? msg 'delete) (delete (car args)))
       ((eq? msg 'lookup) (lookup (car args)))
       ((eq? msg 'map) (map (car args)))
       ((eq? msg 'foreach) (foreach (car args)))
       ((eq? msg 'display) (display-dict))
       (else (error "unknown request -- create-dictionary" msg))))
    dispatch))

(define nl->fr (create-dictionary))
(nl->fr 'insert '(fiets (bicyclette)))
(nl->fr 'insert '(auto (voiture)))
(nl->fr 'insert '(huis (maison)))
(nl->fr 'insert '(vrachtwagen (camion)))
(nl->fr 'insert '(tientonner (camion)))
(nl->fr 'lookup '(fiets))
(nl->fr 'display '())

(define fr->eng (create-dictionary))
(fr->eng 'insert '(bicyclette (bike)))
(fr->eng 'insert '(voiture (car)))
(fr->eng 'insert '(maison (house home)))
(fr->eng 'insert '(camion (truck)))
(fr->eng 'lookup '(bicyclette))

#t
(define (my-++ n) (+ n 1))

(define (my--- n) (- n 1))

(define false #f)

(define true #t)

(define nil '())

(define (key x) x)

(define (make-heap a-vector nr-of-elements)
  (define (iter index)
    (cond ((> index 0)
           (sift-down a-vector index nr-of-elements)
           (iter (my--- index)))))
  (iter (quotient nr-of-elements 2)))

(define (sift-down heap from to)
  (define (smallest-child parent)
    (let* ((child1 (* 2 parent))
           (child2 (my-++ child1)))
      (cond ((> child1 to) false)
            ((> child2 to) child1)
            ((< (key (vector-ref heap child1))
                (key (vector-ref heap child2)))
             child1)
            (else child2))))
  (define (iter parent)
    (let ((child (smallest-child parent)))
      (if child
          (cond ((> (key (vector-ref heap parent))
                    (key (vector-ref heap child)))
                 (swap heap child parent)
                 (iter child))))))
  (iter from))

(define (swap a-vector i1 i2)
  (let ((temp (vector-ref a-vector i1)))
    (vector-set! a-vector i1 (vector-ref a-vector i2))
    (vector-set! a-vector i2 temp)))

(define (sift-up heap from)
  (define (iter child)
    (let ((parent (quotient child 2)))
      (if (> parent 0)
          (cond ((> (key (vector-ref heap parent))
                    (key (vector-ref heap child)))
                 (swap heap child parent)
                 (iter parent))))))
  (iter from))

(define (create-heap size)
  (cons 0 (make-vector (my-++ size))))

(define (is-empty? heap)
  (eq? (car heap) 0))

(define (insert heap item)
  (let* ((content (cdr heap))
         (new-nr-of-elements (my-++ (car heap)))
         (size (my--- (vector-length content))))
    (display "insert    ")
    (if (> new-nr-of-elements size)
        false
        (begin
          (vector-set! content new-nr-of-elements item)
          (sift-up content new-nr-of-elements)
          (set-car! heap new-nr-of-elements)))
    (display heap)(newline)))

(define v (vector 'lol 5 8 1 3 9 10 2 0))

(make-heap v 8)
(equal? v (vector 'lol 0 3 1 5 9 10 2 8))
(define (quick-sort a-list)
  (define (rearrange pivot some-list)
    (define (rearrange-iter rest result)
      (if(null? rest)
         result
         (if( <= (car rest) pivot)
            (rearrange-iter (cdr rest)
                            (cons (cons (car rest)
                                        (car result))
                                  (cdr result)))
            (rearrange-iter (cdr rest)
                            (cons (car result)
                                  (cons (car rest)
                                        (cdr result)))))))
    (rearrange-iter some-list (cons '() '())))
  (if (<= (length a-list) 1)
      a-list
      (let* ((pivot (car a-list))
             (sub-lists (rearrange pivot (cdr a-list))))
        (append (quick-sort (car sub-lists))
                (append (list pivot)
                        (quick-sort (cdr sub-lists)))))))

(equal? (quick-sort '(9 8 7 6 5 4 3 2 1 0 9)) '(0 1 2 3 4 5 6 7 8 9 9))
(define (insertion-sort vector)
  (let ((high (- (vector-length vector) 1)))
    (define (shift-left vector index)
      (vector-set! vector (- index 1) (vector-ref vector index)))
    (define (insert-sort-iter index1)
      (define (insert index1)
        (let ((insert-value (vector-ref vector (- index1 1))))
          (define (insert-iter index2)
              (cond ((and (<= index2 high)
                          (< (vector-ref vector index2)
                             insert-value))
                     (shift-left vector index2)
                     (insert-iter (+ index2 1)))
                    (else (vector-set! vector (- index2 1) insert-value))))
        (insert-iter index1)))
      (if (> index1 0)
	  (begin
             (insert index1)
             (insert-sort-iter (- index1 1)))))
    (insert-sort-iter high)))

(define vect3 (vector 5 2 7 1 0 9 8 6 3 4))
(insertion-sort vect3)
(equal? vect3 (vector 0 1 2 3 4 5 6 7 8 9))
; (define true #t)
;(define false #f)

(define (make-item priority element)
  (cons priority element))

(define (get-priority item) (car item))

(define (get-element item) (cdr item))

(define (create-priority-queue)
  (let ((front (cons 'boe '())))
    (define (content) (cdr front))
    (define (insert-after! cell item)
      (let ((new-cell (cons item '())))
        (set-cdr! new-cell (cdr cell))
        (set-cdr! cell new-cell)))
    (define (find-prev-cell priority)
      (define (find-iter rest prev)
        (cond
          ((null? rest) prev)
          ((> (get-priority (car rest)) priority)
           (find-iter (cdr rest) rest))
          (else prev)))
      (find-iter (content) front))
    (define (empty?)
      (null? (content)))
    (define (enqueue priority element)
      (insert-after! (find-prev-cell priority)
                     (make-item priority element))
      true)
    (define (dequeue)
      (if (null? (content))
          false
          (let ((temp (car (content))))
            (set-cdr! front (cdr (content)))
            (get-element temp))))
    (define (serve)
      (if (null? (content))
          false
          (get-element (car (content)))))
    (define (dispatch m)
      (cond
        ((eq? m 'empty?) empty?)
        ((eq? m 'enqueue) enqueue)
        ((eq? m 'dequeue) dequeue)
        ((eq? m 'serve) serve)
        (else
          (error "unknown request
                 -- create-priority-queue" m))))
    dispatch))

(define pq (create-priority-queue))
((pq 'enqueue) 66 'Patrick)
((pq 'enqueue) -106 'Octo)
((pq 'enqueue) 0 'Sandy)
((pq 'enqueue) 89 'Spongebob)
((pq 'dequeue))
(equal? ((pq 'dequeue)) 'Patrick)
(define (copy from-vector to-vector from-index to-index)
  (vector-set! to-vector to-index (vector-ref from-vector from-index)))

(define (move from-vector to-vector from-low from-high to-index)
  (define (move-iter n)
    (if (<= (+ from-low n) from-high)
        (begin
          (copy from-vector to-vector (+ from-low n) (+ to-index n))
          (move-iter (+ n 1)))))
  (move-iter 0))

(define (merge vector1 vector2 vector low1 high1 low2 high2 to-index)
  (define (merge-iter index index1 index2)
    (cond
      ((> index1 high1)
       (move vector2 vector index2 high2 index))
      ((> index2 high2)
       (move vector1 vector index1 high1 index))
      ((< (vector-ref vector1 index1) (vector-ref vector2 index2))
       (copy vector1 vector index1 index)
       (merge-iter (+ index 1) (+ index1 1) index2))
      (else
        (copy vector2 vector index2 index)
        (merge-iter (+ index 1) index1 (+ index2 1)))))
  (merge-iter to-index low1 low2))

(define (bottom-up-merge-sort vector)
  (define (merge-subs len)
    (let ((aux-vector (make-vector (vector-length vector) 0)))
      (define (merge-subs-iter index)
        (cond
          ((< index (- (vector-length vector) (* 2 len)))
           (merge vector vector aux-vector index (+ index len -1) (+ index len)
                  (+ index len len -1) index)
           (move aux-vector vector index (+ index len len -1) index)
           (merge-subs-iter (+ index len len)))
          ((< index (- (vector-length vector) len))
           (merge vector vector aux-vector index (+ index len -1) (+ index len)
                  (- (vector-length vector) 1) index)
           (move aux-vector vector index (- (vector-length vector) 1) index))
          (else #f)))
      (merge-subs-iter 0)))
  (define (merge-sort-iter len)
    (if(< len (vector-length vector))
       (begin
	 (merge-subs len)
	 (merge-sort-iter (* 2 len)))))
  (merge-sort-iter 1))

(let ((aVector (vector 8 3 6 6 0 5 4 2 9 6)))
  (bottom-up-merge-sort aVector)
  (equal? aVector (vector 0 2 3 4 5 6 6 6 8 9)))
(define (quick-sort2 vector)
  (define (swap v index1 index2)
    (let ((temp (vector-ref v index1)))
      (vector-set! v index1 (vector-ref v index2))
      (vector-set! v index2 temp)))
  (define (quick-sort-aux low high)
    (define (quick-sort-aux-iter mid-value from to)
      (define (quick-right index1)
        (if (and (< index1 high)(< (vector-ref vector index1) mid-value))
            (quick-right (+ index1 1))
            index1))
      (define (quick-left index2)
        (if (and (> index2 low)(> (vector-ref vector index2) mid-value))
            (quick-left (- index2 1))
            index2))
      (let ((index1 (quick-right (+ from 1)))
            (index2 (quick-left to)))
        (cond ((< index1 index2)
               (swap vector index1 index2)
               (quick-sort-aux-iter mid-value index1 index2))
              (else index2))))
    (if (< low high)
	(let ((middle (quotient (+ low high) 2))
	      (pivot-index (+ low 1)))
           (swap vector  middle pivot-index)
           (if(> (vector-ref vector pivot-index)
                 (vector-ref vector high))
              (swap vector pivot-index high))
           (if(> (vector-ref vector  low)
                 (vector-ref vector high))
              (swap vector low high))
           (if(< (vector-ref vector pivot-index)
                 (vector-ref vector low))
              (swap vector pivot-index low))
           (let ((mid-index
                   (quick-sort-aux-iter (vector-ref vector pivot-index)
                                        (+ low 1) high )))
             (swap vector mid-index pivot-index)
             (quick-sort-aux low (- mid-index 1))
             (quick-sort-aux (+ mid-index 1) high)))))
  (quick-sort-aux 0 (- (vector-length vector) 1)))

(define test3 (vector 8 3 6 6 1 5 4 2 9 6))
(quick-sort2 test3)
(equal? test3 (vector 1 2 3 4 5 6 6 6 8 9))
;(define false #f)
;(define true #t)

(define (create-stack eq-fnct)
  (let ((content '()))
    (define (empty?)
      (null? content))
    (define (push element)
      (set! content (cons element content))
      #t)
    (define (pop)
      (if (null? content)
          #f
          (let ((temp (car content)))
            (set! content (cdr content))
            temp)))
    (define (top)
      (if (null? content)
          #f
          (car content)))
    (define (is-in element)
      (if (member element content)
          #t
          #f))
    (define (dispatch m)
      (cond
        ((eq? m 'empty?) empty?)
        ((eq? m 'push) push)
        ((eq? m 'pop) pop)
        ((eq? m 'top) top)
        ((eq? m 'is-in) is-in)
        (else (error "unknown request -- create-stack" m))))
    dispatch))

    (let ((stack (create-stack =)))
      (and ((stack 'empty?))
           (begin
             ((stack 'push) 13)
             (not ((stack 'empty?))))
           ((stack 'is-in) 13)
           (= ((stack 'top)) 13)
           (begin ((stack 'push) 14)
                  (= ((stack 'pop)) 14))))
