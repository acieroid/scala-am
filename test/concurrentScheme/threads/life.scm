;; Conway's game of life with concurrent computations
(define N (expt 2 4))
(define MAXTHREADSIZE 10)

(define (random-bool)
  (> (random 100) 50))

(define (build-vector n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))





(define (range a b)
  (letrec ((loop (lambda (i acc)
                   (if (< i a)
                       acc
                       (loop (- i 1) (cons i acc))))))
    (loop (- b 1) '())))

(define (new-cell)
  (list
   ;; New content of the cell
   (ref #f)
   ;; Current cell content
   (ref (random-bool))
   ;; Lock protecting this cell
   (new-lock)))
(define *field*
  (build-vector N (make-vector N (new-cell))
                (lambda (i)
                  (build-vector N (new-cell) (lambda (i) (new-cell))))))

(define (display-field)
  (for-each (lambda (i)
              (for-each (lambda (j)
                          (if (deref (cadr (field-ref i j)))
                              (display "x ")
                              (display "  ")))
                        (range 0 N))
              (newline))
            (range 0 N))
  (newline))

(define *current-step* (ref 0))

(define (field-ref i j)
  (vector-ref (vector-ref *field* i) j))

(define (update-to-new cell)
  (let ((lock (caddr cell))
        (old (cadr cell))
        (new (car cell)))
    (acquire lock)
    (ref-set old (deref new))
    (release lock)))

(define (game-of-life-new-step)
  (for-each (lambda (i)
              (for-each (lambda (j)
                          (update-to-new (field-ref i j)))
                        (range 0 N)))
            (range 0 N)))

(define (cell-alive? i j)
  (if (and (>= i 0) (>= j 0) (< i N) (< j N))
      (let* ((cell (field-ref i j))
             (v (deref (cadr cell))))
        v)
      #f))

(define (count-true l)
  (if (null? l)
      0
      (if (car l)
          (+ 1 (count-true (cdr l)))
          (count-true (cdr l)))))

(define (neighbors i j)
  (count-true (list (cell-alive? (- i 1) j) (cell-alive? (- i 1) (- j 1)) (cell-alive? (- i 1) (+ j 1))
                    (cell-alive? i (- j 1)) (cell-alive? i (+ j 1))
                    (cell-alive? (+ i 1) j) (cell-alive? (+ i 1) (- j 1)) (cell-alive? (+ i 1) (+ j 1)))))

(define (cell-live i j)
  (let* ((cell (field-ref i j))
         (ref (car cell))
         (lock (caddr cell)))
    (acquire lock)
    (ref-set ref #t)
    (release lock)))

(define (cell-die i j)
  (let* ((cell (field-ref i j))
         (ref (car cell))
         (lock (caddr cell)))
    (acquire lock)
    (ref-set ref #f)
    (release lock)))

(define (game-of-life-thread fromx tox fromy toy)
  (for-each (lambda (i)
              (for-each
               (lambda (j)
                 (let ((n (neighbors i j)))
                   (if (cell-alive? i j)
                       (if (or (< n 2) (> n 3))
                           (cell-die i j)
                           (cell-live i j))
                       (if (= n 3)
                           (cell-live i j)
                           (cell-die i j)))))
               (range fromy toy)))
            (range fromx tox)))

(define (split-threads fromx tox fromy toy max)
  (if (and (<= (- tox fromx) max) (<= (- toy fromy) max))
      (list (list fromx tox fromy toy))
      (let ((halfx (+ fromx (quotient (- tox fromx) 2)))
            (halfy (+ fromy (quotient (- toy fromy) 2))))
        (append
         (split-threads fromx halfx fromy halfy max)
         (append
          (split-threads fromx halfx (+ halfy 1) toy max)
          (append (split-threads (+ halfx 1) tox fromy halfy max)
                  (split-threads (+ halfx 1) tox (+ halfy 1) toy max)))))))

(define (game-of-life-threads)
  (let ((thread-bounds (split-threads 0 N 0 N MAXTHREADSIZE)))
    (map (lambda (bound) (fork (game-of-life-thread (car bound) (cadr bound) (caddr bound) (cadddr bound)))) thread-bounds)))

(define (game-of-life-whole-step)
  (let ((threads (game-of-life-threads)))
    (map (lambda (t) (join t)) threads)
    (game-of-life-new-step)))

(define (game-of-life iterations)
  (if (= iterations 0)
      (display-field)
      (begin
        (display-field)
        (game-of-life-whole-step)
        (game-of-life (- iterations 1)))))

(game-of-life 10)
