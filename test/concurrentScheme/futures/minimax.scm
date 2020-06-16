(define (take l n)
  (if (or (= n 0) (null? l))
      '()
      (cons (car l) (take (cdr l) (- n 1)))))

(define (drop l n)
  (if (or (= n 0) (null? l))
      l
      (drop (cdr l) (- n 1))))

(define foldl
  (lambda (f base lst)

    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
            base
            (foldl-aux (f base (car lst)) (cdr lst)))))

    (foldl-aux base lst)))

(define (make-list n f)
  (letrec ((loop (lambda (i acc)
                   (if (< i 0)
                       acc
                       (loop (- i 1) (cons (f i) acc))))))
    (loop (- n 1) '())))

(define initial-board
  (make-list 9 (lambda (i) (if (= i 6) 'X (if (= i 7) 'O (if (= i 8) 'X '_))))))

(define (display-board b)
  (display (take b 3)) (newline)
  (display (take (drop b 3) 3)) (newline)
  (display (take (drop b 6) 3)) (newline))

(define p1 'X)
(define p2 'O)

(define (opponent p)
  (if (eq? p 'X)
      'O
      'X))

(define (every? f l)
  (foldl (lambda (x y) (and x (f y))) #t l))

(define (check-col board p l)
  (every? (lambda (i) (eq? i p)) (take (drop board (* (- l 1) 3)) 3)))

(define (check-row board p l)
  (every? (lambda (i) (eq? i p)) (map (lambda (i) (list-ref board (+ (- l 1) (* 3 i)))) (list 0 1 2))))

(define (check-diag1 board p)
  (every? (lambda (i) (eq? i p)) (list (list-ref board 0) (list-ref board 4) (list-ref board 8))))

(define (check-diag2 board p)
  (every? (lambda (i) (eq? i p)) (list (list-ref board 2) (list-ref board 4) (list-ref board 6))))

(define (win? board p)
  (or (check-row board p 1) (check-row board p 2) (check-row board p 3)
      (check-col board p 1) (check-col board p 2) (check-row board p 3)
      (check-diag1 board p) (check-diag2 board p)))

(define (score board me)
  (if (win? board me)
      10
      (if (win? board (opponent me))
          -10
          0)))

(define (available-moves board p)
  (cdr
   (foldl (lambda (moves pos)
            (if pos
                (cons (+ (car moves) 1) (cons (car moves) (cdr moves)))
                (cons (+ (car moves) 1) (cdr moves))))
          (cons 0 '())
          (map (lambda (cell) (eq? cell '_)) board))))

(define (apply-move b p move)
  (append (take b move)
          (append (list p) (drop b (+ move 1)))))

(define (minimax board me)
  (define (minimax-helper b p)
    (let ((s (score b me)))
      (if (not (= 0 s))
          (* 10 s)
          (let* ((thrds (map (lambda (move)
                                 (future (minimax-helper (apply-move b p move) (opponent p))))
                               (available-moves b p)))
                 (minimax-childs (map (lambda (t) (deref t)) thrds)))
            (foldl + 0 minimax-childs)))))
  (let ((possible-moves (available-moves board me)))
    (if (null? possible-moves)
        'done
        (let ((move-scores (map (lambda (move) (cons move (minimax-helper (apply-move board me move) (opponent me))))
                                possible-moves)))
          (car (foldl (lambda (acc ms) (if (> (cdr ms) (cdr acc)) ms acc))
                      (car move-scores)
                      (cdr move-scores)))))))

(define (game board p turn)
  (display "turn ") (display turn) (newline)
  (let ((move (minimax board p)))
    (if (eq? move 'done)
        (if (win? board 'X)
            (display "player X won\n")
            (if (win? board 'O)
                (display "player O won\n")
                (display "draw\n")))
        (let ((new-board (apply-move board p move)))
          (display-board new-board)
          (game new-board (opponent p) (+ turn 1))))))

(game initial-board 'O 0)

