(define (for-each f l)
  (if (null? l)
      #t
      (if (pair? l)
          (begin (f (car l)) (for-each f (cdr l)))
          (error "Cannot for-each over a non-list"))))
;;; GRAPHS -- Obtained from Andrew Wright.

(define fold
    (lambda (lst folder state)
        (do ((lst lst
                    (cdr lst))
                (state state
                    (folder (car lst)
                        state)))
            ((null? lst)
                state))))

(define proc->vector
  (lambda (size f)
    (if (zero? size)
        (vector)
        (let ((x (make-vector size (f 0))))
          (let loop ((i 1))
            (if (< i size)
              (begin
                (vector-set! x i (f i))
                (loop (+ i 1)))))
          x))))

(define vector-fold
    (lambda (vec folder state)
        (let ((len
                    (vector-length vec)))
            (do ((i 0
                        (+ i 1))
                    (state state
                        (folder (vector-ref vec i)
                            state)))
                ((= i len)
                    state)))))

(define vector-map
    (lambda (vec proc)
        (proc->vector (vector-length vec)
            (lambda (i)
                (proc (vector-ref vec i))))))

(define giota
    (lambda (limit)
        (let _-*-
            ((limit
                    limit)
                (res
                    '()))
            (if (zero? limit)
                res
                (let ((limit
                            (- limit 1)))
                    (_-*- limit
                        (cons limit res)))))))

(define gnatural-fold
    (lambda (limit folder state)
        (do ((i 0
                    (+ i 1))
                (state state
                    (folder i state)))
            ((= i limit)
                state))))

(define gnatural-for-each
    (lambda (limit proc!)
        (do ((i 0
                    (+ i 1)))
            ((= i limit))
            (proc! i))))

(define natural-for-all?
    (lambda (limit ok?)
        (let _-*-
            ((i 0))
            (or (= i limit)
                (and (ok? i)
                    (_-*- (+ i 1)))))))

(define natural-there-exists?
    (lambda (limit ok?)
        (let _-*-
            ((i 0))
            (and (not (= i limit))
                (or (ok? i)
                    (_-*- (+ i 1)))))))

(define there-exists?
    (lambda (lst ok?)
        (let _-*-
            ((lst lst))
            (and (not (null? lst))
                (or (ok? (car lst))
                    (_-*- (cdr lst)))))))

(define fold-over-perm-tree
    (lambda (universe b-folder b-state t-folder t-state)

        (let _-*-
            ((universe
                    universe)
                (b-state
                    b-state)
                (t-state
                    t-state)
                (accross
                    (lambda (final-t-state)
                        final-t-state)))
            (if (null? universe)
                (t-folder b-state t-state accross)
                (let _-**-
                    ((in
                            universe)
                        (out
                            '())
                        (t-state
                            t-state))
                    (let* ((first
                                (car in))
                            (rest
                                (cdr in))
                            (accross
                                (if (null? rest)
                                    accross
                                    (lambda (new-t-state)
                                        (_-**- rest
                                            (cons first out)
                                            new-t-state)))))
                        (b-folder first
                            b-state
                            t-state
                            (lambda (new-b-state new-t-state)
                                (_-*- (fold out cons rest)
                                    new-b-state
                                    new-t-state
                                    accross))
                            accross)))))))

(define make-minimal?
    (lambda (max-size)
        (let ((iotas
                    (proc->vector (+ max-size 1)
                        giota))
                (perm
                    (make-vector max-size 0)))
            (lambda (size graph folder state)
                (fold-over-perm-tree (vector-ref iotas size)
                    (lambda (perm-x x state deeper accross)
                        (case (cmp-next-vertex graph perm x perm-x)
                            ((less)
                                #f)
                            ((equal)
                                (vector-set! perm x perm-x)
                                (deeper (+ x 1)
                                    state))
                            ((more)
                                (accross state))
                            (else
                                (error "???"))))
                    0
                    (lambda (leaf-depth state accross)
                        (folder perm state accross))
                    state)))))

(define cmp-next-vertex
    (lambda (graph perm x perm-x)
        (let ((from-x
                    (vector-ref graph x))
                (from-perm-x
                    (vector-ref graph perm-x)))
            (let _-*-
                ((y
                        0))
                (if (= x y)
                    'equal
                    (let ((x->y?
                                (vector-ref from-x y))
                            (perm-y
                                (vector-ref perm y)))
                        (cond ((eq? x->y?
                                    (vector-ref from-perm-x perm-y))
                                (let ((y->x?
                                            (vector-ref (vector-ref graph y)
                                                x)))
                                    (cond ((eq? y->x?
                                                (vector-ref (vector-ref graph perm-y)
                                                    perm-x))
                                            (_-*- (+ y 1)))
                                        (y->x?
                                            'less)
                                        (else
                                            'more))))
                            (x->y?
                                'less)
                            (else
                                'more))))))))


(define fold-over-rdg
    (lambda (size max-out folder state)
        (let* ((root
                    (- size 1))
                (edge?
                    (proc->vector size
                        (lambda (from)
                            (make-vector size #f))))
                (edges
                    (make-vector size '()))
                (out-degrees
                    (make-vector size 0))
                (minimal-folder
                    (make-minimal? root))
                (non-root-minimal?
                    (let ((cont
                                (lambda (perm state accross)
                                    (accross #t))))
                        (lambda (size)
                            (minimal-folder size
                                edge?
                                cont
                                #t))))
                (root-minimal?
                    (let ((cont
                                (lambda (perm state accross)
                                    (case (cmp-next-vertex edge? perm root root)
                                        ((less)
                                            #f)
                                        ((equal more)
                                            (accross #t))
                                        (else
                                            (error "???"))))))
                        (lambda ()
                            (minimal-folder root
                                edge?
                                cont
                                #t)))))
            (let _-*-
                ((vertex
                        0)
                    (state
                        state))
                (cond ((not (non-root-minimal? vertex))
                        state)
                    ((= vertex root)
                        (let ((reach?
                                    (make-reach? root edges))
                                (from-root
                                    (vector-ref edge? root)))
                            (let _-*-
                                ((v
                                        0)
                                    (outs
                                        0)
                                    (efr
                                        '())
                                    (efrr
                                        '())
                                    (state
                                        state))
                                (cond ((not (or (= v root)
                                                (= outs max-out)))
                                        (vector-set! from-root v #t)
                                        (let ((state
                                                    (_-*- (+ v 1)
                                                        (+ outs 1)
                                                        (cons v efr)
                                                        (cons (vector-ref reach? v)
                                                            efrr)
                                                        state)))
                                            (vector-set! from-root v #f)
                                            (_-*- (+ v 1)
                                                outs
                                                efr
                                                efrr
                                                state)))
                                    ((and (natural-for-all? root
                                                (lambda (v)
                                                    (there-exists? efrr
                                                        (lambda (r)
                                                            (vector-ref r v)))))
                                            (root-minimal?))
                                        (vector-set! edges root efr)
                                        (folder
                                            (proc->vector size
                                                (lambda (i)
                                                    (vector-ref edges i)))
                                            state))
                                    (else
                                        state)))))
                    (else
                        (let ((from-vertex
                                    (vector-ref edge? vertex)))
                            (let _-**-
                                ((sv
                                        0)
                                    (outs
                                        0)
                                    (state
                                        state))
                                (if (= sv vertex)
                                    (begin
                                        (vector-set! out-degrees vertex outs)
                                        (_-*- (+ vertex 1)
                                            state))
                                    (let* ((state
                                                (_-**- (+ sv 1)
                                                    outs
                                                    state))
                                            (from-sv
                                                (vector-ref edge? sv))
                                            (sv-out
                                                (vector-ref out-degrees sv))
                                            (state
                                                (if (= sv-out max-out)
                                                    state
                                                    (begin
                                                        (vector-set! edges
                                                            sv
                                                            (cons vertex
                                                                (vector-ref edges sv)))
                                                        (vector-set! from-sv vertex #t)
                                                        (vector-set! out-degrees sv (+ sv-out 1))
                                                        (let* ((state
                                                                    (_-**- (+ sv 1)
                                                                        outs
                                                                        state))
                                                                (state
                                                                    (if (= outs max-out)
                                                                        state
                                                                        (begin
                                                                            (vector-set! from-vertex sv #t)
                                                                            (vector-set! edges
                                                                                vertex
                                                                                (cons sv
                                                                                    (vector-ref edges vertex)))
                                                                            (let ((state
                                                                                        (_-**- (+ sv 1)
                                                                                            (+ outs 1)
                                                                                            state)))
                                                                                (vector-set! edges
                                                                                    vertex
                                                                                    (cdr (vector-ref edges vertex)))
                                                                                (vector-set! from-vertex sv #f)
                                                                                state)))))
                                                            (vector-set! out-degrees sv sv-out)
                                                            (vector-set! from-sv vertex #f)
                                                            (vector-set! edges
                                                                sv
                                                                (cdr (vector-ref edges sv)))
                                                            state)))))
                                        (if (= outs max-out)
                                            state
                                            (begin
                                                (vector-set! edges
                                                    vertex
                                                    (cons sv
                                                        (vector-ref edges vertex)))
                                                (vector-set! from-vertex sv #t)
                                                (let ((state
                                                            (_-**- (+ sv 1)
                                                                (+ outs 1)
                                                                state)))
                                                    (vector-set! from-vertex sv #f)
                                                    (vector-set! edges
                                                        vertex
                                                        (cdr (vector-ref edges vertex)))
                                                    state)))))))))))))

(define make-reach?
    (lambda (size vertex->out)
        (let ((res
                    (proc->vector size
                        (lambda (v)
                            (let ((from-v
                                        (make-vector size #f)))
                                (vector-set! from-v v #t)
                                (for-each
                                    (lambda (x)
                                        (vector-set! from-v x #t))
                                    (vector-ref vertex->out v))
                                from-v)))))
            (gnatural-for-each size
                (lambda (m)
                    (let ((from-m
                                (vector-ref res m)))
                        (gnatural-for-each size
                            (lambda (f)
                                (let ((from-f
                                            (vector-ref res f)))
                                    (if (vector-ref from-f m)
                                        (gnatural-for-each size
                                            (lambda (t)
                                                (if (vector-ref from-m t)
                                                    (vector-set! from-f t #t)))))))))))
            res)))


(define (run n)
  (fold-over-rdg n
    2
    cons
    '()))

(= (length (run 5)) 596)
