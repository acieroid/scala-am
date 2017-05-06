;;; MATRIX -- Obtained from Andrew Wright.

; Chez-Scheme compatibility stuff:

(define (chez-box x) (cons x '()))
(define (chez-unbox x) (car x))
(define (chez-set-box! x y) (set-car! x y))

(define maximal?
  (lambda (mat)
    (let pick-first-row
      ((first-row-perm
        (gen-perms mat)))
      (if first-row-perm
          (and (zunda first-row-perm mat)
               (pick-first-row (first-row-perm 'brother)))
          #t))))

(define zunda
  (lambda (first-row-perm mat)
    (let* ((first-row
            (first-row-perm 'now))
           (number-of-cols
            (length first-row))
           (make-row->func
            (lambda (if-equal if-different)
              (lambda (row)
                (let ((vec
                       (make-vector number-of-cols)))
                  (do ((i 0 (+ i 1))
                       (first first-row
                              (cdr first))
                       (row row
                            (cdr row)))
                    ((= i number-of-cols))
                    (vector-set! vec
                                 i
                                 (if (= (car first) (car row))
                                     if-equal
                                     if-different)))
                  (lambda (i)
                    (vector-ref vec i))))))
           (mat
            (cdr mat)))
      (zebra (first-row-perm 'child)
             (make-row->func 1 -1)
             (make-row->func -1 1)
             mat
             number-of-cols))))


(define zebra
  (lambda (row-perm row->func+ row->func- mat number-of-cols)
    (let _-*-
      ((row-perm
        row-perm)
       (mat
        mat)
       (partitions
        (list (miota number-of-cols))))
      (or (not row-perm)
          (and
           (zulu (car mat)
                 (row->func+ (row-perm 'now))
                 partitions
                 (lambda (new-partitions)
                   (_-*- (row-perm 'child)
                         (cdr mat)
                         new-partitions)))
           (zulu (car mat)
                 (row->func- (row-perm 'now))
                 partitions
                 (lambda (new-partitions)
                   (_-*- (row-perm 'child)
                         (cdr mat)
                         new-partitions)))
           (let ((new-row-perm
                  (row-perm 'brother)))
             (or (not new-row-perm)
                 (_-*- new-row-perm
                       mat
                       partitions))))))))


(define zulu
  (let ((cons-if-not-null
         (lambda (lhs rhs)
           (if (null? lhs)
               rhs
               (cons lhs rhs)))))
    (lambda (old-row new-row-func partitions equal-cont)
      (let _-*-
        ((p-in
          partitions)
         (old-row
          old-row)
         (rev-p-out
          '()))
        (let _-split-
          ((partition
            (car p-in))
           (old-row
            old-row)
           (plus
            '())
           (minus
            '()))
          (if (null? partition)
              (let _-minus-
                ((old-row
                  old-row)
                 (m
                  minus))
                (if (null? m)
                    (let ((rev-p-out
                           (cons-if-not-null
                            minus
                            (cons-if-not-null
                             plus
                             rev-p-out)))
                          (p-in
                           (cdr p-in)))
                      (if (null? p-in)
                          (equal-cont (reverse rev-p-out))
                          (_-*- p-in old-row rev-p-out)))
                    (or (= 1 (car old-row))
                        (_-minus- (cdr old-row)
                                  (cdr m)))))
              (let ((next
                     (car partition)))
                (case (new-row-func next)
                  ((1)
                   (and (= 1 (car old-row))
                        (_-split- (cdr partition)
                                  (cdr old-row)
                                  (cons next plus)
                                  minus)))
                  ((-1)
                   (_-split- (cdr partition)
                             old-row
                             plus
                             (cons next minus)))))))))))

(define all?
  (lambda (ok? lst)
    (let _-*-
      ((lst
        lst))
      (or (null? lst)
          (and (ok? (car lst))
               (_-*- (cdr lst)))))))

(define gen-perms
  (lambda (objects)
    (let _-*-
      ((zulu-future
        objects)
       (past
        '()))
      (if (null? zulu-future)
          #f
          (lambda (msg)
            (case msg
              ((now)
               (car zulu-future))
              ((brother)
               (_-*- (cdr zulu-future)
                     (cons (car zulu-future)
                           past)))
              ((child)
               (gen-perms
                (fold past cons (cdr zulu-future))))
              ((puke)
               (cons (car zulu-future)
                     (fold past cons (cdr zulu-future))))
              (else
               (error gen-perms "Bad msg: ~a" msg))))))))

(define fold
  (lambda (lst folder state)
    (let _-*-
      ((lst
        lst)
       (state
        state))
      (if (null? lst)
          state
          (_-*- (cdr lst)
                (folder (car lst)
                        state))))))

(define miota
  (lambda (len)
    (let _-*-
      ((i 0))
      (if (= i len)
          '()
          (cons i
                (_-*- (+ i 1)))))))

(define proc->vector
  (lambda (size proc)
    (let ((res
           (make-vector size)))
      (do ((i 0
              (+ i 1)))
        ((= i size))
        (vector-set! res
                     i
                     (proc i)))
      res)))

(define make-modular
  (lambda (modulus)
    (let* ((reduce
            (lambda (x)
              (modulo x modulus)))
           (coef-zero?
            (lambda (x)
              (zero? (reduce x))))
           (coef-+
            (lambda (x y)
              (reduce (+ x y))))
           (coef-negate
            (lambda (x)
              (reduce (- x))))
           (coef-*
            (lambda (x y)
              (reduce (* x y))))
           (coef-recip
            (let ((inverses
                   (proc->vector (- modulus 1)
                                 (lambda (i)
                                   (extended-gcd (+ i 1)
                                                 modulus
                                                 (lambda (gcd inverse ignore)
                                                   inverse))))))
              (lambda (x)
                (let ((x
                       (reduce x)))
                  (vector-ref inverses (- x 1)))))))
      (lambda (maker)
        (maker 0
               1
               coef-zero?
               coef-+
               coef-negate
               coef-*
               coef-recip)))))

(define extended-gcd
  (let ((n->sgn/abs
         (lambda (x cont)
           (if (>= x 0)
               (cont 1 x)
               (cons -1 (- x))))))
    (lambda (a b cont)
      (n->sgn/abs a
                  (lambda (p-a p)
                    (n->sgn/abs b
                                (lambda (q-b q)
                                  (let _-*-
                                    ((p
                                      p)
                                     (p-a
                                      p-a)
                                     (p-b
                                      0)
                                     (q
                                      q)
                                     (q-a
                                      0)
                                     (q-b
                                      q-b))
                                    (if (zero? q)
                                        (cont p p-a p-b)
                                        (let ((mult
                                               (quotient p q)))
                                          (_-*- q
                                                q-a
                                                q-b
                                                (- p (* mult q))
                                                (- p-a (* mult q-a))
                                                (- p-b (* mult q-b)))))))))))))

(define make-row-reduce
  (lambda (coef-zero coef-one coef-zero? coef-+ coef-negate coef-* coef-recip)
    (lambda (mat)
      (let _-*-
        ((mat
          mat))
        (if (or (null? mat)
                (null? (car mat)))
            '()
            (let _-**-
              ((in
                mat)
               (out
                '()))
              (if (null? in)
                  (map
                   (lambda (x)
                     (cons coef-zero x))
                   (_-*- out))
                  (let* ((prow
                          (car in))
                         (pivot
                          (car prow))
                         (prest
                          (cdr prow))
                         (in
                          (cdr in)))
                    (if (coef-zero? pivot)
                        (_-**- in
                               (cons prest out))
                        (let ((zap-row
                               (map
                                (let ((mult
                                       (coef-recip pivot)))
                                  (lambda (x)
                                    (coef-* mult x)))
                                prest)))
                          (cons (cons coef-one zap-row)
                                (map
                                 (lambda (x)
                                   (cons coef-zero x))
                                 (_-*-
                                  (fold in
                                        (lambda (row mat)
                                          (cons
                                           (let ((first-col
                                                  (car row))
                                                 (rest-row
                                                  (cdr row)))
                                             (if (coef-zero? first-col)
                                                 rest-row
                                                 (map
                                                  (let ((mult
                                                         (coef-negate first-col)))
                                                    (lambda (f z)
                                                      (coef-+ f
                                                              (coef-* mult z))))
                                                  rest-row
                                                  zap-row)))
                                           mat))
                                        out))))))))))))))

(define make-in-row-space?
  (lambda (coef-zero coef-one coef-zero? coef-+ coef-negate coef-* coef-recip)
    (let ((row-reduce
           (make-row-reduce coef-zero
                            coef-one
                            coef-zero?
                            coef-+
                            coef-negate
                            coef-*
                            coef-recip)))
      (lambda (mat)
        (let ((mat
               (row-reduce mat)))
          (lambda (row)
            (let _-*-
              ((row
                row)
               (mat
                mat))
              (if (null? row)
                  #t
                  (let ((r-first
                         (car row))
                        (r-rest
                         (cdr row)))
                    (cond ((coef-zero? r-first)
                           (_-*- r-rest
                                 (map cdr
                                      (if (or (null? mat)
                                              (coef-zero? (caar mat)))
                                          mat
                                          (cdr mat)))))
                          ((null? mat)
                           #f)
                          (else
                           (let* ((zap-row
                                   (car mat))
                                  (z-first
                                   (car zap-row))
                                  (z-rest
                                   (cdr zap-row))
                                  (mat
                                   (cdr mat)))
                             (if (coef-zero? z-first)
                                 #f
                                 (_-*-
                                  (map
                                   (let ((mult
                                          (coef-negate r-first)))
                                     (lambda (r z)
                                       (coef-+ r
                                               (coef-* mult z))))
                                   r-rest
                                   z-rest)
                                  (map cdr mat)))))))))))))))

(define make-modular-row-reduce
  (lambda (modulus)
    ((make-modular modulus)
     make-row-reduce)))


(define make-modular-in-row-space?
  (lambda (modulus)
    ((make-modular modulus)
     make-in-row-space?)))

(define find-prime
  (lambda (bound)
    (let* ((primes
            (list 2))
           (last
            (chez-box primes))
           (is-next-prime?
            (lambda (trial)
              (let _-*-
                ((primes
                  primes))
                (or (null? primes)
                    (let ((p
                           (car primes)))
                      (or (< trial (* p p))
                          (and (not (zero? (modulo trial p)))
                               (_-*- (cdr primes))))))))))
      (if (> 2 bound)
          2
          (let _-*-
            ((trial
              3))
            (if (is-next-prime? trial)
                (let ((entry
                       (list trial)))
                  (set-cdr! (chez-unbox last) entry)
                  (chez-set-box! last entry)
                  (if (> trial bound)
                      trial
                      (_-*- (+ trial 2))))
                (_-*- (+ trial 2))))))))

(define det-upper-bound
  (lambda (size)
    (let ((main-part
           (expt size
                 (quotient size 2))))
      (if (even? size)
          main-part
          (* main-part
             (do ((i 0 (+ i 1)))
               ((>= (* i i) size)
                i)))))))

(define go
  (lambda (number-of-cols inv-size folder state)
    (let* ((in-row-space?
            (make-modular-in-row-space?
             (find-prime
              (det-upper-bound inv-size))))
           (make-tester
            (lambda (mat)
              (let ((tests
                     (let ((old-mat
                            (cdr mat))
                           (new-row
                            (car mat)))
                       (fold-over-subs-of-size old-mat
                                               (- inv-size 2)
                                               (lambda (sub tests)
                                                 (cons
                                                  (in-row-space?
                                                   (cons new-row sub))
                                                  tests))
                                               '()))))
                (lambda (row)
                  (let _-*-
                    ((tests
                      tests))
                    (and (not (null? tests))
                         (or ((car tests) row)
                             (_-*- (cdr tests)))))))))
           (all-rows
            (fold
             (fold-over-rows (- number-of-cols 1)
                             cons
                             '())
             (lambda (row rows)
               (cons (cons 1 row)
                     rows))
             '())))
      (let _-*-
        ((number-of-rows
          1)
         (rev-mat
          (list
           (car all-rows)))
         (possible-future
          (cdr all-rows))
         (state
          state))
        (let ((zulu-future
               (remove-in-order
                (if (< number-of-rows inv-size)
                    (in-row-space? rev-mat)
                    (make-tester rev-mat))
                possible-future)))
          (if (null? zulu-future)
              (folder (reverse rev-mat)
                      state)
              (let _-**-
                ((zulu-future
                  zulu-future)
                 (state
                  state))
                (if (null? zulu-future)
                    state
                    (let ((rest-of-future
                           (cdr zulu-future)))
                      (_-**- rest-of-future
                             (let* ((first
                                     (car zulu-future))
                                    (new-rev-mat
                                     (cons first rev-mat)))
                               (if (maximal? (reverse new-rev-mat))
                                   (_-*- (+ number-of-rows 1)
                                         new-rev-mat
                                         rest-of-future
                                         state)
                                   state))))))))))))

(define go-folder
  (lambda (mat bsize.blen.blist)
    (let ((bsize
           (car bsize.blen.blist))
          (size
           (length mat)))
      (if (< size bsize)
          bsize.blen.blist
          (let ((blen
                 (cadr bsize.blen.blist))
                (blist
                 (cddr bsize.blen.blist)))
            (if (= size bsize)
                (let ((blen
                       (+ blen 1)))
                  (cons bsize
                        (cons blen
                              (cond ((< blen 3000)
                                     (cons mat blist))
                                    ((= blen 3000)
                                     (cons "..." blist))
                                    (else
                                     blist)))))
                (list size 1 mat)))))))

(define really-go
  (lambda (number-of-cols inv-size)
    (cddr
     (go number-of-cols
         inv-size
         go-folder
         (list -1 -1)))))

(define remove-in-order
  (lambda (remove? lst)
    (reverse
     (fold lst
           (lambda (e lst)
             (if (remove? e)
                 lst
                 (cons e lst)))
           '()))))

(define fold-over-rows
  (lambda (number-of-cols folder state)
    (if (zero? number-of-cols)
        (folder '()
                state)
        (fold-over-rows (- number-of-cols 1)
                        (lambda (tail state)
                          (folder (cons -1 tail)
                                  state))
                        (fold-over-rows (- number-of-cols 1)
                                        (lambda (tail state)
                                          (folder (cons 1 tail)
                                                  state))
                                        state)))))

(define fold-over-subs-of-size
  (lambda (universe size folder state)
    (let ((usize
           (length universe)))
      (if (< usize size)
          state
          (let _-*-
            ((size
              size)
             (universe
              universe)
             (folder
              folder)
             (csize
              (- usize size))
             (state
              state))
            (cond ((zero? csize)
                   (folder universe state))
                  ((zero? size)
                   (folder '() state))
                  (else
                   (let ((first-u
                          (car universe))
                         (rest-u
                          (cdr universe)))
                     (_-*- size
                           rest-u
                           folder
                           (- csize 1)
                           (_-*- (- size 1)
                                 rest-u
                                 (lambda (tail state)
                                   (folder (cons first-u tail)
                                           state))
                                 csize
                                 state))))))))))

(equal? (really-go 5 5)
        '(((1 1 1 1 1) (1 1 1 1 -1) (1 1 1 -1 1)
                       (1 1 -1 -1 -1) (1 -1 1 -1 -1) (1 -1 -1 1 1))
          ((1 1 1 1 1) (1 1 1 1 -1) (1 1 1 -1 1)
                       (1 1 -1 1 -1) (1 -1 1 -1 -1) (1 -1 -1 1 1))
          ((1 1 1 1 1) (1 1 1 1 -1) (1 1 1 -1 1)
                       (1 1 -1 1 -1) (1 -1 1 -1 1) (1 -1 -1 1 1))
          ((1 1 1 1 1) (1 1 1 1 -1) (1 1 1 -1 1)
                       (1 1 -1 1 1) (1 -1 1 1 -1) (1 -1 -1 -1 1))
          ((1 1 1 1 1) (1 1 1 1 -1) (1 1 1 -1 1)
                       (1 1 -1 1 1) (1 -1 1 1 1) (1 -1 -1 -1 -1))))
