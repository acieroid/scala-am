(define (make-node key left right parent info)
  (list key left right parent info))

(define (key node)
  (list-ref node 0))

(define (left node)
  (list-ref node 1))

(define (right node)
  (list-ref node 2))

(define (parent node)
  (list-ref node 3))

(define (info node)
  (list-ref node 4))

(define (set-key! node key)
  (set-car! node key))

(define (set-left! node l)
  (set-car! (cdr node) l))

(define (set-right! node r)
  (set-car! (cddr node) r))

(define (set-parent! node p)
  (set-car! (cdddr node) p))

(define (set-info! node i)
  (set-car! (cddddr node) i))

(define null-tree '())

(define (null-tree? tree)
  (null? tree))

(define (is-leaf? node)
  (and (null-tree? (left node))
       (null-tree? (right node))))

(define (is-root? node)
  (null-tree? (parent node)))

