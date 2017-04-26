(define (create-bst . options)
  (let ((root '())
        (same? (if (null? options) = (car options)))
        (less? (if (null? options) < (cadr options))))
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
    
    (define (tree-traverse-nodes tree order action)
      (define (traverse-preorder tree)
        (cond
          ((null-tree? tree)
           #t)
          (else
           (action tree)
           (traverse-preorder (left tree))
           (traverse-preorder (right tree)))))
      (define (traverse-inorder tree)
        (cond
          ((null-tree? tree)
           #t)
          (else
           (traverse-inorder (left tree))
           (action tree)
           (traverse-inorder (right tree)))))
      (define (traverse-postorder tree)
        (cond
          ((null-tree? tree)
           #t)
          (else
           (traverse-postorder (left tree))
           (traverse-postorder (right tree))
           (action tree))))
      (case order
        ((preorder)
         (traverse-preorder tree))
        ((inorder)
         (traverse-inorder tree))
        ((postorder)
         (traverse-postorder tree))
        (else
         (error "Unknown tree traversal order"))))
    
    (define (tree-acc tree operator init function)
      (define (acc-aux tree)
        (if (null-tree? tree)
            init
            (operator (function tree)
                      (acc-aux (left tree))
                      (acc-aux (right tree)))))
      (acc-aux tree))
    
    (define (empty?)
      (null-tree? root))
    
    (define (tree-search k)
      (define (search-aux node)
        (cond
          ((null-tree? node)
           #f)
          ((same? k (key node))
           node)
          ((less? k (key node))
           (search-aux (left node)))
          (else
           (search-aux (right node)))))
      (search-aux root))
    
    (define (tree-minimum x)
      (cond ((null-tree? (left x))
             x)
            (else 
             (tree-minimum (left x)))))
    
    (define (tree-maximum x)
      (cond ((null-tree? (right x))
             x)
            (else 
             (tree-maximum (right x)))))
    
    (define (tree-successor x)
      (define (iter x y)
        (cond ((and (not (null-tree? y))
                    (same? (key x) (key (right y))))
               (iter y (parent y)))
              (else y)))
      (if (not (null-tree? (right x)))
          (tree-minimum (right x))
          (let ((y (parent x)))
            (iter x y))))
    
    (define (tree-insert k i)
      (let ((y null-tree)
            (z (make-node k null-tree null-tree null-tree i)))
        (define (insert-aux x)
          (cond
            ((not (null-tree? x))
             (set! y x)
             (if (less? (key z) (key y))
                 (insert-aux (left x))
                 (insert-aux (right x))))))
        (insert-aux root)
        (if (null-tree? y)
            (set! root z)
            (begin
              (if (less? (key z) (key y))
                  (set-left! y z)
                  (set-right! y z))
              (set-parent! z y)))))
    
    (define (tree-delete k)
      (let ((z (tree-search k)))
        (if z
            (let* ((y (if (or (null-tree? (left z))
                              (null-tree? (right z)))
                          z
                          (tree-successor z)))
                   (x (if (not (null-tree? (left y)))
                          (left y)
                          (right y))))
              (if (not (null-tree? x))
                  (set-parent! x (parent y)))
              (if (null-tree? (parent y))
                  (set! root x)
                  (if (and (not (null-tree? (left (parent y))))
                           (same? (key y) (key (left (parent y)))))
                      (set-left! (parent y) x)
                      (set-right! (parent y) x)))
              (if (not (same? (key y) (key z)))
                  (begin
                    (set-key! z (key y))
                    (set-info! z (info z))))
              y))))
    
    (define (map a-function)
      (tree-acc root cons null-tree a-function))
    (define (foreach a-action)
      (tree-traverse-nodes root 'inorder a-action))
    (define (dispatch msg . args)
      (cond
        ((eq? msg 'empty) (empty?))
        ((eq? msg 'insert) (tree-insert (car args) (cadr args)))
        ((eq? msg 'delete) (tree-delete (car args)))
        ((eq? msg 'lookup) (tree-search (car args)))
        ((eq? msg 'map) (map (car args)))
        ((eq? msg 'foreach) (foreach (car args)))
        ((eq? msg 'display) (foreach (lambda (x) (display (key x)) (newline))))
        (else
         (error "unknown request -- create-BST" msg))))
    dispatch))
