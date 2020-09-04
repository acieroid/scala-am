;; Copyright 2001 Tom Van Cutsem
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; Notice: this file contains a modified version of the original program written by Tom Van Cutsem. This file also is a merger of all files involved in the original program.
;; Some, but not all, modifications have been indicated in comments. Significant changes have been made to the file icons.scm, as our implementation does not support icons.
;; Dummy variables have been introduced to avoid top-level defines being interleaved with program expression.

;; Extra code to avoid user input.
(define user-inputs '(Tom help necromancer yes
                       Coen monk no yes
                       difficulty hard
                       (GAME SETUP OPTIONS) icons off
                       (Coen LOOKS AROUND)
                       (Tom CASTS summon_deamon)
                       (Coen ATTACKS Tom) melee
                       (Coen FLEES)
                       (Tom ASKS spellbook)
                       (Tom CASTS curse) yeran
                       (Coen LOOKS AROUND)
                       (Coen MOVES south)
                       (Coen MOVES east)
                       (Coen GETS lightning-bolt)
                       (Tom ASKS STATUS)
                       (Coen STEALS)
                       (Tom ASKS EXITS)
                       (Coen DROPS ALL)
                       (GAME QUIT Tom)
                       (GAME QUIT Coen)))
(define (read)
  (if (null? user-inputs)
    (error "No more user inputs.")
    (let ((first (car user-inputs)))
      (set! user-inputs (cdr user-inputs))
      first)))

;;
;; MOUNTAINVALE
;;

;;Binary Search Tree ADT

(define (create-bst . options)
  ;;Takes a conscell '(key . data) as input
  (let ((content '())
         (same? (if (null? options) = (car options)))
         (less? (if (null? options) < (cadr options))))
    ;node abstraction
    (define (key-item item) (car item))
    (define (content-item item) (cdr item))
    (define (make-node item left right)
      (list item left right))
    (define (node-item node)
      (car node))
    (define (node-left node)
      (cadr node))
    (define (node-right node)
      (caddr node))
    (define (node-set-item! node item)
      (set-car! node item))
    (define (node-set-left! node son)
      (set-car! (cdr node) son))
    (define (node-set-right! node son)
      (set-car! (cddr node) son))
    (define null-tree '())
    (define (null-tree? tree)
      (null? tree))
    (define (is-leaf? node)
      (and (null-tree? (node-left node))
        (null-tree? (node-right node))))
    ;non basic operations
    (define (traverse tree order action)
      (define (traverse-preorder tree)
        (cond
          ((null-tree? tree)
            #t)
          (else
            (action (content-item (node-item tree)))
            (traverse-preorder (node-left tree))
            (traverse-preorder (node-right tree)))))
      (define (traverse-inorder tree)
        (cond
          ((null-tree? tree)
            #t)
          (else
            (traverse-inorder (node-left tree))
            (action (content-item (node-item tree)))
            (traverse-inorder (node-right tree)))))
      (define (traverse-postorder tree)
        (cond
          ((null-tree? tree)
            #t)
          (else
            (traverse-postorder (node-left tree))
            (traverse-postorder (node-right tree))
            (action (content-item (node-item tree))))))
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
        (if (null-tree tree)
          init
          (operator (function (content-item (node-item tree)))
            (acc-aux (node-left tree))
            (acc-aux (node-right tree)))))
      (acc-aux tree))
    ;operations
    (define (empty?)
      (null-tree? content))
    (define (lookup element)
      (define (lookup-aux node)
        (cond
          ((null-tree? node)
            #f)
          ((same? element (key-item (node-item node)))
            (content-item (node-item node)))
          ((less? element (key-item (node-item node)))
            (lookup-aux (node-left node)))
          (else
            (lookup-aux (node-right node)))))
      (lookup-aux content))
    (define (insert element)
      (define (insert-aux node parent)
        (cond
          ((null-tree? node)
            (let ((new-node (make-node element null-tree null-tree)))
              (if parent
                (if (less? (key-item element) (key-item (node-item parent)))
                  (node-set-left! parent new-node)
                  (node-set-right! parent new-node))
                (set! content new-node)))
            #t)
          ((less? (key-item element) (key-item (node-item node)))
            (insert-aux (node-left node) node))
          (else
            (insert-aux (node-right node) node))))
      (insert-aux content #f))
    (define (delete element)
      (define (replace parent node new-node)
        (if (eq? (node-left parent) node)
          (node-set-left! parent new-node)
          (node-set-right! parent new-node)))
      (define (parent-of-leftmost-node node parent)
        (if (null-tree? (node-left node))
          parent
          (parent-of-leftmost-node (node-left node) node)))
      (define (delete-aux node parent)
        (cond
          ((null-tree? node)
            #f)
          ((same? element (key-item (node-item node)))
            (cond
              ((null-tree? (node-left node))
                ; node has no left child
                (if parent
                  (replace parent node (node-right node))
                  (set! content (node-right node))))
              ((null-tree? (node-right node))
                ; node has no right child
                (if parent
                  (replace parent node (node-left node))
                  (set! content (node-left node))))
              (else
                ; node has two children
                (let ((temp (parent-of-leftmost-node (node-right node) #f)))
                  (if (not temp)
                    (begin
                      (node-set-item! node (node-item (node-right node)))
                      (node-set-right! node (node-right (node-right node))))
                    (begin
                      (node-set-item! node (node-item (node-left temp)))
                      (node-set-left! temp (node-right (node-left temp))))))))
            #t)
          ((less? element (key-item (node-item node)))
            (delete-aux (node-left node) node))
          (else
            (delete-aux (node-right node) node))))
      (delete-aux content #f))
    (define (map a-function)
      (tree-acc content make-node null-tree a-function))
    (define (for-each-element a-action)
      (traverse content 'inorder a-action))
    (define (dispatch msg . args)
      (cond
        ((eq? msg 'empty?) (empty?))
        ((eq? msg 'insert) (insert (car args)))
        ((eq? msg 'delete) (delete (car args)))
        ((eq? msg 'lookup) (lookup (car args)))
        ((eq? msg 'map) (map (car args)))
        ((eq? msg 'for-each) (for-each-element (car args)))
        ((eq? msg 'display) (for-each-element (lambda (x) (display x) (newline))))
        (else
          (error "unknown request -- create-BST" msg))))
    dispatch))

;;Red Black Tree ADT, changed the input to take an element of the form (key . sattelite data)

(define (create-redblack-tree . options)
  (define (make-null-tree)
    (list (cons (cons 'leaf 'leaf) 'black) '() '() '()))
  (let* ((content (make-null-tree))
          (same? (if (null? options) equal?
                   (car options)))
          (less? (if (null? options) < (cadr options))))
    ;node abstraction

    (define (get-node-key item)
      (car item))
    (define (get-node-info item)
      (cdr item))

    (define (make-node item left right parent)
      (list (cons item 'red) left right parent))
    (define (node-item node)
      (car (car node)))
    (define (node-color node)
      (cdr (car node)))
    (define (node-left node)
      (cadr node))
    (define (node-right node)
      (caddr node))
    (define (node-parent node)
      (cadddr node))
    (define (node-set-item! node item)
      (set-car! (car node) item))
    (define (node-set-color! node color)
      (set-cdr! (car node) color))
    (define (node-set-color-red! node)
      (set-cdr! (car node) 'red))
    (define (node-set-color-black! node)
      (set-cdr! (car node) 'black))
    (define (node-set-left! node son)
      (set-car! (cdr node) son))
    (define (node-set-right! node son)
      (set-car! (cddr node) son))
    (define (node-set-parent! node parent)
      (set-car! (cdddr node) parent))
    (define (null-tree? tree)
      (eq? 'leaf (get-node-key (node-item tree))))
    (define (is-leaf? node)
      (and (null-tree? (node-left node))
        (null-tree? (node-right node))))
    (define (traverse tree order action)
      (define (traverse-preorder tree)
        (cond
          ((null-tree? tree)
            #t)
          (else
            (action (node-item tree) (node-color tree))
            (traverse-preorder (node-left tree))
            (traverse-preorder (node-right tree)))))
      (define (traverse-inorder tree)
        (cond
          ((null-tree? tree)
            #t)
          (else
            (traverse-inorder (node-left tree))
            (action (node-item tree) (node-color tree))
            (traverse-inorder (node-right tree)))))
      (define (traverse-postorder tree)
        (cond
          ((null-tree? tree)
            #t)
          (else
            (traverse-postorder (node-left tree))
            (traverse-postorder (node-right tree))
            (action (node-item tree) (node-color tree)))))
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
          (operator (function (get-node-info (node-item tree)))
            (acc-aux (node-left tree))
            (acc-aux (node-right tree))
              '())))
      (acc-aux tree))

    ;rotation operations
    (define (left-rotate node-x)
      (let ((node-y (node-right node-x)))
        (if (null-tree? node-y)
            'done
          (begin (node-set-right! node-x (node-left node-y))
            (node-set-parent! (node-left node-y) node-x)
            (node-set-parent! node-y (node-parent node-x))
            (if (not (null-tree? (node-parent node-x)))
              (if (same? (get-node-key (node-item node-x))
                    (get-node-key (node-item (node-left (node-parent node-x)))))
                (node-set-left! (node-parent node-x) node-y)
                (node-set-right! (node-parent node-x) node-y))
              (set! content node-y))
            (node-set-left! node-y node-x)
            (node-set-parent! node-x node-y)))))

    (define (right-rotate node-y)
      (let ((node-x (node-left node-y)))
        (if (null-tree? node-x)
            'done
          (begin (node-set-left! node-y (node-right node-x))
            (node-set-parent! (node-right node-x) node-y)
            (node-set-parent! node-x (node-parent node-y))
            (if (not (null-tree? (node-parent node-y)))
              (if (same? (get-node-key (node-item node-y))
                    (get-node-key (node-item (node-left (node-parent node-y)))))
                (node-set-left! (node-parent node-y) node-x)
                (node-set-right! (node-parent node-y) node-x))
              (set! content node-x))
            (node-set-right! node-x node-y)
            (node-set-parent! node-y node-x)))))

    (define (parent-is-in node-x node-branch then-rotate other-rotate)
      (let ((node-y (node-branch (node-parent (node-parent node-x)))))
        (cond ((equal? 'red (node-color node-y))
                (node-set-color-black! (node-parent node-x))
                (node-set-color-black! node-y)
                (node-set-color-red! (node-parent (node-parent node-x)))
                (evaluate-redblack-conditions (node-parent (node-parent node-x))))
          (else
            (if (and (not (null-tree? (node-branch (node-parent node-x))))
                  (same? (get-node-key (node-item node-x))
                    (get-node-key (node-item (node-branch (node-parent node-x))))))
              (begin
                (set! node-x (node-parent node-x))
                (then-rotate node-x)))
            (node-set-color-black! (node-parent node-x))
            (node-set-color-red! (node-parent (node-parent node-x)))
            (other-rotate (node-parent (node-parent node-x)))
            (evaluate-redblack-conditions node-x)))))

    (define (evaluate-redblack-conditions node-x)
      (if (and (not (same? (get-node-key (node-item node-x))
                      (get-node-key (node-item content))))
            (equal? 'red (node-color (node-parent node-x)))
            (not (null-tree? (node-parent (node-parent node-x)))))
        (cond ((and
                 (not (null-tree? (node-left (node-parent (node-parent node-x)))))
                 (same? (get-node-key (node-item (node-parent node-x)))
                   (get-node-key (node-item (node-left (node-parent (node-parent node-x)))))))
                (parent-is-in node-x node-right left-rotate right-rotate))
          (else
            (parent-is-in node-x node-left right-rotate left-rotate)))))

    (define (child-is-in node-x main-branch other-branch main-rotate other-rotate)
      (let ((node-w (main-branch (node-parent node-x))))
        (if (equal? 'red (node-color node-w))
          (begin
            (node-set-color-red! (node-parent node-x))
            (node-set-color-black! node-w)
            (main-rotate (node-parent node-x))
            (set! node-w (main-branch (node-parent node-x)))))
        (if (and (not (null-tree? node-w))
              (eq? 'black (node-color (node-left node-w)))
              (eq? 'black (node-color (node-right node-w))))
          (begin
            (node-set-color-red! node-w)
            (fixup-redblack-conditions (node-parent node-x)))
          (begin
            (if (and (not (null-tree? node-w))
                  (eq? 'black (node-color (main-branch node-w))))
              (begin
                (node-set-color-black! (other-branch node-w))
                (node-set-color-red! node-w)
                (other-rotate node-w)
                (set! node-w (main-branch (node-parent node-w)))))
            (node-set-color! node-w (node-color (node-parent node-x)))
            (node-set-color-black! (node-parent node-x))
            (if (not (null-tree? node-w))
              (node-set-color-black! (main-branch node-w)))
            (main-rotate (node-parent node-x))
            (fixup-redblack-conditions content)))))

    (define (fixup-redblack-conditions node-x)
      (if (and
            (not (same? (get-node-key (node-item node-x))
                   (get-node-key (node-item content))))
            (equal? 'black (node-color node-x)))
        (cond ((same? (get-node-key (node-item node-x))
                 (get-node-key (node-item (node-left (node-parent node-x)))))
                (child-is-in node-x node-right node-left left-rotate right-rotate))
          (else
            (child-is-in node-x node-left node-right right-rotate left-rotate)))
        (node-set-color-black! node-x)))

    ;operations
    (define (empty?)
      (null-tree? content))

    (define (lookup element)
      (define (lookup-aux node)
        (cond
          ((null-tree? node)
            #f)
          ((same? element (get-node-key (node-item node)))
            (get-node-info (node-item node)))
          ((less? element (get-node-key (node-item node)))
            (lookup-aux (node-left node)))
          (else
            (lookup-aux (node-right node)))))
      (lookup-aux content))

    (define (insert element)
      (define (insert-aux node parent)
        (cond
          ((null-tree? node)
            (let ((new-node (make-node element (make-null-tree) (make-null-tree) (make-null-tree))))
              (node-set-parent! (node-left new-node) new-node)
              (node-set-parent! (node-right new-node) new-node)
              (if (not (null-tree? parent))
                (begin
                  (node-set-parent! new-node parent)
                  (if (less? (get-node-key element) (get-node-key (node-item parent)))
                    (node-set-left! parent new-node)
                    (node-set-right! parent new-node))
                  (evaluate-redblack-conditions new-node))
                (begin
                  (set! content new-node)))
              (node-set-color-black! content)
              #t))
          ((less? (get-node-key element) (get-node-key (node-item node)))
            (insert-aux (node-left node) node))
          (else
            (insert-aux (node-right node) node))))
      (insert-aux content (make-null-tree)))

    (define (delete element)
      (define (left-most-node node parent)
        (if (null-tree? (node-left node))
          node
          (left-most-node (node-left node) node)))
      (define (delete-aux node)
        (cond
          ((null-tree? node)
            #f)
          ((same? element (get-node-key (node-item node)))
            (let* ((node-y (if (or (null-tree? (node-left node))
                                 (null-tree? (node-right node)))
                             node
                             (left-most-node (node-right node) #f)))
                    (node-x (if (null-tree? (node-left node-y))
                              (node-right node-y)
                              (node-left node-y))))
              (node-set-parent! node-x (node-parent node-y))
              (if (null-tree? (node-parent node-y))
                (set! content node-x)
                (if (and
                      (not (null-tree? (node-left (node-parent node-y))))
                      (same? (get-node-key (node-item node-y))
                        (get-node-key (node-item (node-left (node-parent node-y))))))
                  (node-set-left! (node-parent node-y) node-x)
                  (node-set-right! (node-parent node-y) node-x)))
              (if (not (same? (get-node-key (node-item node-y))
                         (get-node-key (node-item node))))
                (begin
                  (node-set-item! node (node-item node-y))))
              (if (eq? 'black (node-color node-y))
                (fixup-redblack-conditions node-x))
              #t))
          ((less? element (get-node-key (node-item node)))
            (delete-aux (node-left node)))
          (else
            (delete-aux (node-right node)))))
      (delete-aux content))

    (define (display-rb-tree)
      (traverse content 'preorder
        (lambda (element color) (display "key: ")
          (display (get-node-key element))
          (display " content: ")
          (display-line (get-node-info element)))))

    (define (rb-tree-foreach a-function)
      (traverse content 'inorder
        (lambda (element color)
          (a-function (get-node-info element))))) ;;expects procedure of 2 args: key and info

    (define (dispatch msg . args)
      (cond ((eq? msg 'empty?) (empty?))
        ((eq? msg 'insert) (insert (car args))) ;;expects conscell (key . satelite data)
        ((eq? msg 'delete) (delete (car args)))
        ((eq? msg 'lookup) (lookup (car args)))
        ((eq? msg 'for-each) (rb-tree-foreach (car args)))
        ((eq? msg 'map) (tree-acc content make-node (make-null-tree) (car args)))
        ((eq? msg 'display) (display-rb-tree))
        (else (error "unknown request -- create RBT" msg))))

    dispatch))

;;Changed Priority Queue ADT

;;Element abstractions
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
          ((<= (get-priority (car rest)) priority)
            (find-iter (cdr rest) rest))
          (else prev)))
      (find-iter (content) front))

    (define (empty?)
      (null? (content)))

    (define (enqueue element priority)
      (insert-after! (find-prev-cell priority)
        (make-item priority element))
      #t)

    (define (dequeue)
      (if (null? (content))
        #f
        (let ((temp (car (content))))
          (set-cdr! front (cdr (content)))
          (get-element temp))))

    (define (serve)
      (if (null? (content))
        #f
        (car (content))))

    (define (do-for-each afunction)
      (define (iter content)
        (if (null? content)
            'done
          (begin (afunction (car content))
            (iter (cdr content)))))
      (iter (content)))

    (define (delete element)
      (define (iter content)
        (cond ((null? (cdr content)) #f)
          ((eq? (get-element (car (cdr content))) element) (set-cdr! content (cdr (cdr content))) #t)
          (else (iter (cdr content)))))
      (iter front))

    (define (dispatch m . args)
      (cond ((eq? m 'empty?) (empty?))
        ((eq? m 'enqueue) (enqueue (car args) (cadr args)))
        ((eq? m 'dequeue) (dequeue))
        ((eq? m 'serve) (serve))
        ((eq? m 'for-each) (do-for-each (car args)))
        ((eq? m 'delete) (delete (car args)))
        (else (error "unknown request -- create-priority-queue" m))))

    dispatch))

;;Graph ADT, Edge list representation

(define (create-graph eq-label-ft directed? weighted?)
  (let ((nodes '()))
    ;edge abstraction
    (define (make-edge node1 node2 . info)
      (if weighted?
        (list node1 node2 '() (car info))
        (list node1 node2 '() 'notused)))
    (define (get-first-node edge)
      (car edge))
    (define (get-second-node edge)
      (cadr edge))
    (define (get-next-edge edge)
      (caddr edge))
    (define (set-next-edge! edge next-edge)
      (set-car! (cddr edge) next-edge))
    (define (get-edge-info edge)
      (cadddr edge))
    (define (set-edge-info! edge info)
      (set-car! (cdddr edge) info))
    ;node abstraction
    (define (make-node label info . status)
      (list label info '() (if (null? status) 'not-used (car status))))
    (define (get-label node)
      (car node))
    (define (set-label! node label)
      (set-car! node label))
    (define (get-node-info node)
      (cadr node))
    (define (set-node-info! node a-info)
      (set-car! (cdr node) a-info))
    (define (get-node-status node)
      (cadddr node))
    (define (set-node-status! node status)
      (set-car! (cdddr node) status))
    (define (get-edges node)
      (caddr node))
    (define (set-edges! node edges)
      (set-car! (cddr node) edges))
    (define (add-to-edges node edge)
      (set-next-edge! edge (get-edges node))
      (set-edges! node edge))

    (define (find-node label)
      (define (find-iter current)
        (cond
          ((null? current) #f)
          ((eq-label-ft label (get-label (car current))) (car current))
          (else (find-iter (cdr current)))))
      (find-iter nodes))
    (define (find-edge label1 label2)
      (define (find-iter current node)
        (cond
          ((null? current) #f)
          ((eq? node (get-second-node current)) current)
          (else (find-iter (get-next-edge current) node))))
      (let ((node1 (find-node label1))
             (node2 (find-node label2)))
        (if directed?
          (if (and node1 node2)
            (find-iter (get-edges node1) node2)
            #f)
          (if (and node1 node2)
            (cons (find-iter (get-edges node1) node2)
              (find-iter (get-edges node2) node1))
            (cons #f #f)))))

    (define (insert-node label info)
      (let ((node (find-node label)))
        (cond
          (node (set-node-info! node info))
          (else (set! nodes (cons (make-node label info) nodes))
            #t))))
    (define (insert-edge label1 label2 . info)
      (let ((edge (find-edge label1 label2)))
        (cond
          ((and directed? edge)
            (if weighted? (set-edge-info! edge (car info)))
            #t)
          ((and (not directed?) (car edge) (cdr edge))
            (if weighted?
              (begin (set-edge-info! (car edge) (car info))
                (set-edge-info! (cdr edge) (car info))))
            #t)
          (else
            (let* ((node1 (find-node label1))
                    (node2 (find-node label2)))
              (if (and node1 node2)
                (let ((edge (if weighted?
                              (make-edge node1 node2 (car info))
                              (make-edge node1 node2))))
                  (if directed?
                    (add-to-edges node1 edge)
                    (let ((edge-dupl (if weighted?
                                       (make-edge node2 node1 (car info))
                                       (make-edge node2 node1))))
                      (add-to-edges node1 edge)
                      (add-to-edges node2 edge-dupl)))
                  #t)
                #f))))))
    (define (delete-edge label1 label2)
      (define (delete-iter current previous node1 node2)
        (cond
          ((null? current) #f)
          ((eq? (get-second-node current) node2)
            (if previous
              (set-next-edge! previous (get-next-edge current))
              (set-edges! node1 (get-next-edge current)))
            #t)
          (else
            (delete-iter (get-next-edge current) current node1 node2))))
      (let ((node1 (find-node label1))
             (node2 (find-node label2)))
        (if (and node1 node2)
          (if directed?
            (delete-iter (get-edges node1) #f node1 node2)
            (and
              (delete-iter (get-edges node1) #f node1 node2)
              (delete-iter (get-edges node2) #f node2 node1)))
          #f)))
    (define (delete-node label)
      (define (delete-iter current prev)
        (cond
          ((null? current) #f)
          ((eq-label-ft label (get-label (car current)))
            (foreach-neighbour label
              (lambda (from-lbl from-info to-lbl to-info edge-info)
                (delete-edge from-lbl to-lbl)))
            (if prev
              (set-cdr! prev (cdr current))
              (set! nodes (cdr nodes)))
            #t)
          (else (delete-iter (cdr current) current))))
      (delete-iter nodes #f))
    (define (lookup-node-info label)
      (let ((node (find-node label)))
        (if node
          (get-node-info node)
          #f)))
    (define (change-node-info label info)
      (let ((node (find-node label)))
        (if node
          (set-node-info! node info)
          #f)))
    (define (lookup-node-status label)
      (let ((node (find-node label)))
        (if node
          (get-node-status node)
          #f)))
    (define (change-node-status label status)
      (let ((node (find-node label)))
        (if node
          (set-node-status! node status)
          #f)))
    (define (lookup-edge label1 label2)
      (let ((edge (find-edge label1 label2)))
        (if directed?
          (if edge
            (if weighted? (get-edge-info edge) #t)
            #f)
          (if (and (car edge) (cdr edge))
            (if weighted? (get-edge-info (car edge)) #t)
            #f))))
    (define (empty?)
      (null? nodes))
    (define (map-over-nodes a-function)
      ;a-function takes two argument, i.e. label and info of the node
      (define (map-iter current result)
        (if (null? current)
          (reverse result)
          (map-iter (cdr current)
            (cons (a-function (get-label (car current))
                    (get-node-info (car current)))
              result))))
      (map-iter nodes '()))
    (define (foreach-node a-action)
      ;a-action takes two argument, i.e. label and info of the node
      (define (foreach-iter current)
        (cond
          ((null? current) #t)
          (else
            (a-action (get-label (car current)) (get-node-info (car current)))
            (foreach-iter (cdr current)))))
      (foreach-iter nodes))
    (define (map-over-neighbours label a-function)
      ;;a-function takes 5 arguments
      ;;from-label from-info to-label to-info edge-info
      ;;edge-info is not used in a non weighted graph
      (let ((node (find-node label)))
        (define (edges-iter current result)
          (if (null? current)
            result
            (let* ((neigbour-node (get-second-node current))
                    (res (a-function
                           (get-label node)
                           (get-node-info node)
                           (get-label neigbour-node)
                           (get-node-info neigbour-node)
                           (get-edge-info current))))
              (edges-iter (get-next-edge current) (cons res result)))))
        (if node
          (edges-iter (get-edges node) '())
          #f)))
    (define (foreach-neighbour label a-action)
      ;;a-action takes 5 arguments
      ;;from-label from-info to-label to-info edge-info
      ;;edge-info is not used in a non weighted graph
      (let ((node (find-node label)))
        (define (edges-iter current)
          (if (null? current)
            #t
            (let* ((neigbour-node (get-second-node current)))
              (a-action
                (get-label node)
                (get-node-info node)
                (get-label neigbour-node)
                (get-node-info neigbour-node)
                (get-edge-info current))
              (edges-iter (get-next-edge current)))))
        (if node
          (edges-iter (get-edges node))
          #f)))

    (define (dispatch msg . args)
      (cond ((eq? msg 'empty?) (empty?))
        ((eq? msg 'insert-node) (apply insert-node args))
        ((eq? msg 'delete-node) (apply delete-node args))
        ((eq? msg 'insert-edge) (apply insert-edge args))
        ((eq? msg 'delete-edge) (apply delete-edge args))
        ((eq? msg 'lookup-node-info) (apply lookup-node-info args))
        ((eq? msg 'change-node-info) (apply change-node-info args))
        ((eq? msg 'lookup-node-status) (apply lookup-node-status args))
        ((eq? msg 'change-node-status) (apply change-node-status args))
        ((eq? msg 'lookup-edge) (apply lookup-edge args))
        ((eq? msg 'map-over-nodes) (apply map-over-nodes args))
        ((eq? msg 'foreach-node) (apply foreach-node args))
        ((eq? msg 'map-over-neighbours) (apply map-over-neighbours args))
        ((eq? msg 'foreach-neighbour) (apply foreach-neighbour args))
        ((eq? msg 'find-node) (apply find-node args))
        (else (error "unknown request -- create-graph" msg))))

    dispatch))

;;Hashtable ADT

(define (create-hashtable-adt size hash-function . equal-function)
  ;;External Chaining implementation
  (let ((content (make-vector size '()))
         (same? (if (null? equal-function) = (car equal-function))))

    ;;Abstractions:
    (define (make-item key info) (cons key info))
    (define (key item) (car item))
    (define (info item) (cdr item))
    ;;Done

    (define (insert a-key info)
      (let* ((position (hash-function a-key))
              (data (vector-ref content position)))
        (vector-set! content position (cons (make-item a-key info) data))
        #t))

    (define (delete a-key)
      (let* ((position (hash-function a-key))
              (data (vector-ref content position)))

        (define (delete a-key a-list)
          (cond ((null? a-list) a-list)
            ((eq? (key (car a-list)) a-key) (cdr a-list))
            (else (cons (car a-list)
                    (delete a-key (cdr a-list))))))
        (if (null? data)
          #t
          (begin (vector-set! content position (delete a-key data))
            #t))))

    (define (search a-key)
      (let* ((position (hash-function a-key))
              (data (vector-ref content position)))
        (define (search-in-list a-key a-list)
          (cond ((null? a-list) #f)
            ((eq? (key (car a-list)) a-key) (info (car a-list)))
            (else (search-in-list a-key (cdr a-list)))))
        (if (null? data)
          #f
          (search-in-list a-key data))))

    (define (show)
      (define (iter n)
        (if (= n (- (vector-length content) 1))
          (newline)
          (begin (let ((data (vector-ref content n)))
                   (for-each (lambda (item)
                               (display "Key: ")
                               (display (key item))
                               (display " ")
                               (display "Content: ")
                               (display (info item))
                               (newline))
                     data))
            (iter (+ 1 n)))))
      (iter 0))

    (define (dispatch m . args)
      (cond ((eq? m 'insert) (insert (car args) (cadr args)))
        ((eq? m 'delete) (delete (car args)))
        ((eq? m 'search) (search (car args)))
        ((eq? m 'show) (show))
        (else (error "Could not handle your message -- Create Hashtable ADT" m))))
    dispatch))

;;Another Hashtable ADT

(define (create-hashtable-w-bst-adt size hash-function . equal-function)
  ;;External Chaining implementation, buckets are binary search trees

  (define (filled-vector n)
    (let ((the-vector (make-vector n)))
      (do ((counter 0 (+ 1 counter)))
        ((= counter n) the-vector)
        (vector-set! the-vector counter (create-bst eq? tree-less?)))))

  (let ((content (filled-vector size))
         (same? (if (null? equal-function) = (car equal-function)))
         (nr-of-empty-buckets 0)) ;;to test performance

    ;;Abstractions:
    (define (make-item key info) (cons key info))
    (define (key item) (car item))
    (define (info item) (cdr item))
    ;;Done

    (define (insert a-key info)
      (let* ((position (hash-function a-key))
              (the-tree (vector-ref content position)))
        (the-tree 'insert (make-item a-key info))
        #t))

    (define (delete a-key)
      (let* ((position (hash-function a-key))
              (the-tree (vector-ref content position)))
        (the-tree 'delete a-key)))

    (define (search a-key)
      (let* ((position (hash-function a-key))
              (the-tree (vector-ref content position)))
        (if (the-tree 'empty?)
          #f
          (the-tree 'lookup a-key))))

    (define (show)
      (define (iter n)
        (if (= n (- (vector-length content) 1))
          (newline)
          (begin (let ((the-tree (vector-ref content n)))
                   (if (not (the-tree 'empty?))
                     (begin (display "Data on key ")
                       (display n)
                       (display ": ")
                       (the-tree 'for-each
                         (lambda (element)
                           (display (if (procedure? element)
                                      (element 'give-name)
                                      element))
                           (display " ")))
                       (newline))
                     (set! nr-of-empty-buckets (+ 1 nr-of-empty-buckets))))
            (iter (+ 1 n)))))
      (iter 0)
      (begin (display nr-of-empty-buckets)
        (display-line " empty buckets")
        (set! nr-of-empty-buckets 0)
          'done))

    (define (dispatch m . args)
      (cond ((eq? m 'insert) (insert (car args) (cadr args)))
        ((eq? m 'delete) (delete (car args)))
        ((eq? m 'search) (search (car args)))
        ((eq? m 'tree) (vector-ref content (car args)))
        ((eq? m 'show) (show))
        (else (error "Could not handle your message -- Hashtable with BST ADT" m))))

    dispatch))

;;THe Eventhandler ADT
;;Implemented with the Modified Priority Queue ADT

(define (create-eventhandler-ADT starting-time)
  (let ((action-queue (create-priority-queue))
         (time starting-time) ;;internal time of the eventhandler
         (runtime starting-time))
    ;;Runtime is a variable that will be useful when the eventhandler is
    ;;executing to prevent loops

    (define (the-time element) (car element))
    (define (the-action element) (cdr element))

    (define (for-each-action do-something)
      (action-queue 'for-each do-something))

    (define (execute an-action)
      ;;Asks the ADT action to execute itself
      (an-action 'execute))

    (define (execute-step step)
      (define (iter)
        (if ;;Checks whether the time is smaller than the step size
          ;;and whether the queue is empty or not
          (and (not (action-queue 'empty?))
            (<= (the-time (action-queue 'serve)) (+ time step)))
          (begin (let ((new-action (action-queue 'serve)))
                   (action-queue 'dequeue)
                   (execute (the-action new-action)))
            (iter))
          (begin (set! time (+ time step))
              'done)))
      (set! runtime (+ time step 1))
      (iter))

    (define (add an-action a-time)
      (cond ((null? a-time)
              ;;Execute the action directly if no time is given
              (execute an-action))
        ((eq? (the-time a-time) 'hold)
          ;;If the action must be held, enqueue it with highest priority
          (action-queue 'enqueue an-action 0))
        ;;Enqueue the action with the time as its priority
        (else (action-queue 'enqueue an-action (the-time a-time)))))

    (define (delete an-action a-player)
      ;;Delete all actions of a type from a player from the eventhandler queue
      (for-each-action
        (lambda (element)
          (if (and (eq? an-action ((the-action element) 'give-name))
                (eq? a-player ((the-action element) 'give-executer)))
            ;;Asks the ADT action who performed the action
            (action-queue 'delete (the-action element))))))

    (define (delete-all a-player)
      ;;Delete all actions of a player in the queue
      (for-each-action
        (lambda (element)
          (if (eq? a-player ((the-action element) 'give-executer))
            (action-queue 'delete (the-action element))))))

    (define (show)
      ;;Shows the queue
      (if (action-queue 'empty?)
          'empty
        (begin
          (for-each-action
            (lambda (element)
              (display "Time: ")
              (display (the-time element))
              (display " Executer: ")
              (display ((the-action element) 'give-executer))
              (display " Action: ")
              (display-line ((the-action element) 'give-name))))
          (display "Eventhandler time is: ")
          (display-line time))))

    (define (change-time new-time)
      ;;Change the eventhandler's time
      (set! time new-time)
        'done)

    (define (dispatch m . args)
      ;;Functions are immediately executed with given arguments
      (cond ((eq? m 'add) (add (car args) (cdr args))) ;;Expects action ADT and (optionally) an integer or symbol hold
        ((eq? m 'delete) (delete (car args) (cadr args))) ;;Expects action symbol and player symbol
        ((eq? m 'for-each-action) (for-each-action (car args))) ;;Expects procedure
        ((eq? m 'delete-all) (delete-all (car args))) ;;Expects player symbol
        ((eq? m 'show) (show))
        ((eq? m 'execute-step) (execute-step (car args))) ;;Expects integer
        ((eq? m 'change-time) (change-time (car args))) ;;Expects integer
        ((eq? m 'give-time) time) ;;Returns integer
        ((eq? m 'give-runtime) runtime) ;;Returns integer
        (else (error "Eventhandler ADT Could not handle your message " m))))

    dispatch))

;;The Action ADT

(define (create-action-adt name an-executor parameterlist) ; <<== Changed.
  ;;name is a symbol representing an action
  ;;an-executor is also a symbol representing an executor
  ;;parameterlist is a list of extra parameters
  (if (not (the-actions-table 'lookup name))
    (error "Action Type not found in The Action Table -- Create Action ADT" name)
    ;;Creates an action ADT that will keep track of the executer of a function
    ;;The function is actually the function that will execute the specified action
    ;;This function will be looked up in the action table
    ;;The action table is a globally defined instance of the object (action-table)
    (let ((action-name name)
           (action-function (the-actions-table 'give-function name))
           (action-executor an-executor)
           (action-parameters parameterlist))

      (define (dispatch m)
        (cond ((eq? m 'give-name) action-name) ;;Returns symbol
          ((eq? m 'give-function) action-function) ;;Returns procedure
          ((eq? m 'give-executer) action-executor) ;;Returns an ADT
          ((eq? m 'give-parameters) action-parameters) ;;Returns list of parameters
          ((eq? m 'execute) (apply action-function (cons action-executor action-parameters)))
          (else (error "Could not handle your request -- Create Action ADT" m))))

      dispatch)))

;;The Table ADT
;;For use as an efficient dictionary thanks to the hashtable implementation

(define (create-table-adt size equalfunction)

  (define (the-hashfunction key)
    (modulo (chars->numbers key) size))

  (let ((the-table (create-hashtable-adt size the-hashfunction equalfunction))
         (items-in-table 0 .0)
         (loadfactor 1))

    (define (update-loadfactor updateproc)
      (set! items-in-table (updateproc items-in-table 1))
      (set! loadfactor (/ items-in-table size)))

    (define (add key entry)
      (update-loadfactor +)
      (the-table 'insert key entry))

    (define (delete key)
      (update-loadfactor -)
      (the-table 'delete key))

    (define (lookup key)
      (the-table 'search key))

    (define (show)
      (the-table 'show)
      (display "Items in table: ")
      (display-line items-in-table))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (add (car args) (cadr args))) ;;Expects symbol and an ADT
        ((eq? m 'delete) (delete (car args))) ;;Expects symbol
        ((eq? m 'lookup) (lookup (car args))) ;;Expects symbol
        ((eq? m 'give-loadfactor) loadfactor)
        ((eq? m 'show) (show))
        (else (error "Create-Table-ADT could not handle your request" m))))

    dispatch))

;;Another Table ADT, but this one uses the Hashtable with
;;rehashing in BST's as underlying implementation

(define (create-bst-table-adt size equalfunction)

  (define (the-hashfunction key)
    (modulo (chars->numbers key) size))

  (let ((the-table (create-hashtable-w-bst-adt size the-hashfunction equalfunction))
         (items-in-table 0 .0)
         (loadfactor 1))

    (define (update-loadfactor updateproc)
      (set! items-in-table (updateproc items-in-table 1))
      (set! loadfactor (/ items-in-table size)))

    (define (add key entry)
      (update-loadfactor +)
      (the-table 'insert key entry))

    (define (delete key)
      (update-loadfactor -)
      (the-table 'delete key))

    (define (lookup key)
      (the-table 'search key))

    (define (show)
      (the-table 'show)
      (display "Items in table: ")
      (display-line items-in-table))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (add (car args) (cadr args))) ;;Expects symbol and an ADT
        ((eq? m 'delete) (delete (car args))) ;;Expects symbol
        ((eq? m 'lookup) (lookup (car args))) ;;Expects symbol
        ((eq? m 'give-loadfactor) loadfactor)
        ((eq? m 'show) (show))
        (else (error "Binary Search Tree Table could not handle your request" m))))

    dispatch))

;;Inventory Tree ADT
;;this is the Tree which will store all weapons and items of a player

(define (create-inventory-tree)

  (let ((the-tree (create-redblack-tree eq? tree-less?)))
    ;;The Tree is a Red Black Tree
    ;;a Binary Search tree, which takes as input a conscell of the form '(key . data)

    ;;Abstractions
    (define (create-package name adt) (cons name adt))
    (define (the-name item) (car item))
    (define (the-adt item) (cdr item))
    ;;Done

    (define (add an-item)
      (the-tree 'insert (create-package (an-item 'give-name) an-item)))

    (define (delete an-item)
      (the-tree 'delete an-item))

    (define (lookup an-item)
      (the-tree 'lookup an-item))

    (define (show)
      (the-tree 'for-each
        (lambda (item)
          (display-line (item 'give-name)))))

    (define (for-each an-action)
      (the-tree 'for-each an-action))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (add (car args))) ;;Expects item ADT
        ((eq? m 'delete) (delete (car args))) ;;Expects symbol
        ((eq? m 'lookup) (lookup (car args))) ;;Expects symbol
        ((eq? m 'for-each) (for-each (car args))) ;;Expects procedure
        ((eq? m 'show) (show))
        (else (error "Could not handle your request -- Create Inventory" m))))

    dispatch))

;;The Inventory ADT

(define (create-inventory-adt possesor-name)
  ;;Based on an Inventory Tree ADT
  ;;An inventory stocks 2 different ADT's, it stocks Items and Weapons
  (let ((the-inventory (create-inventory-tree))
         (inventory-possesor possesor-name))

    (define (add item)
      (if (symbol? item)
        (error "expects ADT not a name -- ADD -- INVENTORY" item)
        ;;Expects an ADT as an input
        (begin (set-item-possesor item inventory-possesor)
          (the-inventory 'add item))))

    (define (give item)
      (if (symbol? item)
        (the-inventory 'lookup item)
        (error "Expect symbol not an ADT -- GIVE -- INVENTORY" item)))

    (define (drop item)
      (if (symbol? item)
        (let ((the-item (give item)))
          (if the-item
            (begin (set-item-possesor the-item 'nobody)
              (the-inventory 'delete item))
            (display-line "You haven't got that item")))
        (error "Expects symbol not an ADT -- DROP -- INVENTORY" item)))

    (define (use item)
      (let ((the-item (if (symbol? item)
                        (give item)
                        item)))
        (if the-item
          (if (eq? (the-item 'give-possesor) 'nobody)
            (begin (set-item-possesor the-item inventory-possesor)
              (the-item 'use))
            (the-item 'use))
          (begin (display "The item ")
            (display (get-object-name item))
            (display-line " was not found in your inventory")))))

    (define (for-each an-action)
      (the-inventory 'for-each an-action))

    (define (show)
      (the-inventory 'show))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (add (car args))) ;;Expects Weapon/Item ADT
        ((eq? m 'drop) (drop (car args))) ;;Expects symbol
        ((eq? m 'give) (give (car args))) ;;Expects symbol
        ((eq? m 'use) (use (car args))) ;;Expects symbol
        ((eq? m 'for-each) (for-each (car args))) ;;Expects procedure
        ((eq? m 'show) (show))
        (else (error "Could not handle your request -- Create Inventory ADT" m))))

    dispatch))

;;The Item ADT

(define (create-item-adt name)
  (if (not (the-items-table 'give-name name))
    (error "Item Stats not found in the Items Table -- Create Item ADT" name)
    ;;An item is not a weapon or a spell, it is an object
    ;;Relevant info is looked up in the items table
    (let* ((item-name (the-items-table 'give-name name))
            (item-description (the-items-table 'give-description name))
            (item-action-names (the-items-table 'give-functions name))
            (item-possesor 'nobody)
            (extras (the-items-table 'give-extra name)))

      (define (use)
        ;;First, the name of the action of this item is looked up in the items table
        ;;Then, an Action ADT is made of the name found in the items table
        ;;This Action ADT is passed to the eventhandler
        ;;The item possesor is just a symbol, to get the ADT, look it up in the character table
        (for-each
          (lambda (item-action)
            (let* ((item-action-name (car item-action))
                    (item-action-pars (cdr item-action))
                    (item-actions
                      (create-action-adt item-action-name item-possesor (list item-action-pars))))
              (core-eventhandler 'add item-actions 'hold)))
          item-action-names)

        (if (eq? extras 'delete-after-use)
          (delete-item-for-character item-name item-possesor))
        (execute-actions))

      (define (dispatch m . args)
        (cond ((eq? m 'give-name) item-name) ;;Returns symbol
          ((eq? m 'give-description) item-description) ;;Returns string
          ((eq? m 'give-possesor) item-possesor) ;;Returns symbol
          ((eq? m 'use) (use))
          ((eq? m 'set-description!) (set! item-description (car args))) ;;Expects string
          ((eq? m 'set-possesor!) (set! item-possesor (car args))) ;;Expects symbol
          ((eq? m 'get-type) 'item) ;;to identify between weapons and items
          (else (error "Could not handle your request -- Create Item ADT" m))))

      dispatch)))

;;The Weapon ADT

(define (create-weapon-adt name)

  ;;Weapons are: swords, shields, axes, but also Helmets and other Equipment
  ;;Each weapon has a name, a description, a bonus (for some "Magical" Weapons), but
  ;;Most important: a series of Combat Modifiers
  ;;Looks up arguments in the weapons table

  (if (not (the-weapons-table 'give-name name))
    (error "Weapon was not found in the Weapons Database -- Create Weapon ADT" name)
    (let* ((weapon-name (the-weapons-table 'give-name name))
            (weapon-description (the-weapons-table 'give-description name))
            (weapon-statistics (the-weapons-table 'give-statistics name))
            (weapon-bonus (the-weapons-table 'give-bonus name))
            (weapon-possesor 'nobody)
            (attack-damage (list-ref weapon-statistics 0))
            (defense-damage (list-ref weapon-statistics 1))
            (attack-bonus (list-ref weapon-statistics 2))
            (defense-bonus (list-ref weapon-statistics 3))
            (extras (the-weapons-table 'give-extra name)))

      (define (use)

        ;;First, the name of the action of this weapon is looked up in the items table
        ;;Then, an Action ADT is made of the name found in the weapons table
        ;;This Action ADT is passed to the eventhandler
        ;;The weapon possesor is just a symbol, to get the ADT, look it up in the character table

        (for-each
          (lambda (weapon-action)
            (let* ((weapon-action-name (car weapon-action))
                    (weapon-action-pars (cdr weapon-action))
                    (weapon-actions
                      (create-action-adt weapon-action-name weapon-possesor (list weapon-action-pars))))
              (core-eventhandler 'add weapon-actions 'hold)))
          weapon-bonus)

        (if (eq? extras 'delete-after-use)
          (delete-item-for-character weapon-name weapon-possesor))
        (execute-actions))

      ;;A weapon has 4 combat modifiers:
      ;;Attack Damage: the damage the weapon can (maximally) deal
      ;;Defense Damage: the damage a weapon deals when the defender makes a counter-attack
      ;;These are expressed as a pair of (number of dice . number of sides of the dice)
      ;;Eg. '(2 . 6) => 2d6 => 2 dice of 6 sides, so maximum damage is 12
      ;;Attack Bonus: some weapons will give the wielder a bonus
      ;;Defense Bonus: items like Helmets and Plate Mail Armor
      ;;will give the wearer defensive bonusses

      (define (dispatch m . args)
        (cond ((eq? m 'give-name) weapon-name) ;;Returns symbol
          ((eq? m 'give-description) weapon-description) ;;Returns string
          ((eq? m 'give-possesor) weapon-possesor) ;;Returns symbol
          ((eq? m 'give-attack-damage) attack-damage) ;;Returns pair
          ((eq? m 'give-defense-damage) defense-damage) ;;Returns pair
          ((eq? m 'give-attack-bonus) attack-bonus) ;;Returns integer
          ((eq? m 'give-defense-bonus) defense-bonus) ;;Returns integer
          ((eq? m 'set-attack-damage!) (set! attack-damage (car args))) ;;Expects pair
          ((eq? m 'set-defense-damage!) (set! defense-damage (car args))) ;;Expects pair
          ((eq? m 'set-attack-bonus!) (set! attack-bonus (car args))) ;;Expects integer
          ((eq? m 'set-defense-bonus!) (set! defense-bonus (car args))) ;;Expects integer
          ((eq? m 'set-description!) (set! weapon-description (car args))) ;;Expects string
          ((eq? m 'set-possesor!) (set! weapon-possesor (car args))) ;;Expects symbol
          ((eq? m 'use) (use))
          ((eq? m 'get-type) 'weapon) ;;to identify between weapons and items
          (else (error "Could not handle your request -- Create Weapon ADT" m))))

      dispatch)))

;;The Location ADT

(define (create-location-adt name)

  ;;Locations are 'rooms' in the world
  ;;Looks up its arguments in the location table automatically
  ;;the-location-table is a table created by the object location-table

  (if (not (the-locations-table 'lookup name))
    (error "The record of that Location Name was not found in the Location table -- Create location ADT" name)
    (let* ((locations the-locations-table)
            (location-name (locations 'give-name name))
            (location-monsters (locations 'give-monster name))
            (location-npcs (locations 'give-npc name))
            (location-description (locations 'give-description name))
            (location-alterdescr (locations 'give-alterdescription name))
            (location-items (locations 'give-item name))
            (location-actions (locations 'give-actions name))
            (location-players '()))

      (define (give key)
        ;;Gives the data asked for
        (cond ((eq? key 'name) location-name)
          ((eq? key 'monster) location-monsters)
          ((eq? key 'npc) location-npcs)
          ((eq? key 'description) location-description)
          ((eq? key 'alterdescription) location-alterdescr)
          ((eq? key 'item) location-items)
          ((eq? key 'actions) location-actions)
          ((eq? key 'players) location-players)
          (else (error "wrong key type -- Give --Create Location ADT" key))))

      (define (set-item! key new-value)
        ;;Changes the data asked for
        ;;NOTE: these changes only affect the local parameters,
        ;;so the Location TABLE itself will NOT be changed!
        (cond ((eq? key 'name) (set! location-name new-value) #t)
          ((eq? key 'monster) (set! location-monsters new-value) #t)
          ((eq? key 'npc) (set! location-npcs new-value) #t)
          ((eq? key 'description) (set! location-description new-value) #t)
          ((eq? key 'alterdescription) (set! location-alterdescr new-value) #t)
          ((eq? key 'item) (set! location-items new-value) #t)
          ((eq? key 'action) (set! location-actions new-value) #t)
          ((eq? key 'players) (set! location-players new-value) #t)
          (else (error "wrong key type -- Set-item! --Create Location ADT" key))))

      (define (dispatch m . args)
        ;;The dispatcher dispatches to the give and set-item procedures,
        ;;to minimize complexity
        (cond ((eq? m 'give-name) (give 'name)) ;;Returns symbol
          ((eq? m 'give-monster) (give 'monster)) ;;Returns list of symbols
          ((eq? m 'give-npc) (give 'npc)) ;;Returns list of symbols
          ((eq? m 'give-description) (give 'description)) ;;Returns string
          ((eq? m 'give-alterdescription) (give 'alterdescription)) ;;Returns string
          ((eq? m 'give-players) (give 'players)) ;;Returns list of symbols
          ((eq? m 'give-item) (give 'item)) ;;Returns list of symbols
          ((eq? m 'give-actions) (give 'actions)) ;;Returns list of symbols
          ((eq? m 'set-name!) (set-item! 'name (car args))) ;;Expects symbol
          ((eq? m 'set-monster!) (set-item! 'monster (car args))) ;;Expects list of symbols
          ((eq? m 'set-npc!) (set-item! 'npc (car args))) ;;Expects list of symbols
          ((eq? m 'set-description!) (set-item! 'description (car args))) ;;Expects string
          ((eq? m 'set-alterdescription!) (set-item! 'alterdescription (car args))) ;;Expects string
          ((eq? m 'set-item!) (set-item! 'item (car args))) ;;Expects list of adts
          ((eq? m 'set-action!) (set-item! 'action (car args))) ;;Expects list of symbols
          ((eq? m 'set-players!) (set-item! 'players (car args))) ;;Expects list of symbols
          (else (error "Create Location ADT could not handle your request" m))))

      dispatch)))

;;The Monster ADT

(define (create-monster-adt name class startingpoint . route-of-travelling)

  ;;Monsters will be the enemies of players

  (if (not (the-class-table 'lookup class))
    (error "Class not found in Class table -- Create Monster ADT" class)
    (let* ((monster-name name)
            (monster-class class)
            (class-parameters (the-class-table 'give-parameters class))
            (strength (car class-parameters))
            (constitution (cadr class-parameters))
            (wisdom (list-ref class-parameters 2))
            (intelligence (list-ref class-parameters 3))
            (dexterity (list-ref class-parameters 4))
            (charisma (list-ref class-parameters 5))
            (heropoints (+ (abs (- strength constitution)) wisdom))
            (monster-respawn (the-class-table 'give-respawn class))
            (monster-location startingpoint)
            (monster-world the-world) ;;monsters always reside in 'the world'
            (monster-hitpoints (the-class-table 'give-hitpoints class))
            (monster-inventory (create-inventory-adt name))
            (monster-spellbook (create-spellbook-adt name))
            (route (if (null? route-of-travelling)
                     monster-location
                     (car route-of-travelling)))
            (current-offensive-weapon (car (the-class-table 'give-weapons class)))
            (current-defensive-weapon (cadr (the-class-table 'give-weapons class)))
            (status 'new))

      ;;The following three procedures will setup the monster's possesions

      (define (fill-items)
        ;;the possesions is a list of NAMES of the monster's possesions
        ;;the items are created here.
        (define (iter objects)
          (if (null? objects)
              'done
            (begin (monster-inventory 'add (create-item-adt (car objects)))
              (iter (cdr objects)))))
        (let ((possesions (the-class-table 'give-items class)))
          (iter possesions)))

      (define (fill-weapons)
        (define (iter objects)
          (if (null? objects)
              'done
            (begin (monster-inventory 'add (create-weapon-adt (car objects)))
              (iter (cdr objects)))))
        (let ((possesions (the-class-table 'give-weapons class)))
          (iter possesions)))

      (define (fill-spellbook)
        (define (iter spells)
          (if (null? spells)
              'done
            (begin (monster-spellbook 'add (create-spell-adt (car spells)))
              (iter (cdr spells)))))
        (let ((possesions (the-class-table 'give-spells class)))
          (iter possesions)))

      (define (walk-through-route)
        ;;This algorithm lets a monster travel through a given route

        (let ((time (core-eventhandler 'give-runtime)))
          ;;Rotates the routelist and changes the monster location
          (define (last alist)
            (if (null? (cdr alist))
              alist
              (last (cdr alist))))

          (define (rotate)
            ;;Destructively manipulate the routelist
            (let ((next-el (cdr route))
                   (last-el (last route)))
              (set-cdr! last-el (list (car route)))
              (set-cdr! route '())
              (set! route next-el)

              ;;Changes monster location and changes locations
              (monster-location 'set-monster!
                (delete-list monster-name (monster-location 'give-monster)))
              (set! monster-location (car route))
              (monster-location 'set-monster!
                (cons monster-name (monster-location 'give-monster)))

              ;;The monster automatically enqueues that he will travel again in 20 turns
              (let ((walk-again-action (create-action-adt 'monster-travelling monster-name '())))
                (core-eventhandler 'add walk-again-action (+ 20 time)))
              #t))

          (if (list? route)
            (rotate))))

      (define (has-item? an-item)
        (monster-inventory 'search an-item))

      (define (has-spell? a-spell)
        (monster-spellbook 'search a-spell))

      (define (show-inventory)
        (display-line "Monster Possesions: ")
        (monster-inventory 'show)
        (newline))

      (define (show-spellbook)
        (display-line "Monster Spells: ")
        (monster-spellbook 'show)
        (newline))

      (define (show-status)
        (display "Information about monster: ")
        (display-line monster-name)
        (display "Class: ") (display-line monster-class)
        ;(display-line monster-description) ; <<== Changed.
        (display-line "Personal Parameters:")
        (display "Current Hitpoints: ") (display-line monster-hitpoints)
        (display "Strength: ") (display-line strength)
        (display "Constitution: ") (display-line constitution)
        (display "Wisdom: ") (display-line wisdom)
        (display "Intelligence: ") (display-line intelligence)
        (display "Dexterity: ") (display-line dexterity)
        (display "Charisma: ") (display-line charisma)
        (newline)
        (display "Current Location: ") (display-line (monster-location 'give-name))
        (display "Currently equiped offensive weapon: ")
        (display-line (get-object-name current-offensive-weapon))
        (display "Currently equiped defensive weapon: ")
        (display-line (get-object-name current-defensive-weapon))
        (display "Path guarded: ")
        (if (list? route)
          (for-each (lambda (location)
                      (display (location 'give-name))
                      (display " "))
            route)
          (display "monster does not guard a path"))
        (newline)
        (show-inventory)
        (show-spellbook))

      (define (set-monster-hitpoints! value)
        (set! monster-hitpoints value)
        (if (< monster-hitpoints 1)
          (begin (set! status 'dead)
            (core-eventhandler 'delete-all monster-name)
            ;;Delete monster from its location
            (let ((monsters-in-location (monster-location 'give-monster)))
              (monster-location 'set-monster! (delete-list monster-name monsters-in-location)))
            (drop-all monster-name display-off)
            (the-character-table 'delete monster-name)
            (respawn monster-name monster-class monster-location route monster-respawn)
            ;;some monsters can respawn when they die
            (display monster-name) (display-line " is dead."))))

      (define (dispatch m . args)
        (cond ((eq? m 'give-name) monster-name) ;;Returns symbol
          ((eq? m 'give-class) monster-class) ;;Returns symbol
          ((eq? m 'give-strength) strength) ;;Returns integer
          ((eq? m 'give-constitution) constitution) ;;Returns integer
          ((eq? m 'give-wisdom) wisdom) ;;Returns integer
          ((eq? m 'give-intelligence) intelligence) ;;Returns integer
          ((eq? m 'give-dexterity) dexterity) ;;Returns integer
          ((eq? m 'give-charisma) charisma) ;;Returns integer
          ((eq? m 'give-heropoints) heropoints) ;;Returns integer
          ((eq? m 'give-location) monster-location) ;;Returns location ADT
          ((eq? m 'give-world) monster-world) ;;Returns world ADT
          ((eq? m 'give-hitpoints) monster-hitpoints) ;;Returns integer
          ((eq? m 'give-route) route) ;;Returns list of location ADTs
          ((eq? m 'give-respawn) monster-respawn) ;;Returns boolean or number
          ((eq? m 'give-status) status) ;;Returns symbol
          ((eq? m 'give-offensive-weapon) current-offensive-weapon) ;;Returns Weapon ADT
          ((eq? m 'give-defensive-weapon) current-defensive-weapon) ;;Returns Weapon ADT
          ((eq? m 'add-item) (monster-inventory 'add (car args))) ;;Expects Item ADT
          ((eq? m 'drop-item) (monster-inventory 'drop (car args))) ;;Expects Item symbol
          ((eq? m 'add-spell) (monster-spellbook 'add (car args))) ;;Expects Spell ADT
          ((eq? m 'erase-spell) (monster-spellbook 'erase (car args))) ;;Expects Spell symbol
          ((eq? m 'walk-through-route) (walk-through-route))
          ((eq? m 'has-item?) (has-item? (car args))) ;;Returns boolean
          ((eq? m 'has-spell?) (has-spell? (car args))) ;;Returns boolean
          ((eq? m 'show-status) (show-status))
          ((eq? m 'show-inventory) (show-inventory))
          ((eq? m 'show-spellbook) (show-spellbook))
          ((eq? m 'set-name!) (set! monster-name (car args))) ;;Expects symbol
          ((eq? m 'set-strength!) (set! strength (car args))) ;;Expects integer
          ((eq? m 'set-constitution!) (set! constitution (car args))) ;;Expects integer
          ((eq? m 'set-wisdom!) (set! wisdom (car args))) ;;Expects integer
          ((eq? m 'set-intelligence!) (set! intelligence (car args))) ;;Expects integer
          ((eq? m 'set-dexterity!) (set! dexterity (car args))) ;;Expects integer
          ((eq? m 'set-charisma!) (set! charisma (car args))) ;;Expects integer
          ((eq? m 'set-heropoints!) (set! heropoints (car args))) ;;Expects integer
          ((eq? m 'set-location!) (set! monster-location (car args))) ;;Expects location ADT
          ((eq? m 'set-hitpoints!) (set-monster-hitpoints! (car args))) ;;Expects integer
          ((eq? m 'set-route!) (set! route (car args))) ;;Expects list of Location ADTs
          ((eq? m 'set-offensive-weapon!) (set! current-offensive-weapon (car args))) ;;Expects Weapon ADT
          ((eq? m 'set-defensive-weapon!) (set! current-defensive-weapon (car args))) ;;Expects Weapon ADT
          ((eq? m 'set-status!) (set! status (car args))) ;;Expects symbol
          ((eq? m 'give-inventory) monster-inventory) ;;Returns inventory ADT
          ((eq? m 'give-spellbook) monster-spellbook) ;;Returns spellbook ADT
          ((eq? m 'character-type) 'monster) ;;to distinguish between monsters players and NPC's
          (else (error "Create Monster ADT could not handle your request" m))))

      (if (eq? status 'new)
        ;;Initialise monster
        (begin (fill-items)
          (fill-weapons)
          (fill-spellbook)
          (set! status 'alive)
          ;;Begin the guarding route
          (walk-through-route)
          dispatch)

        dispatch))))

;;The NPC ADT

(define (create-npc-adt name)
  (if (not (the-npc-table 'give-name name))
    (error "Name not found in NPC table -- Create NPC ADT" name)
    (let ((npc-name (the-npc-table 'give-name name))
           (npc-location (the-npc-table 'give-startingpoint name))
           (npc-conversation (the-npc-table 'give-conversation name))
           (npc-description (the-npc-table 'give-description name))
           (npc-conditions (the-npc-table 'give-q-conditions name))
           (npc-quest (the-npc-table 'give-quest name))
           (npc-inventory (create-inventory-adt name))
           (status 'new))

      (define (fill-items)
        (define (iter objects)
          (if (null? objects)
              'done
            (begin (npc-inventory 'add (create-item-adt (car objects)))
              (iter (cdr objects)))))
        (let ((possesions (the-npc-table 'give-items name)))
          (iter possesions)))

      (define (fill-weapons)
        (define (iter objects)
          (if (null? objects)
              'done
            (begin (npc-inventory 'add (create-weapon-adt (car objects)))
              (iter (cdr objects)))))
        (let ((possesions (the-npc-table 'give-weapons name)))
          (iter possesions)))

      (define (has-item? an-item)
        (npc-inventory 'give an-item))

      (define (initiate-conversation a-player)

        ;;This is the most important part of the ADT
        ;;This algorithm will make a player converse with an NPC,
        ;;and while doing so, it will check for quests and update them
        ;;if necessary

        (let ((the-npc-quest (if (eq? npc-quest 'none)
                                 'none
                               (create-quest-adt npc-quest))))

          ;;the NPC converses with a player
          ;;The npc-conditions is a list of predicates,
          ;;contained in lambda exps, with a player as parameter,
          ;;eg ((lambda (a-player) (eq? aname (a-player 'give-name)) (lambda (...)))

          (define (check-conditions conditionlist)
            ;;checks whether a player has fulfilled a quest
            (cond ((null? conditionlist) #t)
              (((car conditionlist) a-player) ;;check the condition by applying it to the player
                (check-conditions (cdr conditionlist)))
              (else #f)))

          (define (execute-quest)
            ;;when the player fulfilled a quest, trigger the proper events
            (mark-quest-as-done the-npc-quest (get-character-questlog a-player))
            (newline)
            (the-npc-quest 'trigger a-player)
            (display-line npc-conversation)
            (set! npc-quest 'none))

          ;;There are several situations to consider, the following is a
          ;;tree walk of choices an NPC can make

          (if (eq? the-npc-quest 'none)
            ;;npc has no quest?
            (display-line npc-conversation)
            ;;yes, just talk

            (if (give-quest-from-character the-npc-quest a-player)
              ;;player knows about the quest?

              (if (check-conditions npc-conditions)
                ;;player fullfilled quest?
                (execute-quest)
                ;;yes, execute it
                (display-line npc-conversation))
              ;;no, just talk
              (begin (add-quest-for-character the-npc-quest a-player)
                ;;let the player know about the quest
                (display-line npc-conversation))))))
      ;;and just talk

      (define (show-inventory)
        (display-line "NPC Possesions: ")
        (npc-inventory 'show)
        (newline))

      (define (show-status)
        (display-line "Information about NPC: ")
        (display "Name: ")
        (display-line npc-name)
        (display-line npc-description)
        (display "Current Location: ")
        (display-line npc-location)
        (show-inventory))

      (define (dispatch m . args)
        (cond ((eq? m 'give-name) npc-name) ;;Returns symbol
          ((eq? m 'give-location) npc-location) ;;Returns Location ADT
          ((eq? m 'give-status) status) ;;Returns symbol
          ((eq? m 'give-description) npc-description) ;;Returns string
          ((eq? m 'add-item) (npc-inventory 'add (car args))) ;;Expects Item ADT
          ((eq? m 'drop-item) (npc-inventory 'drop (car args))) ;;Expects symbol
          ((eq? m 'has-item?) (has-item? (car args))) ;;Returns boolean
          ((eq? m 'show-status) (show-status))
          ((eq? m 'show-inventory) (show-inventory))
          ((eq? m 'set-name!) (set! npc-name (car args))) ;;Expects symbol
          ((eq? m 'set-location!) (set! npc-location (car args))) ;;Expects location ADT
          ((eq? m 'conversation) (initiate-conversation (car args))) ;;Returns string, expects player ADT
          ((eq? m 'set-conversation!) (set! npc-conversation (car args))) ;;Expects string
          ((eq? m 'set-status!) (set! status (car args))) ;;Expects symbol
          ((eq? m 'set-quest!) (set! npc-quest (car args))) ;;Expects Quest ADT or symbol none
          ((eq? m 'give-inventory) npc-inventory) ;;Returns Inventory ADT
          ((eq? m 'character-type) 'npc)
          (else (error "Create NPC ADT could not handle your request" m))))

      (if (eq? status 'new)
        (begin (fill-items)
          (fill-weapons)
          (set! status 'alive)
          dispatch)
        dispatch))))

;;The Player ADT

(define (create-player-adt name class)
  (if (not (the-class-table 'lookup class))
    (error "Class not found in Class table -- Create Player ADT" class)
    (let* ((player-name name)
            (player-class class)
            (class-parameters (the-class-table 'give-parameters class))
            (strength (car class-parameters))
            (constitution (cadr class-parameters))
            (wisdom (list-ref class-parameters 2))
            (intelligence (list-ref class-parameters 3))
            (dexterity (list-ref class-parameters 4))
            (charisma (list-ref class-parameters 5))
            (player-location (the-world 'startpoint?))
            (player-world the-world)
            (player-previous-worlds '())
            (player-previous-locations '())
            (player-hitpoints (the-class-table 'give-hitpoints class))
            (player-inventory (create-inventory-adt name))
            (player-questlog (create-questlog-adt name))
            (player-spellbook (create-spellbook-adt name))
            (player-hero-points 0)
            (current-offensive-weapon (car (the-class-table 'give-weapons class)))
            (current-defensive-weapon (cadr (the-class-table 'give-weapons class)))
            (status 'new))

      (define (fill-items)
        ;;the possesions is a list of NAMES of the player's possesions
        ;;the items are created here.
        (define (iter objects)
          (if (null? objects)
              'done
            (begin (player-inventory 'add (create-item-adt (car objects)))
              (iter (cdr objects)))))
        (let ((possesions (the-class-table 'give-items class)))
          (iter possesions)))

      (define (fill-weapons)
        (define (iter objects)
          (if (null? objects)
              'done
            (begin (player-inventory 'add (create-weapon-adt (car objects)))
              (iter (cdr objects)))))
        (let ((possesions (the-class-table 'give-weapons class)))
          (iter possesions)))

      (define (fill-spellbook)
        ;;the spells is a list of NAMES of the player's spells
        ;;the spells are created here.
        (define (iter spells)
          (if (null? spells)
              'done
            (begin (player-spellbook 'add (create-spell-adt (car spells)))
              (iter (cdr spells)))))
        (let ((possesions (the-class-table 'give-spells class)))
          (iter possesions)))

      (define (has-item? an-item)
        (player-inventory 'search an-item))

      (define (has-spell? a-spell)
        (player-spellbook 'search a-spell))

      (define (show-inventory)
        (display-line "Your Possesions: ")
        (player-inventory 'show)
        (newline))

      (define (show-spellbook)
        (display-line "Your Spells: ")
        (player-spellbook 'show)
        (newline))

      (define (show-questlog)
        (display "Current Quests: ")
        (newline)
        (player-questlog 'show)
        (newline)
        (let ((done-quests (player-questlog 'give-completed)))
          (if (not (null? done-quests))
            (begin (display "Done Quests: ")
              (display-list done-quests))))
        (newline))

      (define (show-status)
        (display "Information about player: ")
        (display-line player-name)
        (display "Class: ") (display-line player-class)
        (newline)
        (display-line "Personal Parameters:")
        (display "Hero Points: ") (display-line player-hero-points)
        (display "Current Hitpoints: ") (display-line player-hitpoints)
        (display "Strength: ") (display-line strength)
        (display "Constitution: ") (display-line constitution)
        (display "Wisdom: ") (display-line wisdom)
        (display "Intelligence: ") (display-line intelligence)
        (display "Dexterity: ") (display-line dexterity)
        (display "Charisma: ") (display-line charisma)
        (newline)
        (display "Current Location: ") (display-line (player-location 'give-name))
        (display "Currently equipped offensive weapon: ")
        (display-line (get-object-name current-offensive-weapon))
        (display "Currently equipped defensive weapon: ")
        (display-line (get-object-name current-defensive-weapon))
        (show-inventory)
        (show-spellbook)
        (show-questlog))

      (define (set-player-hitpoints! value)
        ;;checks whether a player isn't dead
        (set! player-hitpoints value)
        (if (< player-hitpoints 1)
          (begin (set! status 'dead)
            (display name) (display-line " is dead.")
            (core-eventhandler 'delete-all player-name)
            (drop-all player-name display-off)
            (delete-player-from-location player-name player-location)
            (the-current-players 'delete player-name)
            (the-character-table 'delete player-name))))

      (define (set-player-hero-points! value)
        ;;checks whether a player has won the game
        (set! player-hero-points value)
        (let ((heropoints-to-win (get-heropoints-to-win global-options)))
          (if (>= player-hero-points heropoints-to-win)
            (player-wins-game player-name)
              'done)))

      (define (dispatch m . args)
        (cond ((eq? m 'give-name) player-name) ;;Returns symbol
          ((eq? m 'give-class) player-class) ;;Returns symbol
          ((eq? m 'give-strength) strength) ;;Returns integer
          ((eq? m 'give-constitution) constitution) ;;Returns integer
          ((eq? m 'give-wisdom) wisdom) ;;Returns integer
          ((eq? m 'give-intelligence) intelligence) ;;Returns integer
          ((eq? m 'give-dexterity) dexterity) ;;Returns integer
          ((eq? m 'give-charisma) charisma) ;;Returns integer
          ((eq? m 'give-location) player-location) ;;Returns Location ADT
          ((eq? m 'give-world) car player-world) ;;Returns world ADT
          ((eq? m 'give-previous-worlds) player-previous-worlds) ;;Returns world ADTs
          ((eq? m 'give-previous-locations) player-previous-locations) ;;Returns location ADTs
          ((eq? m 'give-hitpoints) player-hitpoints) ;;Returns integer
          ((eq? m 'give-status) status) ;;Returns symbol
          ((eq? m 'give-heropoints) player-hero-points) ;;Returns integer
          ((eq? m 'give-offensive-weapon) current-offensive-weapon) ;;Returns Weapon ADT
          ((eq? m 'give-defensive-weapon) current-defensive-weapon) ;;Returns Weapon ADT
          ((eq? m 'add-item) (player-inventory 'add (car args))) ;;Expects Item/Weapon ADT
          ((eq? m 'drop-item) (player-inventory 'drop (car args))) ;;Expects symbol
          ((eq? m 'add-spell) (player-spellbook 'add (car args))) ;;Expects Spell ADT
          ((eq? m 'erase-spell) (player-spellbook 'erase (car args))) ;;Expects symbol
          ((eq? m 'use-item) (player-inventory 'use (car args))) ;;Expects Item/Weapon ADT
          ((eq? m 'cast-spell) (player-spellbook 'cast (car args))) ;;Expects Spell ADT
          ((eq? m 'add-quest) (player-questlog 'add (car args))) ;;Expects Quest ADT
          ((eq? m 'done-quest) (player-questlog 'mark-as-done (car args))) ;;Expects quest ADT
          ((eq? m 'has-item?) (has-item? (car args))) ;;Returns boolean, expects symbol
          ((eq? m 'has-spell?) (has-spell? (car args))) ;;Returns boolean, expects symbol
          ((eq? m 'show-status) (show-status))
          ((eq? m 'show-inventory) (show-inventory))
          ((eq? m 'show-spellbook) (show-spellbook))
          ((eq? m 'show-questlog) (show-questlog))
          ((eq? m 'set-name!) (set! player-name (car args))) ;;Expects symbol
          ((eq? m 'set-strength!) (set! strength (car args))) ;;Expects integer
          ((eq? m 'set-constitution!) (set! constitution (car args))) ;;Expects integer
          ((eq? m 'set-wisdom!) (set! wisdom (car args))) ;;Expects integer
          ((eq? m 'set-intelligence!) (set! intelligence (car args))) ;;Expects integer
          ((eq? m 'set-dexterity!) (set! dexterity (car args))) ;;Expects integer
          ((eq? m 'set-charisma!) (set! charisma (car args))) ;;Expects integer
          ((eq? m 'set-location!) (set! player-location (car args))) ;;Expects Location ADT
          ((eq? m 'set-world!) (set! player-world (car args))) ;;Expects world ADT
          ((eq? m 'set-previous-worlds!) (set! player-previous-worlds (car args))) ;;Expects world ADTs
          ((eq? m 'set-previous-locations!) (set! player-previous-locations (car args))) ;;Expects location ADTs
          ((eq? m 'set-hitpoints!) (set-player-hitpoints! (car args))) ;;Expects integer
          ((eq? m 'set-heropoints!) (set-player-hero-points! (car args))) ;;Expects integer
          ((eq? m 'set-offensive-weapon!) (set! current-offensive-weapon (car args))) ;;Expects Weapon ADT
          ((eq? m 'set-defensive-weapon!) (set! current-defensive-weapon (car args))) ;;Expects Weapon ADT
          ((eq? m 'give-inventory) player-inventory) ;;Returns Inventory ADT
          ((eq? m 'give-spellbook) player-spellbook) ;;Returns Spellbook ADT
          ((eq? m 'give-questlog) player-questlog) ;;Returns Questlog ADT
          ((eq? m 'set-status!) (set! status (car args))) ;;Expects symbol
          ((eq? m 'character-type) 'player) ;;To distinguish between players monsters and NPC's
          (else (error "Create Player ADT could not handle your request" m))))

      (if (eq? status 'new)
        ;;Player is initialised
        (begin (fill-items)
          (fill-weapons)
          (fill-spellbook)
          ;;Let the location know the player is in the room
          (let ((players-in-location (player-location 'give-players)))
            (player-location 'set-players! (cons name players-in-location)))
          (set! status 'alive)
          ;;Player has to be added to the current players list!
          (the-current-players 'add player-name)
          dispatch)
        dispatch))))

;;The Quest ADT

(define (create-quest-adt name)
  (if (not (the-quest-table 'lookup name))
    (error "The Quest was not found in the quest table -- Create Quest ADT" name)
    (let ((quest-name name)
           (title (the-quest-table 'give-title name))
           (status 'uncompleted)
           (description (the-quest-table 'give-description name))
           (trigger-events (the-quest-table 'give-trigger name))
           (quest-points (the-quest-table 'give-heropoints name)))

      (define (trigger a-player)
        (let ((the-player (get-character-adt a-player)))

          ;;The triggerlist contains names of action ADTs looked up in the action table
          ;;A quest is always triggered by a player (through an NPC)

          (define (loop-over-triggers triggerlist)
            (if (null? triggerlist)
                'done
              (begin ((car triggerlist) the-player)
                ;;each trigger is a lambda with one parameter
                (loop-over-triggers (cdr triggerlist)))))

          (loop-over-triggers trigger-events)
          (do-heropoints-up quest-points the-player)
          ;;The player's heropoints are automatically adjusted
          (display (get-character-name the-player))
          (display " has solved a quest and gains ")
          (display quest-points)
          (display-line " heropoints.")
          (set! status 'completed)
          (the-quest-table 'delete name)))

      (define (dispatch m . args)
        (cond ((eq? m 'give-name) quest-name) ;;Returns symbol
          ((eq? m 'give-title) title) ;;Returns string
          ((eq? m 'give-status) status) ;;Returns symbol
          ((eq? m 'give-description) description) ;;Returns string
          ((eq? m 'trigger) (trigger (car args)))
          ((eq? m 'give-heropoints) quest-points) ;;Returns integer
          ((eq? m 'set-status!) (set! status (car args))) ;;Expects symbol
          (else (error "Could not handle your request -- Create Quest ADT" m))))

      dispatch)))

;;The Questlog ADT

(define (create-questlog-adt posessor)
  ;;Questlog is based on an inventory tree
  ;;It stocks Quest ADTs
  (let ((quests (create-inventory-tree))
         (questlog-posessor posessor)
         (completed-quests '()))

    ;;Abstractions
    (define (the-name quest) (car quest))
    (define (the-quest quest) (cdr quest))
    (define (title quest) (quest 'title))
    (define (status quest) (quest 'status))
    (define (description quest) (quest 'description))
    (define (trigger quest) (quest 'trigger))
    (define (heropoints quest) (quest 'give-heropoints))

    (define (add quest)
      ;;Adds a quest to the player's questlog
      (if (symbol? quest)
        (error "Expected procedure not a symbol -- QUESTLOG" quest)
        (quests 'add quest)))

    (define (mark-as-done quest)
      (let ((quest-name (if (symbol? quest)
                          quest
                          (get-object-name quest))))
        ;;Deletes the quest and writes the quest to the log
        (begin (set! completed-quests (cons quest-name completed-quests))
          (quests 'delete quest-name))))

    (define (give key name)
      ;;Gives valid information about a quest
      (let ((the-quest (quests 'lookup name)))
        (if the-quest
          (cond ((eq? key 'title) (title the-quest))
            ((eq? key 'status) (status the-quest))
            ((eq? key 'description) (description the-quest))
            ((eq? key 'heropoints) (heropoints the-quest))
            (else (error "Wrong key -- Give -- Create Questbook ADT" key)))
          (error "Quest not found -- Give -- Create Questbook ADT" name))))

    (define (give-quest a-quest)
      (let ((quest-name (if (symbol? a-quest)
                          a-quest
                          (a-quest 'give-name))))
        (quests 'lookup quest-name)))

    (define (for-each-quest an-action)
      (quests 'for-each an-action))

    (define (show-quests)
      (quests 'show))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (add (car args))) ;;Expects a Quest ADT
        ((eq? m 'mark-as-done) (mark-as-done (car args))) ;;Expects a Quest ADT or name
        ((eq? m 'give) (give-quest (car args))) ;;Expects a quest ADT or a quest name
        ((eq? m 'give-title) (give 'title (car args))) ;;Returns string, expects symbol
        ((eq? m 'give-status) (give 'status (car args))) ;;Returns symbol, expects symbol
        ((eq? m 'give-description) (give 'description (car args))) ;;Returns string, expects symbol
        ((eq? m 'give-heropoints) (give 'heropoints (car args))) ;;Returns integer, expects symbol
        ((eq? m 'give-completed) completed-quests) ;;Returns list
        ((eq? m 'for-each) (for-each-quest (car args))) ;;Expects procedure
        ((eq? m 'show) (show-quests))
        (else (error "Could not handle your request -- Create Questlog ADT" m))))

    dispatch))

;;The Spell ADT

(define (create-spell-adt name)
  ;;Looks up its arguments in the spell table
  (if (not (the-spells-table 'give-name name))
    (error "Spell not found in the Spell Database -- Create Spell ADT" name)
    (let* ((spell-name (the-spells-table 'give-name name))
            (spell-description (the-spells-table 'give-spell-description name))
            (cast-description (the-spells-table 'give-cast-description name))
            (spell-action-list (the-spells-table 'give-actions name))
            (spell-possesor 'nobody)
            (spells-cast-max (the-spells-table 'give-max-castings name))
            (spell-reset-time (the-spells-table 'give-reset-time name))
            (spells-cast-so-far 0))

      ;;A word about the structure:
      ;;The actions of a spell are stored in a list, each element of the list,
      ;;is the name of an action, and some extra parameters, so
      ;;actionlist => '((action1name . action1parameters) (action2name . action2paramaters) etc)

      (define (cast)

        ;;The most important part of this ADT, the Cast Algorithm,
        ;;will trigger all events this spell has, and take care of all
        ;;side-effects the spell introduces

        (let ((spell-possesor-adt (the-character-table 'lookup spell-possesor)))
          (if (= spells-cast-so-far spells-cast-max)
            ;;Spell casts exceeded?

            (begin (display "Cannot cast spell ")
              (display spell-name)
              (display-line ": maximum casts a day exceeded")
              (display "You can cast this spell again in ")
              (display spell-reset-time)
              (display-line " turns")
              (let ((spell-reset (create-action-adt 'reset-spell spell-possesor (list name)))
                     (eventhandler-time (core-eventhandler 'give-runtime)))

                ;;spell-reset is an action ADT, ready to be passed to
                ;;The eventhandler, as its priority, it receives the reset time
                ;;because, once the eventhandler increases its time with the reset time,
                ;;the spell can be cast again
                ;;Spell-reset is created using an action 'reset-spell,
                ;;already in the action table, which will reset the spells-cast-so-far to 0,
                ;;enabling the player to cast the spells again

                (core-eventhandler 'add spell-reset (+ spell-reset-time eventhandler-time))))

            (begin (set! spells-cast-so-far (+ 1 spells-cast-so-far))

              ;;We start a loop through our actionlist,
              ;;We select the first action, and create our action ADT as usual
              ;;(remember what the structure of our actionlist is like),
              ;;We then pass it to the eventhandler, but ask it to hold it,
              ;;When we queued all the actions, we let the eventhandler execute them all

              (letrec ((run-through-actions
                         (lambda (actionlist)
                           (if (null? actionlist)
                               'done
                             (let* ((first-action (car actionlist))
                                     (first-action-name (car first-action))
                                     (first-action-pars (cdr first-action))
                                     (the-action (create-action-adt first-action-name
                                                   spell-possesor
                                                   (if (null? first-action-pars)
                                                       '()
                                                     (list first-action-pars)))))
                               (core-eventhandler 'add the-action 'hold)
                               (run-through-actions (cdr actionlist))))))
                        (display-action (create-action-adt 'display-spell
                                          spell-possesor
                                          (list spell-name))))

                ;;display action will display the actual casting on the screen,
                ;;it passes an action ADT to the eventhandler
                ;;This adt (display-spell) expects 2 arguments:
                ;;the spell name, and its description

                (core-eventhandler 'add display-action 'hold)
                (run-through-actions spell-action-list))

              ;;Execute the actions if playstyle is automatic
              (execute-actions)))))

      (define (dispatch m . args)
        (cond ((eq? m 'give-name) spell-name)
          ((eq? m 'give-spell-description) spell-description)
          ((eq? m 'give-cast-description) cast-description)
          ((eq? m 'give-actions) spell-action-list)
          ((eq? m 'give-possesor) spell-possesor)
          ((eq? m 'give-spells-cast-max) spells-cast-max)
          ((eq? m 'give-spell-reset-time) spell-reset-time)
          ((eq? m 'give-spells-cast-so-far) spells-cast-so-far)
          ((eq? m 'set-spell-description!) (set! spell-description (car args)))
          ((eq? m 'set-cast-description!) (set! cast-description (car args)))
          ((eq? m 'set-actions!) (set! spell-action-list (car args)))
          ((eq? m 'set-possesor!) (set! spell-possesor (car args)))
          ((eq? m 'set-spells-cast-max!) (set! spells-cast-max (car args)))
          ((eq? m 'set-spell-reset-time!) (set! spell-reset-time (car args)))
          ((eq? m 'set-spells-cast-so-far!) (set! spells-cast-so-far (car args)))
          ((eq? m 'priest-spell?) (eq? spell-type 'priest))
          ((eq? m 'wizard-spell?) (eq? spell-type 'wizard))
          ((eq? m 'get-type) 'spell) ;;to separate spells from items and weapons
          ((eq? m 'cast) (cast))
          (else (error "Could not handle your request -- Create Spell ADT" m))))

      dispatch)))

;;The Spellbook ADT

(define (create-spellbook-adt possesor)

  ;;Based on an inventory tree adt
  ;;A spellbook stores the spells of a wizard or a priest
  ;;it expects a spell ADT as an input if you want to stock it, not a name

  (let ((the-spellbook (create-inventory-tree))
         (spellbook-possesor possesor))

    (define (add spell)
      (if (symbol? spell)
        (error "expects ADT not a symbol -- ADD -- SPELLBOOK" spell)
        ;;Expects an ADT as an input
        (begin (spell 'set-possesor! spellbook-possesor)
          (the-spellbook 'add spell))))

    (define (give spell)
      (if (symbol? spell)
        (the-spellbook 'lookup spell)
        (error "expects symbol not an ADT -- GIVE -- SPELLBOOK" spell)))

    (define (erase spell)
      (let ((the-spell (if (symbol? spell)
                         (give spell)
                         spell)))
        (if the-spell
          (begin (the-spell 'set-possesor! 'nobody)
            (the-spellbook 'delete (get-object-name the-spell)))
          (display-line "You haven't got that Spell"))))

    (define (cast spell)
      (if (symbol? spell)
        ;;The spell is cast, if it has enough free casts
        (let ((the-spell (give spell)))
          (if the-spell
            (the-spell 'cast)
            (begin (display "The Spell ")
              (display spell)
              (display-line " was not found in your spellbook"))))
        (error "expects symbol not an ADT -- CAST -- SPELLBOOK")))

    (define (for-each an-action)
      (the-spellbook 'for-each an-action))

    (define (show)
      (the-spellbook 'show))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (add (car args))) ;;Expects Spell ADT
        ((eq? m 'erase) (erase (car args))) ;;Expects Spell ADT or symbol
        ((eq? m 'give) (give (car args))) ;;Expects symbol
        ((eq? m 'cast) (cast (car args))) ;;Expects symbol
        ((eq? m 'for-each) (for-each (car args))) ;;Expects procedure
        ((eq? m 'show) (show))
        (else (error "Could not handle your request -- Create Spellbook ADT" m))))

    dispatch))

;;The World ADT

(define (create-world-adt name startpoint) ;; <<== Changed.

  ;;The world is represented by a directed weighted graph.
  ;;The weights of the edges are directions (north east south west)
  ;;The graph is implemented with edge lists
  ;;The startpoint indicates where a player begins his journey in the world,
  ;;It must be a Location ADT

  (let ((the-world-graph (create-graph eq? #t #t))
         (world-name name)
         (start startpoint))

    ;;The information will be tagged:
    ;;Via a conscell: car = location or world - cdr = the actual object
    (define (create-info tag object) (cons tag object))
    (define (tag info) (car info))
    (define (object info) (cdr info))

    (define (insert-world a-world)
      ;;Graph expects a label and info
      ;;The world's name will be the label,
      ;;the info will be the ADT world itself
      (the-world-graph 'insert-node
        (a-world 'give-name)
        (create-info 'world a-world)))

    (define (insert-location a-location)
      ;;Graph expects a label and info
      ;;The location's name will be the label,
      ;;the info will be the ADT world itself
      (the-world-graph 'insert-node
        (a-location 'give-name)
        (create-info 'location a-location)))

    (define (insert-road location1 location2 direction . reversed?)

      ;;The direction must be north east south or west
      ;;If reversed is true; the world will create the reversed road automatically

      (define (reverse dir)
        (cond ((eq? dir 'east) 'west)
          ((eq? dir 'west) 'east)
          ((eq? dir 'north) 'south)
          ((eq? dir 'south) 'north)
          (else (error "Wrong direction -- Create World - Rerverse" dir))))

      (the-world-graph 'insert-edge
        (location1 'give-name)
        (location2 'give-name)
        direction)
      (if (not (null? reversed?))
        (the-world-graph 'insert-edge
          (location2 'give-name)
          (location1 'give-name)
          (reverse direction))))

    (define (delete-location a-location)
      ;;Graph expects a label
      ;;This function also deletes worlds
      (the-world-graph 'delete-node (a-location 'give-name)))

    (define (delete-road location1 location2)
      ;;Graph expects 2 labels
      (the-world-graph 'delete-edge (location1 'give-name) (location2 'give-name)))

    (define (check-for a-tag a-location)
      ;;Graph expects a label, asks the information of a
      ;;location/world and checks its tag
      (let ((result (the-world-graph 'lookup-node-info (a-location 'give-name))))
        (if result
          (eq? a-tag (tag result))
          #f)))

    (define (give-location a-name)
      ;;Will check the world for a location with the given name
      ;;and returns that object
      (object (the-world-graph 'lookup-node-info a-name)))

    (define (give-neighbour a-location a-direction)
      (let ((the-neighbour 'unknown))

        ;;graph expects a label and a action
        ;;a-action takes 5 arguments:
        ;;from-label from-info to-label to-info edge-info

        (the-world-graph 'foreach-neighbour (a-location 'give-name)
          (lambda (fromlbl frominfo tolbl toinfo edgeinfo)

            ;;If the direction equals the direction of the edge,
            ;;give the location ADT in that direction

            (if (eq? a-direction edgeinfo)
              (set! the-neighbour toinfo))))
        ;;The foreach will return #t so we will have to
        ;;assign our neighbour manually

        (if (eq? the-neighbour 'unknown)
            'not-found
          (object the-neighbour))))

    (define (for-each-location do-something)
      ;;Graph expects a function with two arguments: the label and the info
      (the-world-graph 'foreach-node
        (lambda (node-label node-info)
          (do-something (object node-info)))))

    (define (map-over-locations do-something)
      ;;Graph expects a function with two arguments: the label and the info
      (the-world-graph 'map-over-nodes
        (lambda (node-label node-info)
          (do-something (object node-info)))))

    (define (for-each-neighbour location do-something)
      ;;Expects procedure with 5 arguments: fromlbl frominfo tolbl toinfo edgeinfo
      (the-world-graph 'foreach-neighbour (location 'give-name)
        do-something))

    (define (give-road fromlocation tolocation)
      (the-world-graph 'lookup-edge (fromlocation 'give-name) (tolocation 'give-name)))

    (define (show)
      (for-each-location (lambda (location1)
                           (for-each-location (lambda (location2)
                                                (if (give-road location1 location2)
                                                  (begin (display "Road From ")
                                                    (display (location1 'give-name))
                                                    (display " going ")
                                                    (display (give-road location1 location2))
                                                    (display " to ")
                                                    (display-line (location2 'give-name)))))))))

    (define (dispatch m . args)
      (cond ((eq? m 'give-name) world-name)
        ((eq? m 'insert-world) (insert-world (car args))) ;;Expects World ADT
        ((eq? m 'insert-location) (insert-location (car args))) ;;Expects Insert ADT
        ((eq? m 'insert-road) (apply insert-road args)) ;;Expects 2 location ADTs, a symbol and option
        ((eq? m 'delete-location) (delete-location (car args))) ;;Expects symbol
        ((eq? m 'delete-road) (delete-road (car args) (cadr args))) ;;Expects two symbols
        ((eq? m 'startpoint?) start) ;;Returns Location ADT
        ((eq? m 'set-startlocation!) (set! start (car args))) ;;Expects location ADT
        ((eq? m 'isworld?) (check-for 'world (car args))) ;;Expects ADT
        ((eq? m 'islocation?) (check-for 'location (car args))) ;;Expects ADT
        ((eq? m 'give-location) (give-location (car args))) ;;Returns Location/World ADT
        ((eq? m 'give-neighbour) (give-neighbour (car args) (cadr args))) ;;expects direction symbol
        ((eq? m 'for-each-location) (for-each-location (car args))) ;;Expects procedure with 2 arguments
        ((eq? m 'map-over-locations) (map-over-locations (car args))) ;;Expects procedure with 2 arguments
        ((eq? m 'for-each-neighbour) (for-each-neighbour (car args) (cadr args)))
        ((eq? m 'give-road) (give-road (car args) (cadr args))) ;;Expects 2 Location ADTs
        ((eq? m 'show) (show))
        (else (error "Could not handle your request -- Create World ADT" m))))

    dispatch))

;;The Current Players ADT

(define (current-players-adt)
  ;;A list of all active players
  (let ((player-list '()))

    (define (add player-name)
      ;;Accepts the name of a player
      (set! player-list (cons player-name player-list))
      #t)

    (define (delete player-name)
      (define (delete-list name alist)
        (cond ((null? alist) '())
          ((eq? (car alist) name) (cdr alist))
          (else (cons (car alist)
                  (delete-list name (cdr alist))))))
      (set! player-list (delete-list player-name player-list))
      (if (null? player-list)
        (begin (display-line "No more Players Alive")
          (set! game-over? #t))
        #t))

    (define (lookup player-name)
      ;;Checks whether the player is an active player, when found
      ;;it searches the ADT in the character table.
      (define (search pname plist)
        (cond ((null? plist) #f)
          ((eq? pname (car plist)) (get-character-adt (car plist)))
          (else (search pname (cdr plist)))))
      (search player-name player-list))

    (define (show)
      (display-line "Players in the game: ")
      (for-each (lambda (player)
                  (display-line player))
        player-list))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (add (car args))) ;;Expects symbol
        ((eq? m 'delete) (delete (car args))) ;;Expects symbol
        ((eq? m 'lookup) (lookup (car args))) ;;Expects symbol
        ((eq? m 'give-player-list) player-list)
        ((eq? m 'show) (show))
        (else (error "Could not handle your request -- Current Players" m))))

    dispatch))

;;all user options are stored here

(define (the-options)
  (let ((difficulty 'normal)
         (heropoints-to-win 500)
         (playstyle 'continuous)
         (nr-of-actions-allowed 4)
         (action-execution 'automatic)
         (action-hold-offset 10)
         (iconmode 'on))

    (define (change-difficulty value)
      ;;standard difficulty is normal
      ;;determines monster power and AI
      ;;choose between easy normal and hard
      (if (member value '(easy normal hard))
        (begin (set! difficulty value)
            'ok)
          'failed))

    (define (change-heropoints-to-win value)
      ;;standard is 500
      ;;if a player reaches this heropoint total, he/she wins the game
      ;;choose any integer value you like
      (if (number? value)
        (begin (set! heropoints-to-win value)
            'ok)
          'failed))

    (define (change-playstyle value turns)
      ;;standard is continuous
      ;;continuous playstyle does not require turns
      ;;turnbased playstyle requires the players to
      ;;play in a certain order, an integer must be given to
      ;;specify how many actions one may undertake in a turn
      ;;choose between continuous and turnbased
      (cond ((eq? value 'continuous) (set! playstyle 'continuous) 'ok)
        ((eq? value 'turnbased)
          (if (null? turns)
              'failed
            (begin (set! playstyle 'turnbased)
              (set! nr-of-actions-allowed (car turns))
              (the-turn-status 'reset-turncounter)
              ;;to reset the turns for all players
              ;;to the new number of turns
                'ok)))
        (else 'failed)))

    (define (change-action-execution value)
      ;;standard is automatic
      ;;if automatic, then the eventhandler will automatically execute actions
      ;;if manual, then the players can themselves execute the actions at
      ;;the times they want
      ;;choose between automatic and manual
      (if (member value '(automatic manual))
        (begin (set! action-execution value)
            'ok)
          'failed))

    (define (change-action-hold-offset value)
      ;;standard is 10
      ;;it is an offset for holding actions
      ;;eg, if a player types 'hold 5 <action>',
      ;;the eventhandler will add 10 (the offset)
      ;;to the 5, resulting in an enqueue with priority 15
      (if (number? value)
        (begin (set! action-hold-offset value)
            'ok)
          'failed))

    (define (change-iconmode value)
      ;;standard is on
      ;;display action icons or not
      (if (or (eq? value 'on)
            (eq? value 'off))
        (begin (set! iconmode value)
            'ok)
          'failed))

    (define (show)
      (newline)
      (display-line "*** GLOBAL OPTIONS ***")
      (display "Difficulty: ") (display difficulty)
      (display-line " (Standard is Normal - Easy/Normal/Hard)")
      (display "Heropoints needed to win: ") (display heropoints-to-win)
      (display-line " (Standard is 500 - Choose any number)")
      (display "Action Execution: ") (display action-execution)
      (display-line " (Standard is Automatic - Automatic/Manual)")
      (display "Playstyle: ") (display playstyle) (display " Turns: ") ; <<== Changed.
      (display nr-of-actions-allowed)
      (display-line " (Standard is Continuous - Continuous/Turnbased (enter list with number of turns))")
      (display "Action Offset: ") (display action-hold-offset)
      (display-line " (Standard is 10 - Choose any number)")
      (display "Iconmode: ") (display iconmode)
      (display-line " (Standard is on - On / Off)")
      (newline))

    (define (dispatch m . args)
      (cond ((eq? m 'difficulty) difficulty)
        ((eq? m 'heropoints-to-win) heropoints-to-win)
        ((eq? m 'playstyle) playstyle)
        ((eq? m 'action-execution) action-execution)
        ((eq? m 'action-hold-offset) action-hold-offset)
        ((eq? m 'iconmode) iconmode)
        ((eq? m 'set-difficulty) (change-difficulty (car args)))
        ((eq? m 'set-heropoints-to-win) (change-heropoints-to-win (car args)))
        ((eq? m 'set-playstyle) (change-playstyle (car args) (cdr args)))
        ((eq? m 'nr-of-actions-allowed) nr-of-actions-allowed)
        ((eq? m 'set-action-execution) (change-action-execution (car args)))
        ((eq? m 'set-action-hold-offset) (change-action-hold-offset (car args)))
        ((eq? m 'set-iconmode) (change-iconmode (car args)))
        ((eq? m 'show) (show))
        (else (error "Could not handle your request -- Options" m))))

    dispatch))

(define (action-table)
  ;;The table consists of a hashtable containing all the info of the actions
  (let ((actions (create-table-adt 73 eq?)))

    ;;Abstractions
    (define (create-package name function) (cons name function))
    (define (the-name data) (car data))
    (define (the-function data) (cdr data))
    ;;Done

    (define (add a-name a-function)
      ;;The key is the name of the action, it will be unique
      ;;Other parameters are:
      ;;The function which will be called to execute the given action
      (actions 'add a-name (create-package a-name a-function)))

    (define (delete a-name)
      (actions 'delete a-name))

    (define (lookup a-name)
      (actions 'lookup a-name))

    (define (show)
      (actions 'show))

    (define (give key a-name)
      (cond ((eq? key 'name) (the-name (lookup a-name)))
        ((eq? key 'function) (the-function (lookup a-name)))
        (else (error "wrong key type -- Give -- Action table ADT" key))))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (add (car args) (cadr args)))
        ((eq? m 'delete) (delete (car args)))
        ((eq? m 'lookup) (lookup (car args)))
        ((eq? m 'show) (show))
        ((eq? m 'give-name) (give 'name (car args)))
        ((eq? m 'give-function) (give 'function (car args)))
        ((eq? m 'give-loadfactor) (actions 'give-loadfactor))
        (else (error "Actions table could not handle your request" m))))

    dispatch))

(define (character-table)
  ;;Keeps track of all characters in the game:
  ;;Monsters, NPCs and players
  ;;Based on a Table, more specific on a Hash Table with BST ADT
  (let ((the-characters (create-bst-table-adt 149 eq?)))

    (define (add-binding an-adt)
      ;;Expects ADT
      (the-characters 'add (an-adt 'give-name) an-adt))

    (define (delete-binding a-character-name)
      ;;Expects symbol
      (the-characters 'delete a-character-name))

    (define (lookup-adt name-of-adt)
      ;;Expects symbol
      (the-characters 'lookup name-of-adt))

    (define (show-bindings)
      (the-characters 'show))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (add-binding (car args)))
        ((eq? m 'delete) (delete-binding (car args)))
        ((eq? m 'lookup) (lookup-adt (car args)))
        ((eq? m 'give-loadfactor) (the-characters 'give-loadfactor))
        ((eq? m 'show) (show-bindings))
        (else (error "Could not handle your request -- Create Characters Table " m))))

    dispatch))

(define (class-table)

  ;;Based on a Table ADT
  ;;The class table stores player classes (fighter, priest, wizard, rogue)
  ;;But also monster classes (zombie, dragon, elf, orc,...)
  ;;Also special monsters (like bosses) are stored here
  ;;The parameters of a class are variable, so parameters are randomly assigned
  ;;through the helpfunction random-range
  ;;The first item in the inventory MUST be an offensive weapon! The second must be a defensive weapon.

  (define (random-range lower upper)
    ;;Creates a random number between a lower and an upper bound
    (+ (quotient (* (random 100) (+ (- upper lower) 1)) 100) lower))

  ;;The class table stores for players:
  ;;their parameters, possesions (items weapons and spells) and HP
  ;;for monsters their possesions, HP parameters and an additional description
  ;;IMPORTANT: the given parameters are COUPLES of the form '(lower . upper)
  ;;The class table will store the lower and upper boundaries and when asked for
  ;;a specific class, the class table will return a random value between the boundaries
  ;;This way, all monsters and players will be unique

  ;;Parameterlist = (strength constitution wisdom intelligence dexterity charisma)
  ;;each parameter is a couple (lower . upper)

  (let ((characters (create-table-adt 30 eq?)))
    ;;Abstractions
    (define (create-package class
              parameterlist
              hitpoints
              itemlist
              weaponlist
              spelllist . respawn)
      (list class parameterlist hitpoints itemlist weaponlist spelllist respawn))
    (define (the-class data) (list-ref data 0))
    (define (the-parameterlist data) (list-ref data 1))
    (define (the-hitpoints data) (list-ref data 2))
    (define (the-itemlist data) (list-ref data 3))
    (define (the-weaponlist data) (list-ref data 4))
    (define (the-spelllist data) (list-ref data 5))
    (define (the-respawn-time data) (if (equal? (list-ref data 6) (list '()))
                                        'none
                                      (caar (list-ref data 6))))
    (define (lowerb couple) (car couple))
    (define (upperb couple) (cdr couple))
    ;;Done

    (define (add class parameterlist hitpoints itemlist weaponlist spelllist . respawn)
      (characters 'add class
        (create-package class
          parameterlist
          hitpoints
          itemlist
          weaponlist
          spelllist
          respawn)))

    (define (delete class-name)
      (characters 'delete class-name))

    (define (lookup class-name)
      (characters 'lookup class-name))

    (define (give-random-value a-couple)
      (random-range (lowerb a-couple) (upperb a-couple)))

    (define (give-random-values list-of-couples)
      (if (null? list-of-couples)
          '()
        (cons (give-random-value (car list-of-couples))
          (give-random-values (cdr list-of-couples)))))

    (define (give key name)
      (let ((found-class (lookup name)))
        (if found-class
          (cond ((eq? key 'class) (the-class found-class))
            ((eq? key 'parameters) (give-random-values (the-parameterlist found-class)))
            ((eq? key 'hitpoints) (give-random-value (the-hitpoints found-class)))
            ((eq? key 'weapons) (the-weaponlist found-class))
            ((eq? key 'items) (the-itemlist found-class))
            ((eq? key 'spells) (the-spelllist found-class))
            ((eq? key 'respawn) (the-respawn-time found-class))
            (else (error "Wrong key -- Give -- Class Table" key)))
          (error "Class not found in class Table" name))))

    (define (show)
      (characters 'show))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (apply add args))
        ((eq? m 'delete) (delete (car args)))
        ((eq? m 'lookup) (lookup (car args)))
        ((eq? m 'give-class) (give 'class (car args)))
        ((eq? m 'give-parameters) (give 'parameters (car args)))
        ((eq? m 'give-hitpoints) (give 'hitpoints (car args)))
        ((eq? m 'give-weapons) (give 'weapons (car args)))
        ((eq? m 'give-items) (give 'items (car args)))
        ((eq? m 'give-spells) (give 'spells (car args)))
        ((eq? m 'give-respawn) (give 'respawn (car args)))
        ((eq? m 'give-loadfactor) (characters 'give-loadfactor))
        ((eq? m 'show) (show))
        (else (error "Could not handle your request -- Class Table" m))))

    dispatch))

(define (items-table)
  ;;The table consists of a hashtable containing a binding
  ;;between the name of an item and the action that the item can perform,
  ;;it also contains a description of an item
  ;;Extras are parameters for the actions of the item, if none please enter '()
  (let ((items (create-table-adt 41 eq?)))

    ;;Abstractions
    (define (create-package name description function-list extra) (list name description function-list extra))
    (define (the-name data) (car data))
    (define (the-description data) (cadr data))
    (define (the-functions data) (caddr data))
    (define (extras data) (cadddr data))
    ;;Done

    (define (add name description function-list extra)
      ;;Adds an item (that is: its name, its description and the actions it can perform
      ;;That actions is just a list of names of actions that can be looked up in the action table
      (items 'add name (create-package name description function-list extra)))

    (define (delete name)
      (items 'delete name))

    (define (give key name)
      ;;Gives relevant information about the item
      (let ((the-item (items 'lookup name)))
        (if the-item
          (cond ((eq? key 'name) (the-name the-item))
            ((eq? key 'description) (the-description the-item))
            ((eq? key 'functions) (the-functions the-item))
            ((eq? key 'extra) (extras the-item))
            (else (error "Wrong key -- Give -- Items Table" key)))
          (error "Item was not found in the Items Table -- Items Table" name))))

    (define (show)
      (items 'show))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (add (car args) (cadr args) (caddr args) (cadddr args)))
        ((eq? m 'delete) (delete (car args)))
        ((eq? m 'give-name) (give 'name (car args)))
        ((eq? m 'give-description) (give 'description (car args)))
        ((eq? m 'give-functions) (give 'functions (car args)))
        ((eq? m 'give-extra) (give 'extra (car args)))
        ((eq? m 'give-loadfactor) (items 'give-loadfactor))
        ((eq? m 'show) (show))
        (else (error "Could not handle your request -- Items Table" m))))

    dispatch))

(define (location-table)
  ;;The table consists of a hashtable containing all the info of the locations
  (let ((locations (create-table-adt 100 eq?)))

    ;;Abstractions
    (define (create-package name
              monsterlst
              npclst
              description
              alterdescription
              itemlst
              actions-on-enter-lst)
      (list name
        monsterlst
        npclst
        description
        alterdescription
        itemlst
        actions-on-enter-lst))

    (define (the-name data) (list-ref data 0))
    (define (monsterlst data) (list-ref data 1))
    (define (npclst data) (list-ref data 2))
    (define (description data) (list-ref data 3))
    (define (alterdescription data) (list-ref data 4))
    (define (itemlst data) (list-ref data 5))
    (define (actionlst data) (list-ref data 6))
    ;;Done

    (define (add name monsterlst npclst description alterdescription itemlst actions-lst)
      ;;The key is the name of the location, it will be unique
      ;;Other parameters are:
      ;;Monsterlist: list of monsters in the room
      ;;NPClist: list of npcs in the room
      ;;Description: the description of the room
      ;;alterdescription: an alternative description
      ;;Items: the items and weapons to be found in the room
      ;;Actions: the actions performed when a player enters the room
      (locations 'add name (create-package name
                             monsterlst
                             npclst
                             description
                             alterdescription
                             itemlst
                             actions-lst)))

    (define (delete name)
      (locations 'delete name))

    (define (lookup name)
      (locations 'lookup name))

    (define (show)
      (locations 'show))

    (define (give key name)
      (cond ((eq? key 'name) (the-name (lookup name)))
        ((eq? key 'monster) (monsterlst (lookup name)))
        ((eq? key 'npc) (npclst (lookup name)))
        ((eq? key 'description) (description (lookup name)))
        ((eq? key 'alterdescription) (alterdescription (lookup name)))
        ((eq? key 'item) (itemlst (lookup name)))
        ((eq? key 'action) (actionlst (lookup name)))
        (else (error "wrong key type -- Give -- Locations ADT" key))))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (apply add args))
        ((eq? m 'delete) (delete (car args)))
        ((eq? m 'lookup) (lookup (car args)))
        ((eq? m 'show) (show))
        ((eq? m 'give-name) (give 'name (car args)))
        ((eq? m 'give-monster) (give 'monster (car args)))
        ((eq? m 'give-npc) (give 'npc (car args)))
        ((eq? m 'give-description) (give 'description (car args)))
        ((eq? m 'give-alterdescription) (give 'alterdescription (car args)))
        ((eq? m 'give-item) (give 'item (car args)))
        ((eq? m 'give-actions) (give 'action (car args)))
        ((eq? m 'give-loadfactor) (locations 'give-loadfactor))
        (else (error "Locations Table could not handle your request" m))))

    dispatch))

(define (npc-table)

  ;;Based on a Table adt
  ;;Name: the NPC name
  ;;Description: a brief description of the NPC
  ;;Startingpoint: where the NPC is found
  ;;Conversation: the text the NPC displays when you talk to him/her
  ;;Possesionlist: NPC possesions
  ;;Quest-Conditions: a list of conditions who need to be satisfied to trigger a quest
  ;;Quest-to-trigger: the name of the quest to trigger when the quest conditions are satisfied,
  ;;Quest conditions are checked when a player converses with an NPC

  (let ((npcs (create-table-adt 100 eq?)))

    ;;Abstractions
    (define (create-package name
              description
              startingpoint
              conversation
              itemlist
              weaponlist
              quest-conditions
              quest-to-trigger)
      (list name
        description
        startingpoint
        conversation
        itemlist
        weaponlist
        quest-conditions
        quest-to-trigger))

    (define (the-name data) (list-ref data 0))
    (define (the-description data) (list-ref data 1))
    (define (the-startingpoint data) (list-ref data 2))
    (define (the-conversation data) (list-ref data 3))
    (define (the-items data) (list-ref data 4))
    (define (the-weapons data) (list-ref data 5))
    (define (the-q-conditions data) (list-ref data 6))
    (define (the-q-to-trigger data) (list-ref data 7))
    ;;Done

    (define (add name description
              startingpoint
              conversation
              itemlist
              weaponlist
              quest-conditions
              quest-to-trigger)
      (npcs 'add name (create-package name
                        description
                        startingpoint
                        conversation
                        itemlist
                        weaponlist
                        quest-conditions
                        quest-to-trigger)))

    (define (delete name)
      (npcs 'delete name))

    (define (give key name)
      (let ((the-npc (npcs 'lookup name)))
        (if the-npc
          (cond ((eq? key 'name) (the-name the-npc))
            ((eq? key 'conversation) (the-conversation the-npc))
            ((eq? key 'items) (the-items the-npc))
            ((eq? key 'weapons) (the-weapons the-npc))
            ((eq? key 'startingpoint) (the-startingpoint the-npc))
            ((eq? key 'description) (the-description the-npc))
            ((eq? key 'q-conditions) (the-q-conditions the-npc))
            ((eq? key 'q-to-trigger) (the-q-to-trigger the-npc))
            (else (error "Wrong Key -- Give -- NPC Table" key)))
          (error "NPC not found in table -- Give -- NPC Table" name))))

    (define (show)
      (npcs 'show))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (apply add args))
        ((eq? m 'delete) (delete (car args)))
        ((eq? m 'give-name) (give 'name (car args)))
        ((eq? m 'give-conversation) (give 'conversation (car args)))
        ((eq? m 'give-items) (give 'items (car args)))
        ((eq? m 'give-weapons) (give 'weapons (car args)))
        ((eq? m 'give-startingpoint) (give 'startingpoint (car args)))
        ((eq? m 'give-description) (give 'description (car args)))
        ((eq? m 'give-q-conditions) (give 'q-conditions (car args)))
        ((eq? m 'give-quest) (give 'q-to-trigger (car args)))
        ((eq? m 'give-loadfactor) (npcs 'give-loadfactor))
        ((eq? m 'show) (show))
        (else (error "Could not handle your request -- NPC Table" m))))

    dispatch))

(define (quest-table)
  (let ((the-quests (create-table-adt 30 eq?)))
    ;;Abstractions
    ;;Quests are a list of (name title description (triggerlist) heropoints)
    (define (create-package name title description triggers heropoints)
      (list name title description triggers heropoints))
    (define (the-name quest) (list-ref quest 0))
    (define (the-title quest) (list-ref quest 1))
    (define (the-description quest) (list-ref quest 2))
    (define (triggerlist quest) (list-ref quest 3))
    (define (heropoints quest) (list-ref quest 4))

    (define (add questname
              questtitle
              questdescription
              questtriggerlist
              herop)
      (the-quests 'add questname (create-package questname
                                   questtitle
                                   questdescription
                                   questtriggerlist
                                   herop)))
    (define (delete name)
      (the-quests 'delete name))
    (define (lookup name)
      (the-quests 'lookup name))
    (define (show)
      (the-quests 'show))

    (define (give key name)
      (let ((the-quest (the-quests 'lookup name)))
        (if the-quest
          (cond ((eq? key 'name) (the-name the-quest))
            ((eq? key 'title) (the-title the-quest))
            ((eq? key 'description) (the-description the-quest))
            ((eq? key 'triggers) (triggerlist the-quest))
            ((eq? key 'heropoints) (heropoints the-quest))
            (else (error "Wrong key -- Give -- Quest Table" key)))
          (error "Could not find Quest in Quest Table -- Quest Table" name))))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (apply add args))
        ((eq? m 'delete) (delete (car args)))
        ((eq? m 'lookup) (lookup (car args)))
        ((eq? m 'show) (show))
        ((eq? m 'give-name) (give 'name (car args)))
        ((eq? m 'give-title) (give 'title (car args)))
        ((eq? m 'give-description) (give 'description (car args)))
        ((eq? m 'give-trigger) (give 'triggers (car args)))
        ((eq? m 'give-heropoints) (give 'heropoints (car args)))
        ((eq? m 'give-loadfactor) (the-quests 'give-loadfactor))
        (else (error "Could not handle your request -- Quest Table" m))))

    dispatch))

(define (spell-table)

  ;;Based on Table ADT
  ;;A spell can only be used by wizards and priests
  ;;It consists of a name, a description for the caster (what the spell does),
  ;;a description for the game (what is shown when the spell is cast), a number of actions in a list,
  ;;the maximum number of times the spell can be cast, and the reset time of the spell,
  ;;That is: the time it takes for the player to be able to use the spell again
  ;;A word on the action list: it is of the form '((actionname . actionparameters) ( ... etc))

  (let ((spells (create-table-adt 30 eq?)))

    ;;Abstractions
    (define (create-package name spelldescription castdescription actionlist max reset)
      (list name spelldescription castdescription actionlist max reset))
    (define (the-name data) (list-ref data 0))
    (define (the-spell-description data) (list-ref data 1))
    (define (the-cast-description data) (list-ref data 2))
    (define (the-actions data) (list-ref data 3))
    (define (the-maximum-nr-of-casts data) (list-ref data 4))
    (define (the-reset-time data) (list-ref data 5))
    ;;Done

    (define (add name spelldescription castdescription actionlist max reset)
      (spells 'add name (create-package name
                          spelldescription
                          castdescription
                          actionlist
                          max
                          reset)))

    (define (delete name)
      (spells 'delete name))

    (define (give key name)
      (let ((the-spell (spells 'lookup name)))
        (if the-spell
          (cond ((eq? key 'name) (the-name the-spell))
            ((eq? key 'spelldescription) (the-spell-description the-spell))
            ((eq? key 'castdescription) (the-cast-description the-spell))
            ((eq? key 'actions) (the-actions the-spell))
            ((eq? key 'max-casts) (the-maximum-nr-of-casts the-spell))
            ((eq? key 'reset-time) (the-reset-time the-spell))
            (else (error "Wrong key -- Give -- Spell Table" key)))
          (error "Spell not found in the Spell Table -- Spell Table" name))))

    (define (show)
      (spells 'show))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (apply add args))
        ((eq? m 'delete) (delete (car args)))
        ((eq? m 'give-name) (give 'name (car args)))
        ((eq? m 'give-spell-description) (give 'spelldescription (car args)))
        ((eq? m 'give-cast-description) (give 'castdescription (car args)))
        ((eq? m 'give-actions) (give 'actions (car args)))
        ((eq? m 'give-max-castings) (give 'max-casts (car args)))
        ((eq? m 'give-reset-time) (give 'reset-time (car args)))
        ((eq? m 'give-loadfactor) (spells 'give-loadfactor))
        ((eq? m 'show) (show))
        (else (error "Could not handle your request -- Spell Table" m))))

    dispatch))

(define (weapons-table)
  ;;Based on the Table ADT
  (let ((weapons (create-table-adt 50 eq?)))

    ;;Abstractions
    (define (create-package name description statistics bonus-list extras)
      (list name description statistics bonus-list extras))
    (define (the-name data) (car data))
    (define (the-description data) (cadr data))
    (define (the-statistics data) (caddr data))
    (define (the-bonus data) (cadddr data))
    (define (extras data) (list-ref data 4))
    ;;Done

    (define (add name description statistics bonus extra)

      ;;Adds a weapon to the table (that is: its name, its description, its
      ;;Combat Modifiers and its Bonus, if it has no bonus,
      ;;please enter the symbol 'none
      ;;That bonus is just a name of an action (or actions)
      ;;that can be looked up in the action table
      ;;Extras are special messages,
      ;;like a message that only allows one usage of the bonus

      (weapons 'add name (create-package name description statistics bonus extra)))

    (define (delete name-of-weapon)
      (weapons 'delete name-of-weapon))

    (define (give key name)
      ;;Gives relevant information about the weapon
      (let ((the-weapon (weapons 'lookup name)))
        (if the-weapon
          (cond ((eq? key 'name) (the-name the-weapon))
            ((eq? key 'description) (the-description the-weapon))
            ((eq? key 'statistics) (the-statistics the-weapon))
            ((eq? key 'bonus) (the-bonus the-weapon))
            ((eq? key 'extra) (extras the-weapon))
            (else (error "Wrong key -- Give -- Weapons Table" key)))
          (error "Weapon was not found in the Weapons Table -- Weapons Table" name))))

    (define (show)
      (weapons 'show))

    (define (dispatch m . args)
      (cond ((eq? m 'add) (apply add args))
        ((eq? m 'delete) (delete (car args)))
        ((eq? m 'give-name) (give 'name (car args)))
        ((eq? m 'give-description) (give 'description (car args)))
        ((eq? m 'give-statistics) (give 'statistics (car args)))
        ((eq? m 'give-bonus) (give 'bonus (car args)))
        ((eq? m 'give-extra) (give 'extra (car args)))
        ((eq? m 'give-loadfactor) (weapons 'give-loadfactor))
        ((eq? m 'show) (show))
        (else (error "Could not handle your request -- Weapons Table" m))))

    dispatch))

;;All abstractions involving actions on ADTs are listed here

;;Various
;;Little auxiliary functions
(define (add-list name alist)
  (cons name alist))

(define (delete-list name alist)
  (cond ((null? alist) '())
    ((eq? (car alist) name) (cdr alist))
    (else (cons (car alist)
            (delete-list name (cdr alist))))))

(define (display-list alist)
  (for-each (lambda (item)
              (display item)
              (display " ")) alist))

(define (display-line x)
  (display x) (newline))

(define (chars->numbers a-symbol)
  ;;converts symbols into a number
  (let* ((a-string (symbol->string a-symbol))
          (stringlist (string->list a-string)))
    (define (iter a-list result n)
      (cond ((null? a-list) result)
        (else (iter (cdr a-list)
                (+ result (* n (char->integer (car a-list)))) (+ n 1)))))
    (iter stringlist 0 1)))

(define (tree-less? x y)
  (< (chars->numbers x) (chars->numbers y)))

;;******** Player and monster ADTs *********

(define (get-character-adt character)
  (if (symbol? character)
    (the-character-table 'lookup character)
    character))

;;Useful function if it is not known wheter the action receives a player ADT or player symbol
(define (get-from-character message character)
  ((get-character-adt character) message))

(define (write-to-character message data character)
  ((get-character-adt character) message data))

;;Various abstractions to get character items
(define (get-character-inventory character)
  (get-from-character 'give-inventory character))
(define (get-character-location character)
  (get-from-character 'give-location character))
(define (get-character-world character)
  (get-from-character 'give-world character))
(define (get-character-spellbook character)
  (get-from-character 'give-spellbook character))
(define (get-character-questlog character)
  (get-from-character 'give-questlog character))
(define (get-character-status character)
  (get-from-character 'give-status character))
(define (get-character-name character)
  (get-from-character 'give-name character))
(define (get-character-class character)
  (get-from-character 'give-class character))
(define (get-character-dexterity character)
  (get-from-character 'give-dexterity character))
(define (get-character-strength character)
  (get-from-character 'give-strength character))
(define (get-character-wisdom character)
  (get-from-character 'give-wisdom character))
(define (get-character-intelligence character)
  (get-from-character 'give-intelligence character))
(define (get-character-constitution character)
  (get-from-character 'give-constitution character))
(define (get-character-charisma character)
  (get-from-character 'give-charisma character))
(define (get-offensive-weapon character)
  (give-item-from-character
    (get-from-character 'give-offensive-weapon character) character))
(define (get-defensive-weapon character)
  (give-item-from-character
    (get-from-character 'give-defensive-weapon character) character))
(define (get-character-heropoints character)
  (get-from-character 'give-heropoints character))
(define (get-character-hitpoints character)
  (get-from-character 'give-hitpoints character))

(define (get-previous-worlds character)
  (get-from-character 'give-previous-worlds character))
(define (get-previous-locations character)
  (get-from-character 'give-previous-locations character))

(define (set-character-location locationadt character)
  (write-to-character 'set-location! locationadt character))
(define (set-character-status value character)
  (write-to-character 'set-status! value character))
(define (set-offensive-weapon item character)
  (write-to-character 'set-offensive-weapon! item character))
(define (set-defensive-weapon item character)
  (write-to-character 'set-defensive-weapon! item character))
(define (set-character-world world character)
  (write-to-character 'set-world! world character))
(define (set-previous-worlds worldlist character)
  (write-to-character 'set-previous-worlds! worldlist character))
(define (set-previous-locations locationlist character)
  (write-to-character 'set-previous-locations! locationlist character))

;;Abstractions to change location and world pointers for a character
(define (add-previous-world-for-character world character)
  (let ((the-character-worldlist (get-previous-worlds character)))
    (set-previous-worlds (add-list world the-character-worldlist) character)))

(define (delete-previous-world-for-character world character)
  (let ((the-character-worldlist (get-previous-worlds character)))
    (set-previous-worlds (delete-list world the-character-worldlist) character)))

(define (add-previous-location-for-character location character)
  (let ((the-character-locationlist (get-previous-locations character)))
    (set-previous-locations (add-list location the-character-locationlist) character)))

(define (delete-previous-location-for-character location character)
  (let ((the-character-locationlist (get-previous-locations character)))
    (set-previous-locations (delete-list location the-character-locationlist) character)))

;;Abstractions to alter numerical values of a player
(define (change-stats get-message write-message sign amount character)
  (let ((stat (get-from-character get-message character)))
    (write-to-character write-message (sign stat amount) character)))

(define (do-hitpoints-up amount character)
  (change-stats 'give-hitpoints 'set-hitpoints! + amount character))
(define (do-hitpoints-down amount character)
  (change-stats 'give-hitpoints 'set-hitpoints! - amount character))
(define (do-heropoints-up amount character)
  (change-stats 'give-heropoints 'set-heropoints! + amount character))

(define (do-strength-up amount character)
  (change-stats 'give-strength 'set-strength! + amount character))
(define (do-constitution-up amount character)
  (change-stats 'give-constitution 'set-constitution! + amount character))
(define (do-wisdom-up amount character)
  (change-stats 'give-wisdom 'set-wisdom! + amount character))
(define (do-intelligence-up amount character)
  (change-stats 'give-intelligence 'set-intelligence! + amount character))
(define (do-dexterity-up amount character)
  (change-stats 'give-dexterity 'set-dexterity! + amount character))
(define (do-charisma-up amount character)
  (change-stats 'give-charisma 'set-charisma! + amount character))

(define (do-strength-down amount character)
  (change-stats 'give-strength 'set-strength! - amount character))
(define (do-constitution-down amount character)
  (change-stats 'give-constitution 'set-constitution! - amount character))
(define (do-wisdom-down amount character)
  (change-stats 'give-wisdom 'set-wisdom! - amount character))
(define (do-intelligence-down amount character)
  (change-stats 'give-intelligence 'set-intelligence! - amount character))
(define (do-dexterity-down amount character)
  (change-stats 'give-dexterity 'set-dexterity! - amount character))
(define (do-charisma-down amount character)
  (change-stats 'give-charisma 'set-charisma! - amount character))

(define (alive? character)
  (let ((character-adt (get-character-adt character)))
    (if character-adt
      (if (eq? 'alive (get-character-status character-adt))
        #t
        #f)
      #f)))

(define (dead? character)
  (let ((character-adt (get-character-adt character)))
    (if character-adt
      (if (eq? 'alive (get-character-status character-adt))
        #f
        #t)
      #t)))

;;Manipulations on a player Inventory
(define (add-item-for-character item character)
  (let ((inv (get-character-inventory character)))
    (inv 'add item)))

(define (delete-item-for-character item character)
  (let ((inv (get-character-inventory character)))
    (inv 'drop item)))

(define (drop-item-for-character item character)
  (let ((inv (get-character-inventory character))
         (off-weapon (get-offensive-weapon character))
         (def-weapon (get-defensive-weapon character)))
    (inv 'drop item)
    (cond ((eq? item off-weapon) (set-offensive-weapon 'none character))
      ((eq? item def-weapon) (set-defensive-weapon 'none character)))))

(define (use-item-for-character item character)
  (let ((inv (get-character-inventory character)))
    (inv 'use item)))

(define (give-item-from-character item character)
  (let ((inv (get-character-inventory character)))
    (inv 'give item)))

;;Manipulations on a character Spellbook
(define (add-spell-for-character spell character)
  (let ((spb (get-character-spellbook character)))
    (spb 'add spell)))

(define (erase-spell-for-character spell character)
  (let ((spb (get-character-spellbook character))
         (spell-name (get-object-name spell)))
    (if spell-name
      (spb 'erase spell-name)
      #f)))

(define (cast-spell-for-character spell character)
  (let ((spb (get-character-spellbook character)))
    (spb 'cast spell)))

(define (give-spell-from-character spell character)
  (if (procedure? spell)
    spell
    (let ((spb (get-character-spellbook character)))
      (spb 'give spell))))

;;Manipulations on a character Questlog
(define (add-quest-for-character quest character)
  (let ((qlog (get-character-questlog character)))
    (qlog 'add quest)))

(define (delete-quest-for-character quest character)
  (let ((qlog (get-character-questlog character))
         (quest-name (get-object-name quest)))
    (if quest-name
      (qlog 'delete quest-name)
      #f)))

(define (trigger-quest-for-character quest character)
  (let ((qlog (get-character-questlog character)))
    ((qlog 'give quest) 'trigger character)))

(define (give-quest-from-character quest character)
  (let ((qlog (get-character-questlog character)))
    (qlog 'give quest)))

;;Tests whether a character is a player, a monster or an npc
(define (typecheck character type)
  ;;predicate that tests for an active player
  (let ((the-character (get-character-adt character)))
    (if the-character
      (eq? (the-character 'character-type) type)
      #f)))

(define (isplayer? character)
  (typecheck character 'player))

(define (ismonster? character)
  (typecheck character 'monster))

(define (isnpc? character)
  (typecheck character 'npc))

;;********** NPC ADTs **********

(define (get-npc-adt npc)
  (if (symbol? npc)
    (the-character-table 'lookup npc)
    npc))

;;Useful function if it is not known wheter the action
;;receives a npc ADT or npc symbol

(define (get-from-npc message npc)
  ((get-npc-adt npc) message))

(define (write-to-npc message data npc)
  ((get-npc-adt npc) message data))

;;Various abstractions to get npc items
(define (get-npc-inventory npc)
  (get-from-npc 'give-inventory npc))
(define (get-npc-location npc)
  (get-from-npc 'give-location npc))
(define (get-npc-status npc)
  (get-from-npc 'give-status npc))
(define (get-npc-name npc)
  (get-from-npc 'give-name npc))

(define (set-npc-status! value npc)
  (write-to-npc 'set-status! value npc))

(define (get-npc-conversation npc)
  (get-from-npc 'give-conversation npc))
(define (get-npc-description npc)
  (get-from-npc 'give-description npc))

;;Manipulations on a npc Inventory
(define (add-item-for-npc item npc)
  (let ((inv (get-npc-inventory npc)))
    (inv 'add item)))

(define (drop-item-for-npc item npc)
  (let ((inv (get-npc-inventory npc))
         (item-name (get-object-name item)))
    (inv 'drop item-name)))

(define (use-item-for-npc item npc)
  (let ((inv (get-npc-inventory npc)))
    (inv 'use item)))

;;*********** Location ADT ***********

(define (get-location-adt location . a-world)
  (let ((search-world (if (null? a-world) the-world (car a-world))))
    (if (symbol? location)
      (search-world 'give-location location)
      location)))

(define (test-for-presence a-character-list)
  ;;Tests whether there are any characters in the room
  (not (null? a-character-list)))

(define (test-characters object a-charlist)
  ;;shows all characters in the room
  (if (test-for-presence a-charlist)
    (begin (display object)
      (display " in this area: ")
      (display-list a-charlist)
      (newline))
    (begin (display "There are no ")
      (display object)
      (display " in this area.")
      (newline))))

;;Useful function if it is not known wheter the action
;;receives a location ADT or location symbol

(define (get-from-location message location world)
  ((get-location-adt location world) message))

(define (write-to-location message data location world)
  ((get-location-adt location world) message data))

(define (get-location-name location world)
  (get-from-location 'give-name location world))
(define (get-location-description location world)
  (get-from-location 'give-description location world))
(define (get-location-items location world)
  (get-from-location 'give-item location world))
(define (set-location-items location newdata world)
  (write-to-location 'set-item! newdata location world))
(define (get-location-monsters location world)
  (get-from-location 'give-monster location world))
(define (get-location-players location world)
  (get-from-location 'give-players location world))
(define (get-location-npcs location world)
  (get-from-location 'give-npc location world))
(define (get-location-alterdescription location world)
  (get-from-location 'give-alterdescription location world))
(define (get-exit-to-direction location world)
  (car (get-from-location 'give-monster location world)))

(define (exit-location? a-location a-world)
  (let* ((location (get-location-adt a-location a-world))
          (possible-directions (location 'give-monster)))
    (or (member 'north possible-directions)
      (member 'east possible-directions)
      (member 'south possible-directions)
      (member 'west possible-directions))))

(define (add-character-to-location type character location)
  (let ((search-world (get-character-world character)))
    (cond ((eq? type 'player)
            (write-to-location 'set-players!
              (add-list character (get-location-players location search-world))
              location
              search-world))
      ((eq? type 'monster)
        (write-to-location 'set-monster!
          (add-list character (get-location-monsters location search-world))
          location
          search-world))
      ((eq? type 'npc)
        (write-to-location 'set-npc!
          (add-list character (get-location-npcs location search-world))
          location
          search-world))
      (else (error "Wrong type -- Add character to location -- Abstractions " type)))))

(define (delete-character-from-location type character location)
  (let ((search-world (get-character-world character)))
    (cond ((eq? type 'player)
            (write-to-location 'set-players!
              (delete-list character (get-location-players location search-world))
              location
              search-world))
      ((eq? type 'monster)
        (write-to-location 'set-monster!
          (delete-list character (get-location-monsters location search-world))
          location
          search-world))
      ((eq? type 'npc)
        (write-to-location 'set-npc!
          (delete-list character (get-location-npcs location search-world))
          location
          search-world))
      (else (error "Wrong type -- Delete character to location -- Abstractions " type)))))

(define (add-player-to-location player location)
  (add-character-to-location 'player (get-character-name player) location))
(define (add-monster-to-location monster location)
  (add-character-to-location 'monster (get-character-name monster) location))
(define (add-npc-to-location npc location)
  (add-character-to-location 'npc (get-npc-name npc) location))
(define (delete-player-from-location player location)
  (delete-character-from-location 'player (get-character-name player) location))
(define (delete-monster-from-location monster location)
  (delete-character-from-location 'monster (get-character-name monster) location))
(define (delete-npc-from-location npc location)
  (delete-character-from-location 'npc (get-npc-name npc) location))

;;*********** Various for Inventory/Spellbook/Questlog and Objects **********

(define (write-to-adt message data adt)
  (adt message data))

(define (get-from-object message object)
  ((get-object-adt object) message))

(define (get-object-adt object)
  (if (symbol? object)
    (error "Expects object not a symbol -- Get object ADT " object)
    object))

(define (get-object-name object)
  (if (symbol? object)
    object
    (object 'give-name)))

;;************** Spell ADT *************

(define (spell? spell-adt)
  (eq? 'spell (spell-adt 'get-type)))

(define (get-spell-description spell)
  (get-from-object 'give-spell-description spell))
(define (get-cast-description spell)
  (get-from-object 'give-cast-description spell))
(define (get-spell-max spell)
  (get-from-object 'give-spells-cast-max spell))
(define (get-spell-current spell)
  (get-from-object 'give-spells-cast-so-far spell))

;;*********** Inventory ADT ***********
;;An object is an Item or a weapon
(define (add-object-to-inventory object inventory)
  (write-to-adt 'add (get-object-adt object) inventory))

(define (delete-object-from-inventory object inventory)
  (write-to-adt 'delete (get-object-name object) inventory))

(define (lookup-object-in-inventory object inventory)
  (write-to-adt 'give (get-object-name object) inventory))

(define (use-object-in-inventory object inventory)
  (write-to-adt 'use (get-object-name object) inventory))

;;*********** Spellbook ADT ************

(define (add-spell-to-spellbook spell spellbook)
  (write-to-adt 'add (get-object-adt spell) spellbook))

(define (delete-spell-from-spellbook spell spellbook)
  (write-to-adt 'delete (get-object-name spell) spellbook))

(define (lookup-spell-in-spellbook spell spellbook)
  (write-to-adt 'give (get-object-adt spell) spellbook))

(define (cast-spell-in-spellbook spell spellbook)
  (write-to-adt 'cast (get-object-adt spell) spellbook))

;;*********** Questlog ADT ************

(define (add-quest-to-questlog quest questlog)
  (write-to-adt 'add (get-object-adt quest) questlog))

(define (mark-quest-as-done quest questlog)
  (write-to-adt 'mark-as-done (get-object-adt quest) questlog))

(define (give-quest-heropoints quest questlog)
  (write-to-adt 'give-heropoints (get-object-name quest) questlog))

;;************ Item ADT ************

(define (item? item-adt)
  (eq? 'item (item-adt 'get-type)))

(define (set-item-possesor item possesor)
  (item 'set-possesor! possesor))

;;************ Weapon ADT ***********

(define (weapon? weapon-adt)
  (eq? 'weapon (weapon-adt 'get-type)))

(define (change-stat givestat putstat sign amount weapon)
  (weapon putstat (sign (weapon givestat) amount)))

(define (increase-attack-bonus amount weapon)
  (change-stat 'give-attack-bonus 'set-attack-bonus! + amount weapon))

(define (increase-defense-bonus amount weapon)
  (change-stat 'give-defense-bonus 'set-defense-bonus! + amount weapon))

;;All command handler abstractions are listed here

;;These tests classify user commands

(define (the-executor command)
  (car command))

(define (the-keyword command)
  (cadr command))

(define (other-parameters command)
  (cddr command))

(define (keyword-extension command)
  (caddr command))

(define (command-keyword-with-pars? keyword command)
  (if (list? command)
    (and (eq? (the-keyword command) keyword)
      (symbol? (the-executor command))
      (not (null? (other-parameters command))))
    #f))

(define (command-keyword? keyword command)
  (if (list? command)
    (and (eq? (the-keyword command) keyword)
      (symbol? (the-executor command)))
    #f))

(define (move-command? command)
  (command-keyword-with-pars? 'moves command))

(define (cast-command? command)
  (command-keyword-with-pars? 'casts command))

(define (drop-all-command? command)
  (and (command-keyword-with-pars? 'drops command)
    (eq? (keyword-extension command) 'all)))

(define (get-all-command? command)
  (and (command-keyword-with-pars? 'gets command)
    (eq? (keyword-extension command) 'all)))

(define (get-command? command)
  (command-keyword-with-pars? 'gets command))

(define (drop-command? command)
  (command-keyword-with-pars? 'drops command))

(define (erase-command? command)
  (command-keyword-with-pars? 'erases command))

(define (use-command? command)
  (command-keyword-with-pars? 'uses command))

(define (read-command? command)
  (command-keyword-with-pars? 'reads command))

(define (examine-command? command)
  (command-keyword-with-pars? 'examines command))

(define (attack-command? command)
  (command-keyword-with-pars? 'attacks command))

(define (look-command? command)
  (command-keyword? 'looks command))

(define (talk-command? command)
  (command-keyword? 'talks command))

(define (flee-command? command)
  (command-keyword? 'flees command))

(define (steal-command? command)
  (command-keyword? 'steals command))

(define (ask-command? command)
  (command-keyword-with-pars? 'asks command))

(define (ask-exits? command)
  (and (ask-command? command)
    (eq? (keyword-extension command) 'exits)))

(define (ask-actions? command)
  (and (ask-command? command)
    (eq? (keyword-extension command) 'actions)))

(define (ask-status? command)
  (and (ask-command? command)
    (eq? (keyword-extension command) 'status)))

(define (ask-inventory? command)
  (and (ask-command? command)
    (eq? (keyword-extension command) 'inventory)))

(define (ask-spellbook? command)
  (and (ask-command? command)
    (eq? (keyword-extension command) 'spellbook)))

(define (ask-questlog? command)
  (and (ask-command? command)
    (eq? (keyword-extension command) 'questlog)))

(define (equip-command? command)
  (command-keyword-with-pars? 'equips command))

(define (equip-offense-command? command)
  (and (equip-command? command)
    (eq? (keyword-extension command) 'offensive)
    (eq? (cadddr command) 'weapon)))

(define (equip-defense-command? command)
  (and (equip-command? command)
    (eq? (keyword-extension command) 'defensive)
    (eq? (cadddr command) 'weapon)))

(define (cancel-actions? command)
  (and (list? command)
    (not (null? (cdr command)))
    (eq? (cadr command) 'cancels)
    (not (null? (cddr command)))))

(define (on-hold? command)
  (and (eq? (car command) 'hold)
    (number? (cadr command))
    (not (null? (cddr command)))))

(define (hold-time command)
  (cadr command))

(define (dump-hold command)
  (cddr command))

;;The command handler will analyze the command, and then dispatch it
;;the execute-command action will take care of the proper execution of
;;the specified command

(define (command-handler command time)
  (cond ((move-command? command) (execute-command 'move (filter-keyword command) time))
    ((cast-command? command) (execute-command 'cast (filter-keyword command) time))
    ((drop-all-command? command) (execute-command 'drop-all (filter-rest command) time))
    ((get-all-command? command) (execute-command 'get-all (filter-rest command) time))
    ((drop-command? command) (execute-command 'drop (filter-keyword command) time))
    ((erase-command? command) (execute-command 'erase (filter-keyword command) time))
    ((get-command? command) (execute-command 'get (filter-keyword command) time))
    ((use-command? command) (execute-command 'use (filter-keyword command) time))
    ((read-command? command) (execute-command 'player-read (filter-keyword command) time))
    ((examine-command? command) (execute-command 'player-examine (filter-keyword command) time))
    ((attack-command? command) (execute-command 'combat (filter-keyword command) time))
    ((look-command? command) (execute-command 'look (filter-rest command) time))
    ((talk-command? command) (execute-command 'converse (filter-keyword command) time))
    ((flee-command? command) (execute-command 'flee (filter-keyword command) time))
    ((steal-command? command) (execute-command 'steal (filter-keyword command) time))
    ((ask-exits? command) (execute-command 'show-exits (filter-rest command) 'no-enqueue))
    ((ask-actions? command) (execute-command 'show-actions (filter-rest command) 'no-enqueue))
    ((cancel-actions? command) (execute-command 'cancel-actions (filter-keyword command) 'no-enqueue))
    ((equip-offense-command? command) (execute-command 'equip-offence (filter-equip command) time))
    ((equip-defense-command? command) (execute-command 'equip-defense (filter-equip command) time))
    ((ask-status? command) (execute-command 'show-status (filter-rest command) 'no-enqueue))
    ((ask-inventory? command) (execute-command 'show-inventory (filter-rest command) time))
    ((ask-spellbook? command) (execute-command 'show-spellbook (filter-rest command) time))
    ((ask-questlog? command) (execute-command 'show-questlog (filter-rest command) time))
    (else (display-line "Bad command. Enter show commands to see a list of valid commands."))))

(define (filter-keyword command)
  (cons (car command)
    (cddr command)))

(define (filter-rest command)
  (list (car command)))

(define (filter-equip command)
  (cons (car command)
    (cddddr command)))

(define (execute-command action-type commandpars time)
  (let* ((command-executor (car commandpars))
          (command-type action-type)
          (command-parameters (cdr commandpars))
          (command-action (create-action-adt command-type
                            command-executor
                            command-parameters)))

    (if (eq? time 'no-enqueue)
      (core-eventhandler 'add command-action)
      (core-eventhandler 'add command-action time))

    (execute-actions)))

;;The option actions are listed here
;;Global options is the name of the global options ADT created

;;Aliases
(define (action-execution-automatic? options)
  (eq? (options 'action-execution) 'automatic))

(define (get-heropoints-to-win options)
  (options 'heropoints-to-win))

(define (get-difficulty options)
  (options 'difficulty))

(define (playstyle-continuous? options)
  (eq? (options 'playstyle) 'continuous))

(define (icons-on? options)
  (eq? (options 'iconmode) 'on))

(define (execute-actions)
  (if (action-execution-automatic? global-options)
    (core-eventhandler 'execute-step 1)
      'done))

(define (show-options)
  (global-options 'show))

(define (setup-options)
  (define (change-options choice value)
    (cond ((eq? choice 'difficulty)
            (print-change (global-options 'set-difficulty value)))
      ((eq? choice 'heropoints)
        (print-change (global-options 'set-heropoints-to-win value)))
      ((eq? choice 'playstyle)
        (print-change (if (list? value)
                        (global-options 'set-playstyle (car value) (cadr value))
                        (global-options 'set-playstyle value))))
      ((eq? choice 'action-execution)
        (print-change (global-options 'set-action-execution value)))
      ((eq? choice 'action-hold-offset)
        (print-change (global-options 'set-action-hold-offset value)))
      ((eq? choice 'icons)
        (print-change (global-options 'set-iconmode value)))
      (else (error "Wrong choice -- Change options" choice))))

  (define (print-change result)
    (if (eq? result 'ok)
      (display-line "Options changed succesfully")
      (display-line "Wrong value for that option. Options have not changed"))
    (newline))

  (newline)
  (display-line "*** Welcome to the Setup options menu ***")
  (global-options 'show)
  (newline)
  (display-line "Which option do you want to change?")
  (display-line "Difficulty -- Heropoints -- Playstyle -- Action-Execution -- Action-Hold-Offset -- Icons")
  (display "Choose one: ")
  (let ((choice (read)))
    (if (member choice '(difficulty heropoints playstyle action-execution action-hold-offset icons))
      (begin (display "Enter new value: ")
        (let ((input (read)))
          (change-options choice input)))
      (begin (display "Wrong choice: ")
        (display-line choice)
        (newline)))))

;;Various useful actions
;;most are building blocks for spells

;;functions to alter character stats
(define (stats-up stat stat-message player an-amount)
  (let ((amount (if (pair? an-amount) ;;eg 2d6 => (2 . 6) => diceroll needed
                  (dicethrow an-amount)
                  an-amount)))
    (stat-message amount player)
    (display (get-character-name player))
    (display " gained ")
    (display amount)
    (display-line stat)))

(define (stats-down stat stat-message player an-amount)
  (let ((amount (if (pair? an-amount)
                  (dicethrow an-amount)
                  an-amount)))
    (stat-message amount player)
    (display (get-character-name player))
    (display " lost ")
    (display amount)
    (display-line stat)))

(define (hp-up player amount)
  (stats-up " hitpoints" do-hitpoints-up player amount))
(define (heropoints-up player amount)
  (stats-up " heropoints" do-heropoints-up player amount))
(define (strength-up player amount)
  (stats-up " strength points" do-strength-up player amount))
(define (dexterity-up player amount)
  (stats-up " dexterity points" do-dexterity-up player amount))
(define (constitution-up player amount)
  (stats-up " constitution points" do-constitution-up player amount))
(define (wisdom-up player amount)
  (stats-up " wisdom points" do-wisdom-up player amount))
(define (intelligence-up player amount)
  (stats-up " intelligence points" do-intelligence-up player amount))
(define (charisma-up player amount)
  (stats-up " charisma points" do-charisma-up player amount))

(define (hp-down player amount)
  (stats-down " hitpoints" do-hitpoints-down player amount))
(define (strength-down player amount)
  (stats-down " strength points" do-strength-down player amount))
(define (dexterity-down player amount)
  (stats-down " dexterity points" do-dexterity-down player amount))
(define (constitution-down player amount)
  (stats-down " constitution points" do-constitution-down player amount))
(define (wisdom-down player amount)
  (stats-down " wisdom points" do-wisdom-down player amount))
(define (intelligence-down player amount)
  (stats-down " intelligence points" do-intelligence-down player amount))
(define (charisma-down player amount)
  (stats-down " charisma points" do-charisma-down player amount))

;;this function shows all targets and lets you select one
(define (select-target-in-room a-player type)
  (define (show-targets)
    (cond ((eq? type 'all)
            (show-targets-in-room a-player 'players)
            (show-targets-in-room a-player 'monsters))
      ((eq? type 'players) (show-targets-in-room a-player 'players))
      ((eq? type 'monsters) (show-targets-in-room a-player 'monsters))
      ((eq? type 'npcs) (show-targets-in-room a-player 'npcs))
      (else (error "Wrong type -- Select Target In room" type))))

  (define (test-target target-name)
    (let* ((current-world (get-character-world a-player))
            (current-location (get-character-location a-player))
            (possible-human-targets (get-location-players current-location current-world))
            (possible-monster-targets (get-location-monsters current-location current-world))
            (possible-npc-targets (get-location-npcs current-location current-world)))

      (cond ((eq? type 'all) (or (memq target-name possible-human-targets)
                               (memq target-name possible-monster-targets)))
        ((eq? type 'players) (memq target-name possible-human-targets))
        ((eq? type 'monsters) (memq target-name possible-monster-targets))
        ((eq? type 'npcs) (memq target-name possible-npc-targets)))))

  (define (select-target)
    (display "Select Target: ")
    (let* ((targetname (read))
            (target (the-character-table 'lookup targetname)))
      (if (test-target targetname)
        (if target
          target
          (begin (display-line "That is not a valid target.")
            #f))
        (begin (display-line "That target is not in this area.")
          #f))))

  (if (isplayer? a-player)
    (begin (display-line "Available Targets: ")
      (show-targets)
      (select-target))
    (select-random-player (get-character-location a-player))))

;;this function shows players, monsters or npcs
(define (show-targets-in-room a-player charactertype)
  (let* ((player-world (get-character-world a-player))
          (location (get-character-location a-player))
          (location-adt (get-location-adt location player-world)))
    (cond ((eq? charactertype 'monsters)
            (test-characters 'monsters (get-location-monsters location-adt player-world)))
      ((eq? charactertype 'players)
        (test-characters 'players (get-location-players location-adt player-world)))
      ((eq? charactertype 'npcs)
        (test-characters 'npcs (get-location-npcs location-adt player-world)))
      (else (error "Wrong Charactertype -- Show Targets in room -- Various Actions" charactertype)))))

;;action used for monsters to select their target when casting spells
(define (select-random-player a-location)
  (let* ((player-list (a-location 'give-players))
          (listlength (length player-list))
          (choice (if (null? player-list)
                    #f
                    (random listlength))))
    (if choice
      (get-character-adt (list-ref player-list choice))
      choice)))

;;shows all monsters and players,
;;lets you select one and deals an amount of damage to your target
(define (deal-x-damage-to-target dealer the-damage)
  (let* ((damage (if (pair? the-damage)
                   (dicethrow the-damage)
                   the-damage))
          (caster-adt (get-character-adt dealer))
          (target (if (isplayer? caster-adt)
                    (select-target-in-room dealer 'all)
                    ;;players choose their targets themselves
                    (select-random-player (get-character-location caster-adt)))))
    ;;monsters choose random players
    (if target
      (begin (display (target 'give-name))
        (display " suffered ")
        (display damage)
        (display-line " damage.")
        (do-hitpoints-down damage target)))))

(define (display-spell caster spell)
  (let ((spell-adt (give-spell-from-character spell caster)))
    (if spell-adt
      (begin (display (get-character-name caster))
        (display " is casting a ")
        (display-line (get-object-name spell))
        (display-line (spell-adt 'give-cast-description)))
        'done)))

(define (reset-spell possesor spell)
  (let ((spell-adt (give-spell-from-character spell possesor)))
    (if spell-adt
      (spell-adt 'set-spells-cast-so-far! 0)
        'done))) ;;this might be the case when
;;the wizard dropped the spell in the meanwhile

(define (perform-enter-actions-for-location player)
  ;;Performs all the actions a location should undertake
  ;;when a player enters the room

  (define (iter actionlist)
    (if (null? actionlist)
        'done
      (begin (let* ((action-name (car actionlist))
                     (action-adt (create-action-adt action-name (get-character-name player) '())))
               (core-eventhandler 'add action-adt 'hold))
        (iter (cdr actionlist)))))

  (let* ((the-location (get-character-location player))
          (location-actions (the-location 'give-actions)))
    (if (null? location-actions)
      (execute-actions)
      (begin (iter location-actions)
        (execute-actions)))))

;;All specific spell casting actions are listed here

(define (cast-doom caster)
  ;;casts the spell doom
  (let ((target (select-target-in-room caster 'all))
         (damage (cons 2 4)))
    (if target
      (begin (strength-down target damage)
        (dexterity-down target damage)
        (constitution-down target damage)
        (charisma-down target damage)
        (display (get-character-name target))
        (display-line " is doomed")
        (let ((undo-action (create-action-adt 'undo-doom-effects target '()))
               (run-time (core-eventhandler 'give-runtime)))
          (core-eventhandler 'add undo-action (+ run-time 30)))))))

(define (cast-judgement caster)
  ;;casts the spell judgement
  (let* ((target (select-target-in-room caster 'all))
          (caster-charisma (get-character-charisma caster))
          (target-charisma (get-character-charisma target))
          (damage (- caster-charisma target-charisma)))
    (if target
      (if (< damage 0)
        (begin (display (get-character-name target))
          (display-line "has been judged positive"))
        (begin (strength-down target damage)
          (dexterity-down target damage)
          (constitution-down target damage)
          (charisma-down target damage)
          (display (get-character-name target))
          (display-line " is judged negative")
          (let ((undo-action (create-action-adt 'undo-doom-effects
                               target
                               (list damage)))
                 (run-time (core-eventhandler 'give-runtime)))
            (core-eventhandler 'add undo-action (+ run-time 30))))))))

(define (cast-curse caster)
  ;;casts the spell curse
  (let ((target (select-target-in-room caster 'all))
         (damage (round (/ (get-character-wisdom caster) 3))))
    (if target
      (begin (intelligence-down target damage)
        (wisdom-down target damage)
        (display (get-character-name target))
        (display-line " is cursed")
        (let ((undo-action (create-action-adt 'undo-curse-effects
                             target
                             (list damage)))
               (run-time (core-eventhandler 'give-runtime)))
          (core-eventhandler 'add undo-action (+ run-time 30)))))))

(define (summon caster creature-type)

  (define (conditions-fulfilled? player creature type)
    (let* ((the-player (get-character-adt player))
            (player-strength (get-character-strength the-player))
            (player-wisdom (get-character-strength the-player))
            (player-intelligence (get-character-strength the-player))
            (player-charisma (get-character-strength the-player))
            (creature-strength (get-character-strength creature))
            (creature-charisma (get-character-strength creature))
            (creature-wisdom (get-character-wisdom creature))
            (creature-intelligence (get-character-intelligence creature))
            (greater-strength? (> creature-strength player-strength))
            (greater-charisma? (> creature-charisma player-charisma))
            (greater-intelligence? (> creature-intelligence player-intelligence))
            (greater-wisdom? (> creature-wisdom player-wisdom)))
      (cond ((eq? type 'demon)
              (if (and greater-strength? greater-charisma?)
                (begin (combat creature caster) ;;demon turns agains his owner
                  #f)
                #t))
        ((eq? type 'angel)
          (if (and greater-strength? greater-charisma?)
            #f
            #t))
        ((eq? type 'imp)
          (if greater-charisma?
            #f
            #t))
        ((eq? type 'dragon)
          (if (and greater-strength?
                greater-charisma?
                greater-intelligence?
                greater-wisdom?)
            (begin (combat creature caster) ;;dragon turns againts his owner
              #f)
            (if (and greater-strength? greater-intelligence?)
              #f
              #t)))
        ((eq? type 'shadow)
          (if greater-wisdom?
            #f
            #t))
        (else #t))))

  (let* ((summon-location (get-character-location caster))
          (summoned-creature (make-monster 'summoned-monster creature-type summon-location))
          (unsummon-action (create-action-adt 'unsummon-monster
                               'summoned-monster
                             (list summoned-creature)))
          (run-time (core-eventhandler 'give-runtime)))
    (display "You summoned a ")
    (display-line creature-type)
    (display-line "Who do you want it to attack?")
    (let ((target (select-target-in-room caster 'all)))
      (if target
        (if (conditions-fulfilled? caster summoned-creature creature-type)
          ;;some creatures require conditions to be fulfilled
          (begin (combat summoned-creature target) ;;engage in combat
            (core-eventhandler 'add unsummon-action (+ run-time 5))
            (display-line "Creature will be unsummoned in 5 turns"))
          (display-line "Cannot summon: conditions for summoning such creature not fulfilled"))))))

(define (cast-force-drop caster)
  (let ((target (select-target-in-room caster 'all)))
    (if target
      (begin (drop-all target)
        (display-line "Your force drop spell was effective")))))

(define (cast-terror caster)
  ;;casts the spell terror
  (let ((target (select-target-in-room caster 'all))
         (damage (get-character-intelligence caster)))
    (if target
      (begin (strength-down target damage)
        (dexterity-down target damage)
        (constitution-down target damage)
        (charisma-down target damage)
        (intelligence-down target damage)
        (wisdom-down target damage)
        (drop-all target)
        (display (get-character-name target))
        (display-line " is terrorized")
        (let ((undo-action (create-action-adt 'undo-terror-effects
                             target
                             (list damage)))
               (run-time (core-eventhandler 'give-runtime)))
          (core-eventhandler 'add undo-action (+ run-time 40)))))))

;;undo effects:
(define (undo caster type)
  (let ((undo-action (create-action-adt type (get-character-name caster) '()))
         (run-time (core-eventhandler 'give-runtime)))
    (core-eventhandler 'add undo-action (+ run-time 30))))

(define (undo-doom-effects target)
  (strength-up target '(2 . 4))
  (dexterity-up target '(2 . 4))
  (constitution-up target '(2 . 4))
  (charisma-up target '(2 . 4))
  (display "Doom effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-judgement-effects target damage)
  (strength-up target damage)
  (dexterity-up target damage)
  (constitution-up target damage)
  (charisma-up target damage)
  (display "Judgement effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-curse-effects target damage)
  (intelligence-up target damage)
  (wisdom-up target damage)
  (display "Curse effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-terror-effects target damage)
  (strength-up target damage)
  (dexterity-up target damage)
  (constitution-up target damage)
  (charisma-up target damage)
  (intelligence-up target damage)
  (wisdom-up target damage)
  (display "Terror effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (unsummon-monster summoned-creature-name summoned-creature)
  (let ((monster-name (get-character-name summoned-creature)))
    (display "Unsummoning ")
    (display-line (get-character-class summoned-creature))
    (summoned-creature 'set-status! 'dead)
    (core-eventhandler 'delete-all monster-name)
    (the-character-table 'delete monster-name)))

(define (undo-blessing-effects target)
  (strength-down target '(2 . 4))
  (dexterity-down target '(2 . 4))
  (display "Blessing effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-greater-blessing-effects target)
  (strength-down target '(2 . 4))
  (dexterity-down target '(2 . 4))
  (wisdom-down target '(3 . 6))
  (display "Greater blessing effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-brainstorm-effects target)
  (intelligence-down target '(2 . 6))
  (wisdom-down target '(2 . 6))
  (display "Brainstorm effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-bloodlust-effects target)
  (strength-down target '(4 . 10))
  (constitution-up target '(4 . 10))
  (display "Bloodlust effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-berserk-effects target)
  (strength-down target '(4 . 10))
  (hp-up target '(3 . 10))
  (display "Berserk effects on ")
  (display (get-character-name target))
  (display-line " undone"))

;;All game actions are listed here
(define classlist '(fighter priest wizard paladin monk necromancer druid thief ranger))

(define (add-player-to-game)

  (define (choose-class player-name)
    (newline)
    (display "Choose your character class (type help to see a list of classes): ")
    (let ((class (read)))
      (cond ((eq? class 'help)
              (display "Available classes: ")
              (display-list classlist)
              (choose-class player-name))
        ((the-class-table 'lookup class)
          (let ((new-player (create-player-adt player-name class)))
            (the-character-table 'add new-player)
            (display-line "+++ New Character Created +++")
            (newline)
            (display "*** ")
            (display player-name)
            (display-line " has entered the game ***")))
        (else (display "The class ")
          (display class)
          (display-line " does not exist.")))))

  (newline)
  (display-line "+++ Create New Player +++")
  (display "Please enter your character name: ")
  (let ((chosen-name (read)))
    (if (symbol? chosen-name)
      (if (the-character-table 'lookup chosen-name)
        (display-line "That name already exists, please choose another one")
        (choose-class chosen-name))
      (begin (display chosen-name)
        (display-line " is an invalid name.")))))

(define (delete-player-from-game player)
  (let* ((player-location (get-character-location player)))
    (delete-player-from-location player player-location)
    (drop-all player display-off)
    (the-current-players 'delete player)
    (core-eventhandler 'delete-all player)
    (newline)
    (display "*** ")
    (display player)
    (display-line " has left the game ***")
    (newline)
    (the-character-table 'delete player)))

(define (player-wins-game a-player)
  (define (game-over-loop)
    (display "Do you want to continue with higher heropoint limit (c) or stop the game? (s) ")
    (let ((continue (read)))
      (newline)
      (cond ((or (eq? continue 'c) (eq? continue 'continue))
              (display-line "Game will continue, please reset the heropoint limit to a higher number"))
        ((or (eq? continue 's) (eq? continue 'stop))
          (display-line "Game Over -- Bye")
          (set! game-over? #t))
        (else (display-line "Please enter continue or stop")
          (game-over-loop)))))
  ;;when a player wins by scoring the limited heropoints
  (let ((player-name (get-character-name a-player))
         (player-adt (get-character-adt a-player)))
    (newline)
    (display "*** ")
    (display player-name)
    (display-line " has won the game! ***")
    (display "Congratulations new hero. You have scored ")
    (display (get-character-heropoints player-adt))
    (display-line " heropoints.")
    (game-over-loop)))

;;The game handler dispatches several commands
;;to create new players, delete players etc

(define (game-handler command)
  (cond ((new-player-command? command) (add-player-to-game))
    ((delete-player-command? command) (delete-player-from-game (caddr command)))
    ((show-options-command? command) (show-options))
    ((setup-options-command? command) (setup-options))
    ((show-commands-command? command) (show-commands))
    ((show-players-command? command) (the-current-players 'show))
    (else (display-line "Bad command. That game command does not exist. Type show commands for a list of valid commands."))))

(define (new-player-command? command)
  (equal? '(game add player) command))

(define (delete-player-command? command)
  (and (equal? 'game (car command))
    (equal? 'quit (cadr command))
    (not (null? (cddr command)))))

(define (setup-options-command? command)
  (equal? command '(game setup options)))

(define (show-options-command? command)
  (equal? command '(game show options)))

(define (show-commands-command? command)
  (equal? command '(game show commands)))

(define (show-players-command? command)
  (equal? command '(game show players)))

;;This part contains all actions involving players
;;monsters can sometimes perform the same actions
;;the argument displayhandler is a function that can be used
;;to control the output, you can choose whether or not to display things
;;with the following 2 functions

(define (display-on x . special)
  ;;specials can be:
  ;;line for newline
  ;;list for a display-list
  ;;listline for a combination of the latter two
  (cond ((eq? x newline) (newline))
    ((null? special) (display x))
    ((eq? (car special) 'line) (display x) (newline))
    ((eq? (car special) 'list) (display-list x))
    ((eq? (car special) 'listline) (display-list x) (newline))
    (else (display x))))

(define (display-off x . special)
    'done)

(define (display-player-icon player command)
  (if (isplayer? player)
    (display-icon command)
      'done))

(define (spellcaster? player)
  (let ((the-class (get-character-class player)))
    (memq the-class spellcaster-classes)))

(define (rogue? player)
  (let ((the-class (get-character-class player)))
    (memq the-class rogue-classes)))

(define (show-commands)
  (display-line "Valid commands: ")
  (display-line "Name >> Description >> Syntax")
  (for-each
    (lambda (command)
      (let ((name (car command))
             (descr (cadr command))
             (syntax (caddr command)))
        (display name)
        (display ": ")
        (display descr)
        (display "   ")
        (display-line syntax)))
      '(("move " "lets you move around in the world " "player_name MOVES direction")
         ("look " "lets you look around in a location " "player_name LOOKS AROUND")
         ("flee " "lets you flee in a random direction " "player_name FLEES")
         ("talk " "lets you talk to NPC's in the world " "player_name TALKS")
         ("steal " "lets you steal items from NPC's if you're a rogue  " "player_name STEALS")
         ("cast " "lets you cast a spell if you're a spellcaster " "player_name CASTS spell_name")
         ("get " "lets you pick up items " "player_name GETS item_name")
         ("get-all " "lets you pick up all items " "player_name GETS ALL")
         ("drop " "lets you drop an item from your inventory " "player_name DROPS item_name")
         ("drop " "lets you drop a spell from your spellbook " "player_name ERASES spell_name")
         ("drop-all " "lets you drop all your items " "player_name DROPS ALL")
         ("use " "lets you use an item or a weapon " "player_name USES item_name")
         ("read " "lets you read a quest " "player_name READS quest_name")
         ("examine " "lets you examine an item " "player_name EXAMINES item_name")
         ("equip-offence" "lets you equip your offensive weapon " "player_name EQUIPS OFFENSIVE WEAPON weapon_name")
         ("equip-defense" "lets you equip your defensive weapon " "player_name EQUIPS DEFENSIVE WEAPON weapon_name")
         ("attack " "lets you attack other monsters and players " "player_name ATTACKS opponent_name")
         ("actions " "shows your enqueued actions " "player_name ASKS ACTIONS")
         ("exits " "shows all possible exits from this location " "player_name ASKS EXITS")
         ("status " "shows your status " "player_name ASKS STATUS")
         ("inventory " "shows your inventory " "player_name ASKS INVENTORY")
         ("spellbook " "shows your spellbook " "player_name ASKS SPELLBOOK")
         ("questlog " "shows your questlog " "player_name ASKS QUESTLOG")
         ("cancel " "cancels an action in manual action execution " "player_name CANCELS action_name")
         ("options " "shows the options " "GAME SHOW OPTIONS")
         ("setup options " "lets you modify the options " "GAME SETUP OPTIONS")
         ("add player " "adds a new player to the game " "GAME ADD PLAYER")
         ("delete player " "a player quits from the game " "GAME QUIT player_name"))))

(define (teleport player to-location . options)
  (let* ((the-player (get-character-adt player))
          (old-worlds (if (isplayer? the-player)
                        (get-previous-worlds the-player)
                          '()))
          (old-world (if (null? old-worlds) #f (car old-worlds)))
          (from-location (get-character-location the-player)))
    (set-character-location to-location player)
    (if (isplayer? player) ;;monsters and players treated differently
      (begin (add-player-to-location player to-location)
        (if old-world
          (if (old-world 'isworld? from-location)
              'done ;;we're moving from one world to another
            (delete-player-from-location player from-location))
          ;;top level, we cant go to another world beyond this one
          (if (the-world 'isworld? from-location)
            ;;test to treat top level differently
              'done ;;nothing to do
            (delete-player-from-location player from-location))))
      (begin (add-monster-to-location player to-location)
        (delete-monster-from-location player from-location)))))

(define (exit-world player direction)
  ;;lets a player leave the current world and return
  ;;to the previous one through an exit

  (let* ((the-player (get-character-adt player))
          (oldlocation (get-character-location the-player))
          (oldworld (get-character-world the-player))
          (previousworlds (get-previous-worlds the-player))
          (previouslocations (get-previous-locations the-player))
          (newworld (car previousworlds))
          (newlocation (car previouslocations)))
    (set-character-world newworld the-player)
    (set-character-location newlocation the-player)
    (delete-previous-location-for-character newlocation the-player)
    (delete-previous-world-for-character newworld the-player)
    (delete-player-from-location the-player oldlocation)
    ;;Deletes the player from the old room

    ;;Display the event:
    (display (get-character-name the-player))
    (display " moves back from ")
    (display (get-location-name oldworld newworld))
    (display " to ")
    (display-line (newworld 'give-name))

    ;;The player will now move on a higher level in the world
    (move the-player direction)))

(define (move player direction . options)

  (define (report-move player new-location new-world direction displayhandler)
    (displayhandler (get-character-name player))
    (displayhandler " moves ")
    (displayhandler direction)
    (displayhandler " to ")
    (displayhandler (get-location-name new-location new-world) 'line)
    (if (isplayer? player)
      (begin (displayhandler (get-location-description new-location new-world) 'line)
        (look player))
        'done))

  ;;Moves a player to the desired direction (also works for monsters)
  (let* ((displayhandler (if (null? options)
                           display-on
                           (if (list? (car options)) ;;extra check for apply
                             (caar options)
                             (car options))))
          (oldlocation (get-character-location player))
          (oldworld (get-character-world player))
          (newlocation (oldworld 'give-neighbour oldlocation direction)))
    (display-player-icon player 'move)
    (cond ;;CASE 1: Location not found
      ((eq? newlocation 'not-found) ;;There is no location in that direction
        (if (isplayer? player)
          (begin (displayhandler (get-character-name player))
            (displayhandler ", you cannot travel in that direction. " 'line))
            'done))

      ;;CASE 2: Location is itself a world
      ((oldworld 'isworld? newlocation) ;;The new location is actually a world
        (if (isplayer? player) ;;Only players can travel through worlds
          (let* ((newworld newlocation)
                  (to-location (newworld 'startpoint?))
                  (previous-worlds (get-previous-worlds player))
                  (previous-locations (get-previous-locations player)))
            (set-character-world newworld player)
            (add-previous-location-for-character newworld player)
            (add-previous-world-for-character oldworld player)
            (teleport player to-location)
            (report-move player to-location newworld direction displayhandler)
            (perform-enter-actions-for-location player))
          (flee player displayhandler))) ;;monsters flee when entering new world

      ;;CASE 3: Location is an exit out of this world
      ((exit-location? newlocation oldworld) ;;The new location is an exit
        (if (isplayer? player) ;;only players can exit worlds
          (exit-world player (get-exit-to-direction newlocation oldworld))
          (flee player displayhandler)))

      ;;CASE 4: Location is an ordinary location
      (else (teleport player newlocation)
        (report-move player newlocation oldworld direction displayhandler)
        (perform-enter-actions-for-location player)))))

(define (look player)
  ;;A player looks around, sees the room description,
  ;;the items and the characters
  ;;currently in the room
  (let* ((the-location (get-character-location player))
          (the-world (get-character-world player))
          (the-description (get-location-alterdescription the-location the-world))
          (the-items (get-location-items the-location the-world))
          (the-monsterlist (get-location-monsters the-location the-world))
          (the-npclist (get-location-npcs the-location the-world))
          (the-playerlist (get-location-players the-location the-world)))
    (display-player-icon player 'look)
    (display-line the-description)
    (if (null? the-items)
      (display-line "There are no items or weapons in this room")
      (begin (display "Items and weapons in this room: ")
        (display-list (map (lambda (item) (item 'give-name)) the-items))
        (newline)))
    (test-characters 'Monsters the-monsterlist)
    (test-characters 'Players the-playerlist)
    (test-characters 'NPCs the-npclist)
    (newline)))

(define (flee player . options)
  (let ((displayhandler (if (null? options)
                          display-on
                          (if (list? (car options))
                            ;;extra check for apply
                            (caar options)
                            (car options)))))
    ;;A random direction is chosen and the player moves to that direction
    ;;also works for monsters
    (let ((the-direction (+ (random 4) 1))
           (display-flee (lambda (direction)
                           (displayhandler (get-character-name player))
                           (displayhandler " tries to flee ")
                           (displayhandler direction 'line))))
      (display-player-icon player 'flee)
      (cond ((eq? the-direction 1)
              (display-flee 'north)
              (move player 'north displayhandler))
        ((eq? the-direction 2)
          (display-flee 'south)
          (move player 'south displayhandler))
        ((eq? the-direction 3)
          (display-flee 'east)
          (move player 'east displayhandler))
        (else (display-flee 'west)
          (move player 'west displayhandler))))))

(define (converse player)
  (display-player-icon player 'talk)
  ;;A player initiates dialogue with an NPC
  (let* ((player-adt (get-character-adt player))
          (player-location (get-character-location player))
          (player-world (get-character-world player))
          (npc-list (get-location-npcs player-location player-world)))
    (test-characters 'NPCs npc-list)
    (newline)
    (display "With whom do you want to converse? ")
    (let* ((the-npc (read))
            (npc-adt (get-npc-adt the-npc)))
      (if (and (memq the-npc npc-list) the-npc)
        (begin (display the-npc)
          (display ": ")
          (npc-adt 'conversation player-adt))
        (display "That NPC cannot be talked to. ")))))

(define (steal player)
  ;;A Player with the Rogue Class can steal
  ;;an item from an NPC, the chance of succes depends on his dexterity

  (define (rob an-npc a-player)
    (let* ((npc-inventory (get-npc-inventory an-npc))
            (player-inventory (get-character-inventory a-player))
            (npc-item (random-item an-npc)))

      (if (not npc-item)
        (display-line "NPC has nothing to steal.")
        (begin
          ;;Gives random NPC item, deletes it from the NPC inventory
          ;;and adds it to the player's one
          (npc-inventory 'drop (get-object-name npc-item))
          (player-inventory 'add npc-item)
          (display (get-character-name a-player))
          (display " has stolen ")
          (display (get-object-name npc-item))
          (display " from ")
          (display-line (get-npc-name an-npc))))))

  (define (attempt)
    (let* ((player-adt (get-character-adt player))
            (player-location (get-character-location player))
            (player-world (get-character-world player))
            (npc-list (get-location-npcs player-location player-world)))
      (test-characters 'NPCs npc-list)
      (newline)
      (display "Who do you want to rob? ")
      (let* ((the-npc (read))
              (npc-adt (get-npc-adt the-npc)))
        (if (and (memq the-npc npc-list) the-npc)
          (begin (display "You try to rob ")
            (display-line (get-npc-name npc-adt))
            (rob npc-adt player-adt))
          (display "That NPC cannot be robbed.")))))

  (display-player-icon player 'steal)
  (if (rogue? player)
    ;;Tests if the player is a rogue, rogues are the
    ;;only class allowed to steal.
    (let* ((the-dexterity (get-character-dexterity player))
            (try (random the-dexterity)))
      ;;The player's trial is based on his dexterity
      ;;If the player's trial is higher than 8, he can make an attempt
      (if (< 8 try)
        (attempt)
        (display-line "Your attempt to rob someone has failed, due to your lack of dexterity.")))
    (display-line "You cannot steal, you are not a Rogue.")))

(define (cast player spell . options)
  (let ((displayhandler (if (null? options)
                          display-on
                          (if (list? (car options))
                            ;;extra check for apply (because of dot notation)
                            (caar options)
                            (car options)))))
    ;;First test whether the player is a spellcaster
    ;;also works for monsters
    (display-player-icon player 'cast)
    (if (and (isplayer? player) (not (spellcaster? player)))
      (displayhandler "You cannot cast spells, you are not a Spellcaster." 'line)
      (let* ((player-adt (get-character-adt player))
              (spell-adt (give-spell-from-character spell player-adt)))
        (if (not spell-adt)
          (displayhandler "You haven't got that spell." 'line)
          ;;This function lets the player cast its particular spell,
          ;;the spell itself calls it's own functions
          (cast-spell-for-character (get-object-name spell-adt) player-adt))))))

(define (use player item . options)
  ;;also works for monsters
  (let* ((displayhandler (if (null? options)
                           display-on
                           (if (list? (car options)) ;;extra check for apply
                             (caar options)
                             (car options))))
          (player-adt (get-character-adt player))
          (item-adt (give-item-from-character item player-adt)))
    (display-player-icon player 'use)

    (if (not item-adt)
      (displayhandler "You haven't got that item." 'line)
      (begin (displayhandler (get-character-name player))
        (displayhandler " uses ")
        (displayhandler (get-object-name item-adt) 'line)
        ;;This function lets the player use its item,
        ;;the item itself calls it's own functions
        (use-item-for-character (get-object-name item-adt) player-adt)))))

(define (get player item . options)

  ;;search and delete combines the searching of the item
  ;;and the deletion of the item from the location list
  (define (search-and-delete item-name item-list)
    (define (iter rest deletedlist neededitem)
      (if (null? rest)
        (cons neededitem deletedlist)
        (let ((current-item ((car rest) 'give-name)))
          (if (eq? item-name current-item)
            (iter '()
              (append deletedlist (cdr rest))
              (car rest))
            (iter (cdr rest)
              (cons (car rest) deletedlist)
                '())))))
    (iter item-list '() '()))

  (display-player-icon player 'get)

  (let ((displayhandler (if (null? options)
                          display-on
                          (if (list? (car options)) ;;extra check for apply
                            (caar options)
                            (car options)))))
    ;;A player can pick up an item (or a weapon or spell) in a room
    (let* ((location (get-character-location player))
            (world (get-character-world player))
            (location-items (get-location-items location world))
            (item-name (get-object-name item))
            (items-in-list (search-and-delete item-name location-items)))
      (if (not (null? (car items-in-list)))
        (begin (set-location-items location
                 (cdr items-in-list)
                 world)
          (let* ((the-item (car items-in-list))
                  (item-type (the-item 'get-type)))
            (if (eq? item-type 'spell) ;;item picked up is a spell
              (add-spell-for-character the-item player)
              (add-item-for-character the-item player)))
          (displayhandler "You picked up: ")
          (displayhandler item-name 'line))
        (displayhandler "You cannot pick up that item." 'line)))))

(define (drop player item . options)
  (drop-action give-item-from-character
    drop-item-for-character
    player
    item
    options))

(define (erase player item . options)
  (drop-action
    give-spell-from-character
    erase-spell-for-character
    player
    item
    options))

(define (drop-action get-function put-function player item . the-options)
  (let* ((options (car the-options))
          ;;because extra options are already passed via drop and erase
          (displayhandler (if (null? options)
                            display-on
                            (if (list? (car options)) ;;extra check for apply
                              (caar options)
                              (car options)))))
    (display-player-icon player 'drop)

    ;;A player drops an item in a room
    (let* ((location (get-character-location player))
            (world (get-character-world player))
            (location-items (get-location-items location world))
            (item-name (get-object-name item))
            (item-in-inventory (get-function item player)))
      (if item-in-inventory
        (begin (set-location-items location
                 (add-list item-in-inventory location-items)
                 world)
          (put-function item-name player)
          (fix-equipment player displayhandler)
          (displayhandler "You dropped: ") (displayhandler item-name 'line))
        (displayhandler "You haven't got that item." 'line)))))

;;checks whether the player has not dropped his current equipment
(define (fix-equipment a-player displayhandler)
  (let* ((player-adt (get-character-adt a-player))
          (off-weapon (get-offensive-weapon player-adt))
          (def-weapon (get-defensive-weapon player-adt))
          (check-off-for-none (eq? 'none (player-adt 'give-offensive-weapon)))
          (check-def-for-none (eq? 'none (player-adt 'give-defensive-weapon))))
    (if (and (not check-off-for-none)
          ;;player already knows he has no offensive weapon
          (not off-weapon)) ;;player has thrown away his offensive weapon
      (equip-offence player-adt 'none displayhandler)
        'done)
    (if (and (not check-def-for-none)
          ;;player already knows he has no defensive weapon
          (not def-weapon)) ;;player has thrown away his defensive weapon
      (equip-defense player-adt 'none displayhandler)
        'done)))

(define (get-all player . options)
  (let ((displayhandler (if (null? options)
                          display-on
                          (if (list? (car options)) ;;extra check for apply
                            (caar options)
                            (car options)))))
    (display-player-icon player 'get)

    ;;A player can pick up all items in a room
    (let* ((location (get-character-location player))
            (world (get-character-world player))
            (location-items (get-location-items location world)))
      (if (null? location-items)
        (displayhandler "There is nothing to pick up in this room." 'line)
        (begin (displayhandler "You picked up: ")
          (displayhandler (map (lambda (item)
                                 (item 'give-name))
                            location-items) 'list)
          (for-each
            (lambda (item)
              (if (eq? (item 'get-type) 'spell) ;;item picked up is a spell
                (add-spell-for-character item player)
                (add-item-for-character item player)))
            location-items)
          (set-location-items location '() world)
          (displayhandler newline))))))

(define (drop-all player . options)
  (let ((displayhandler (if (null? options)
                          display-on
                          (if (list? (car options))
                            ;;extra check for apply
                            (caar options)
                            (car options)))))
    (display-player-icon player 'drop)

    ;;A player drops all of his items in a room
    (let* ((location (get-character-location player))
            (world (get-character-world player))
            (location-items (get-location-items location world))
            (player-inventory (get-character-inventory player))
            (player-spellbook (get-character-spellbook player))
            (temp-items-for-location location-items)
            (temp-items-to-delete '())
            (temp-spells-to-delete '()))

      (player-inventory 'for-each ;;enumerates all items to add to the location
        (lambda (item)
          (set! temp-items-to-delete
            (cons (item 'give-name) temp-items-to-delete))
          (set! temp-items-for-location
            (cons item temp-items-for-location))))

      (for-each (lambda (item) ;;all items must be deleted afterwards
                  (drop-item-for-character item player))
        temp-items-to-delete)

      (player-spellbook 'for-each
        (lambda (spell)
          (set! temp-spells-to-delete
            (cons (spell 'give-name) temp-spells-to-delete))
          (set! temp-items-for-location
            (cons spell temp-items-for-location))))

      (for-each (lambda (spell) ;;all spells deleted afterwards
                  (erase-spell-for-character spell player))
        temp-spells-to-delete)

      ;;Extend the location items list
      (set-location-items location temp-items-for-location world)
      ;;Set the player's equiped weapons to none
      (equip-offence player 'none displayhandler)
      (equip-defense player 'none displayhandler)
      (displayhandler (get-character-name player))
      (displayhandler " dropped all of his possesions" 'line))))

(define (player-read player quest)
  (let* ((the-player (get-character-adt player))
          (the-quest (give-quest-from-character quest the-player)))
    (display-player-icon player 'read)

    (if the-quest
      (begin (display-line (the-quest 'give-title))
        (display-line (the-quest 'give-description))
        (display "This Quest is worth ")
        (display (the-quest 'give-heropoints))
        (display-line " heropoints."))
      (display-line "You haven't got that quest."))))

(define (player-examine player item)
  (let* ((the-player (get-character-adt player))
          (the-item (give-item-from-character item the-player)))
    (display-player-icon player 'examine)

    (if the-item
      (if (item? the-item)
        (begin (display "Examining item: ")
          (display-line (the-item 'give-description)))
        (examine-weapon the-item))
      (display-line "You haven't got that item."))))

(define (examine-weapon weapon-adt)

  (define (display-dice dice)
    (let ((nr (car dice))
           (size (cdr dice)))
      (display nr)
      (display "d")
      (display size)
      (newline)))

  ;;expects weapon adt
  (let ((weapon-off-damage (weapon-adt 'give-attack-damage))
         (weapon-def-damage (weapon-adt 'give-defense-damage))
         (weapon-off-bonus (weapon-adt 'give-attack-bonus))
         (weapon-def-bonus (weapon-adt 'give-defense-bonus))
         (weapon-description (weapon-adt 'give-description)))
    (display "Examining weapon: ") (display-line weapon-description)
    (display "Attack Damage:  ") (display-dice weapon-off-damage)
    (display "Defense Damage: ") (display-dice weapon-def-damage)
    (display "Attack Bonus:   ") (display weapon-off-bonus) (newline)
    (display "Defense Bonus:  ") (display weapon-off-bonus) (newline)))

(define (equip player weapon choice . options)
  (let ((displayhandler (if (null? options)
                          display-on
                          (if (list? (car options))
                            ;;extra check for apply
                            (caar options)
                            (car options)))))
    ;;Equips a player with an offensive or defensive weapon,
    ;;a check is performed to see whether the player has the weapon
    ;;none is a special symbol indicating the player hasn't got a weapon equiped
    (let ((the-weapon (give-item-from-character weapon player)))
      (if (or the-weapon (eq? weapon 'none))
        (if (or (eq? weapon 'none) (eq? 'weapon (the-weapon 'get-type)))
          (cond ((eq? choice 'offence)
                  (display-player-icon player 'equip-offence)
                  (set-offensive-weapon weapon player)
                  (displayhandler "Offensive weapon set to ")
                  (displayhandler (get-object-name weapon) 'line))
            ((eq? choice 'defense)
              (display-player-icon player 'equip-defense)
              (set-defensive-weapon weapon player)
              (displayhandler "Defensive weapon set to ")
              (displayhandler (get-object-name weapon) 'line))
            (else (error "Wrong choice offence/defense, given: " choice)))
          (displayhandler "You cannot equip items." 'line))
        (displayhandler "You haven't got that weapon." 'line)))))

(define (equip-offence player weapon . displayhandler)
  (equip player weapon 'offence (if (null? displayhandler)
                                  display-on
                                  (car displayhandler))))

(define (equip-defense player weapon . displayhandler)
  (equip player weapon 'defense (if (null? displayhandler)
                                  display-on
                                  (car displayhandler))))

(define (show-status player)
  ((get-character-adt player) 'show-status))

(define (show-inventory player)
  (let ((player-inventory (get-character-inventory player)))
    (display-player-icon player 'inventory)
    (display (get-character-name player))
    (display-line "'s possesions: ")
    (player-inventory 'for-each (lambda (item)
                                  (display (get-object-name item)) (display ": ")
                                  (display-line (item 'give-description))))
    (newline)))

(define (show-spellbook player)
  (let ((player-spellbook (get-character-spellbook player)))
    (display-player-icon player 'spellbook)
    (display (get-character-name player))
    (display-line "'s spells: ")
    (player-spellbook 'for-each (lambda (spell)
                                  (display (get-object-name spell)) (display ": ")
                                  (display-line (spell 'give-spell-description))))
    (newline)))

(define (show-questlog player)
  (let* ((player-questlog (get-character-questlog player))
          (done-quests (player-questlog 'give-completed)))
    (display-player-icon player 'questlog)
    (display (get-character-name player))
    (display-line "'s quests: ")
    (player-questlog 'for-each (lambda (quest)
                                 (display (get-object-name quest)) (display ": ")
                                 (display-line (quest 'give-title))))
    (display "Quests already completed: ")
    (if (null? done-quests)
      (display "none so far")
      (display-list (player-questlog 'give-completed)))
    (newline)))

(define (show-exits player)
  (let* ((the-player (get-character-adt player))
          (the-player-world (get-character-world the-player))
          (the-location (get-character-location the-player)))
    (the-player-world 'for-each-neighbour the-location
      (lambda (fromlbl frominfo tolbl toinfo edgeinfo)
        (let* ((type (car toinfo))
                (location (cdr toinfo))
                (direction edgeinfo)
                (destination tolbl))
          (cond ((eq? type 'world) ;;a neighbour is a world
                  (display "A road leading ")
                  (display direction)
                  (display " to the world ")
                  (display-line destination))
            ((exit-location? location the-player-world)
              ;;a neighbour is an exit
              (let* ((previous-worlds (get-previous-worlds the-player))
                      (previous-world (car previous-worlds))
                      (world-name (previous-world 'give-name)))
                (display "An exit leading ")
                (display direction)
                (display " to ")
                (display-line world-name)))
            (else ;;a neigbour is a location
              (display "A road leading ")
              (display direction)
              (display " to ")
              (display-line destination))))))))

(define (show-actions player)
  (let ((player-name (get-character-name player))
         (global-time (core-eventhandler 'give-time)))
    (display "Actions enqueued for ")
    (display player-name)
    (display-line ": ")
    (core-eventhandler 'for-each-action
      (lambda (action-pair)
        (let* ((time (car action-pair))
                (action (cdr action-pair))
                (executor (action 'give-executer)))
          (if (and (eq? executor player-name)
                (not (eq? (action 'give-name) 'monster-attack)))
            ;;filter out monster attacks
            (begin
              (display "Number of turns: ")
              (display (- time global-time))
              (display " ")
              (display (action 'give-name))
              (display " ")
              (display-line (action 'give-parameters)))))))))

(define (cancel-actions player action)
  (core-eventhandler 'delete action player))

;;All specific monster actions are listed here
;;some monster actions are also found in the player actions file

(define (monster-attack player)
  ;;A monster in a room attacks a player
  (let* ((location (get-character-location player))
          (world (get-character-world player))
          (monsterlist (get-location-monsters location world)))
    (if (null? monsterlist)
        'done
      (let ((the-monster (car monsterlist)))
        (combat the-monster player)
        (attack-in-the-future location world)))))

(define (multiple-monster-attack player)
  ;;Multiple monsters all attack the same player
  (let* ((location (get-character-location player))
          (world (get-character-world player))
          (monsterlist (get-location-monsters location world)))

    (define (multi-attack monsterlist)
      (if (or (null? monsterlist)
            (dead? player))
          'done
        (begin (combat (car monsterlist) player)
          (multi-attack (cdr monsterlist)))))

    (cond ((null? monsterlist) 'done)
      ((= (length monsterlist) 1) (combat (car monsterlist) player))
      (else (multi-attack monsterlist)))
    (attack-in-the-future location world)))

;;Monsters not always attack when a player enters the room,
;;monsters should also attack when a player resides in their room
;;this function takes care of that
(define (attack-in-the-future a-location a-world)
  (let ((location-players (get-location-players a-location a-world))
         (location-monsters (get-location-monsters a-location a-world)))
    (if (or (null? location-monsters)
          (null? location-players))
        'done
      (let* ((attack-action (create-action-adt 'monster-attack
                              (car location-players)
                                '()))
              (time (core-eventhandler 'give-runtime))
              (enqueue-time (+ time
                              (random 5)
                              (global-options 'action-hold-offset))))
        (core-eventhandler 'add attack-action enqueue-time)))))

(define (monster-travelling monster)
  (let* ((monster-name (get-character-name monster))
          (the-monster-adt (the-character-table 'lookup monster-name)))
    (if the-monster-adt
      (the-monster-adt 'walk-through-route)
      #f)))

;;Monster AI choices

(define (random-generator C F A N)
  ;;input percentages of chance:
  ;;C cast
  ;;F flee
  ;;A attack
  ;;N nothing
  (let ((choicenumber (random 100))
         (accumulatedC C)
         (accumulatedF (+ C F))
         (accumulatedA (+ C F A))
         (accumulatedN (+ C F A N)))
    (cond ((< choicenumber accumulatedC) 'cast)
      ((< choicenumber accumulatedF) 'flee)
      ((< choicenumber accumulatedA) 'attack)
      ((< choicenumber accumulatedN) 'nothing)
      (else 'nothing))))

(define (check-strength monster opponent)
  (let* ((monsterstrength (get-character-strength monster))
          (monsterintelligence (get-character-intelligence monster))
          (opponentstrength (get-character-strength opponent))
          (choicefactor (+ (random 20) 5))
          (result (- monsterstrength opponentstrength)))
    (if (< 0 result) ;;losing monster
      (if (< monsterintelligence choicefactor) ;;dumb monster
        #t
        #f)
      #t)))

(define (check-opponent-healthpoints monster opponent)
  (let* ((monsterhp (get-character-hitpoints monster))
          (monsterintelligence (get-character-intelligence monster))
          (opponenthp (get-character-hitpoints opponent))
          (choicefactor (+ (random 20) 5))
          (result (- monsterhp opponenthp)))
    (if (< 0 result) ;;losing monster
      (if (< monsterintelligence choicefactor) ;;dumb monster
        #t
        #f)
      #t)))

(define (check-opponent-weapon monster opponent)
  (let* ((monsterweapon (get-offensive-weapon monster))
          (monsterintelligence (get-character-intelligence monster))
          (opponentweapon (get-offensive-weapon opponent))
          (monsterdamage (if (or (not monsterweapon)
                               (eq? monsterweapon 'none))
                           0
                           (rolldice (get-offensive-damage monsterweapon))))
          (opponentdamage (if (or (not opponentweapon)
                                (eq? opponentweapon 'none))
                            0
                            (rolldice (get-offensive-damage opponentweapon))))
          (choicefactor (+ (random 20) 5))
          (result (- monsterdamage opponentdamage)))
    (if (< 0 result) ;;losing monster
      (if (< monsterintelligence choicefactor) ;;dumb monster
        #t
        #f)
      #t)))

(define (simulate-battle monster opponent)
  (let* ((monsterstats (car (get-statistics monster 'attacker)))
          (opponentstats (car (get-statistics opponent 'defender)))
          (monsterintelligence (get-character-intelligence monster))
          (choicefactor (+ (random 20) 5))
          (result (- monsterstats opponentstats)))
    (if (< 0 result) ;;losing monster
      (if (< monsterintelligence choicefactor) ;;dumb monster
        #t
        #f)
      #t)))

(define (check-own-healthpoints monster opponent)
  (let* ((monsterhp (get-character-hitpoints monster))
          (monsterintelligence (get-character-intelligence monster))
          (monsterclass (get-character-class monster))
          (classhp (the-class-table 'give-hitpoints monsterclass))
          (choicefactor (+ (random 20) 5))
          (result (/ monsterhp classhp)))
    (if (< 0 .3 result) ;;monster has critical hp
      (if (< monsterintelligence choicefactor) ;;dumb monster
        #t
        #f)
      #t)))

(define (restore-hp monster)
  (let* ((monster-inv (get-character-inventory monster))
          (monster-spb (get-character-spellbook monster))
          (healitem (search-healing-item-in monster-inv))
          (healspell (search-healing-spell-in monster-spb)))
    (if healspell
      (cast monster healspell display-on)
      (if healitem
        (use monster healitem display-on)
          'done))))

(define (easy-AI-tree monster-adt opponent-adt)
  (random-generator 10 30 25 35))

(define (normal-AI-tree monster-adt opponent-adt)
  ;;Tree structure represented by ifs
  ;;a positive check returns #t, a negative check returns #f
  (if (check-own-healthpoints monster-adt opponent-adt)
    (if (check-strength monster-adt opponent-adt)
      (random-generator 20 20 40 20)
      (if (check-opponent-healthpoints monster-adt opponent-adt)
        (random-generator 20 15 40 25)
        (random-generator 15 25 30 30)))
    (restore-hp monster-adt)))

(define (hard-AI-tree monster-adt opponent-adt)
  ;;Tree structure represented by ifs
  ;;a positive check returns #t, a negative check returns #f
  (if (check-own-healthpoints monster-adt opponent-adt)
    (if (check-strength monster-adt opponent-adt)
      (if (check-opponent-weapon monster-adt opponent-adt)
        (random-generator 25 5 40 30)
        (if (simulate-battle monster-adt opponent-adt)
          (random-generator 23 2 50 25)
          (random-generator 20 30 25 25)))
      (if (check-opponent-healthpoints monster-adt opponent-adt)
        (random-generator 25 5 40 30)
        (if (simulate-battle monster-adt opponent-adt)
          (random-generator 23 2 50 25)
          (random-generator 20 30 25 25))))
    (restore-hp monster-adt)))

(define (monster-AI-routine monster opponent . options)
  ;;options are the difficulty and the displayhandler

  (define (execute-choice choice displayhandler difficulty)
    (cond ((eq? choice 'cast)
            (let ((the-spell (random-spell monster)))
              (if the-spell
                (cast monster the-spell displayhandler)
                (execute-actions))))
      ((eq? choice 'flee) (flee monster displayhandler))
      ((eq? choice 'attack) (combat monster
                              opponent
                              displayhandler
                              #t
                              difficulty))
      ((eq? choice 'nothing) (execute-actions))
      (else (execute-actions))))

  (if (and (alive? monster)
        (alive? opponent))

    (let* ((difficulty (if (null? options)
                           'normal
                         (cond ((or (equal? (car options) 0 .5)
                                  (eq? (car options) 'easy)) 'easy)
                           ((or (eq? (car options) 1)
                              (eq? (car options) 'normal)) 'normal)
                           ((or (eq? (car options) 2)
                              (eq? (car options) 'hard)) 'hard)
                           ((error "Wrong difficulty number -- monster AI Routine" options)))))
            (displayhandler (if (null? options) display-on (cadr options)))
            (monster-adt (get-character-adt monster))
            (opponent-adt (get-character-adt opponent)))

      ;;traverse the AI tree, this produces a symbol (flee, attack etc)
      ;;this symbol is then transformed into an action through execute-choice

      (cond ((eq? difficulty 'easy)
              (execute-choice (easy-AI-tree monster-adt opponent-adt)
                displayhandler
                difficulty))
        ((eq? difficulty 'normal)
          (execute-choice (normal-AI-tree monster-adt opponent-adt)
            displayhandler
            difficulty))
        ((eq? difficulty 'hard)
          (execute-choice (hard-AI-tree monster-adt opponent-adt)
            displayhandler
            difficulty))
        (else (error "Wrong difficulty -- AI Routine" difficulty))))

    (execute-actions)))

(define (respawn monster-name
          monster-class
          monster-location
          monster-route
          monster-respawn)
  (if (and monster-respawn (not (eq? monster-respawn 'none)))
    (let* ((runtime (core-eventhandler 'give-runtime))
            (enqueuetime (+ runtime (car monster-respawn)))
            (respawn-action (create-action-adt 'respawn-monster
                              monster-name
                              (list monster-class
                                monster-location
                                monster-route))))
      (core-eventhandler 'add respawn-action enqueuetime))
      'done))

(define (respawn-monster monster-name
          monster-class
          monster-location
          monster-route)
  (make-monster monster-name
    monster-class
    monster-location
    monster-route))

;;The Combat Actions are listed here

;;Dicethrow will transform a couple like (2 . 6) in
;;a random number
(define (dicethrow specs)
  (define (loop n upper)
    (if (= n 0)
      0
      (+ (loop (- n 1) upper)
        (+ 1 (random upper)))))
  (let ((nr-of-dice (car specs))
         (nr-of-dice-sides (cdr specs)))
    (loop nr-of-dice nr-of-dice-sides)))

(define (get-statistics character choice)
  ;;looks up all the relevant stats of the character
  ;;choice is either attacker or defender
  ;;it returns the calculated damage and the heropoints
  (let ((the-character (get-character-adt character)))
    (if the-character
      (let* ((strength (get-character-strength the-character))
              (dexterity (get-character-dexterity the-character))
              (heropoints (if (isplayer? the-character)
                            (get-character-heropoints the-character)
                              'none))
              (weapondamage (if (eq? choice 'attacker)
                              (let ((offweapon (get-offensive-weapon the-character)))
                                (if (not offweapon) ;;no equiped weapon
                                  strength
                                  (dicethrow (offweapon 'give-attack-damage))))
                              (let ((defweapon (get-defensive-weapon the-character)))
                                (if (not defweapon)
                                  dexterity
                                  (dicethrow (defweapon 'give-defense-damage))))))
              (weaponbonus (if (eq? choice 'attacker)
                             (let ((offweapon (get-offensive-weapon the-character)))
                               (if (not offweapon) ;;no equipped weapon
                                 0
                                 (offweapon 'give-attack-bonus)))
                             (let ((defweapon (get-defensive-weapon the-character)))
                               (if (not defweapon) ;;no equipped weapon
                                 0
                                 (defweapon 'give-defense-bonus)))))
              (damagefactor (+ 10 (random 10)))
              (resultingdamage (/ (+ (* weapondamage strength)
                                    weaponbonus
                                    dexterity) damagefactor)))
        (cons (round resultingdamage) heropoints))
      (error "Character does not exist -- Get Statistics -- Combat actions" character))))

(define (calc-damage attack-stats defense-stats)
  ;;returns a list of the form '(winner damage attackertype defendertype)
  (let* ((attackdamage (car attack-stats))
          (defensedamage (car defense-stats))
          (attackheropoints (cdr attack-stats))
          (defenseheropoints (cdr defense-stats))
          (winner 'not-known)
          (result (- attackdamage defensedamage))
          (attacker 'not-known)
          (defender 'not-known))
    (if (eq? attackheropoints 'none)
      (set! attacker 'monster)
      (set! attacker 'player))
    (if (eq? defenseheropoints 'none)
      (set! defender 'monster)
      (set! defender 'player))
    (if (> result 0)
      (set! winner 'attacker)
      (set! winner 'defender))
    ;;check whether the 2 characters are players or not
    (if (and (eq? attacker 'player)
          (eq? defender 'player))
      (let* ((bonus (quotient (abs (- attackheropoints defenseheropoints))
                      (+ 1 (random 20))))
              (resultbonus (round bonus)))
        (list winner (+ resultbonus (abs result)) attacker defender))
      (list winner (abs result) attacker defender))))

(define (prompt-for-spell-cast attacker defender displayhandler spellcast? difficulty)
  ;;lets the spellcaster decide to cast a spell or not
  (let ((attacker-name (get-character-name attacker))
         (defender-name (get-character-name defender)))
    (display attacker-name)
    (display ", do you want to engage in melee combat or cast a spell? (melee/spell) ")
    (let ((choice (read)))
      (cond ((eq? choice 'melee)
              (combat attacker defender displayhandler #f difficulty))
        ((eq? choice 'spell)
          (announce-spell attacker displayhandler))
        (else (newline)
          (display-line "Wrong choice, please select melee or spell")
          (prompt-for-spell-cast attacker defender displayhandler spellcast? difficulty))))))

(define (display-all-spells caster . options)
  ;;shows all spells the caster possesses
  ;;options contains one parameter:
  ;;if true, then additional spell info is shown
  ;;if false, then only brief descriptions are shown
  (let ((info? (if (null? options) #f (car options)))
         (the-spellbook (get-character-spellbook caster)))
    (display-line "Spells in possesion:")
    (the-spellbook 'for-each
      (lambda (spell-adt)
        (let ((spell-name (get-object-name spell-adt))
               (spell-description (get-spell-description spell-adt))
               (spell-cast-points (cons (get-spell-current spell-adt)
                                    (get-spell-max spell-adt))))
          (display spell-name)
          (if info?
            (begin (display ": ")
              (display spell-description)))
          (display " (")
          (display (car spell-cast-points))
          (display "/")
          (display (cdr spell-cast-points))
          (display-line ")"))))))

(define (announce-spell caster displayhandler)
  ;;allows the player to select a spell
  (display-all-spells caster)
  (display "Which spell do you want to cast? ")
  (let* ((the-spell (read)))
    (cast caster the-spell displayhandler)))

(define (report-combat attacker defender winner damage displayhandler)
  (let* ((attackername (get-character-name attacker))
          (defendername (get-character-name defender))
          (winnername (if (eq? winner 'attacker) attackername defendername))
          (losername (if (eq? winner 'attacker) defendername attackername)))
    (displayhandler attackername)
    (displayhandler " attacks ")
    (displayhandler defendername 'line)
    (displayhandler winnername)
    (displayhandler " deals ")
    (displayhandler damage)
    (displayhandler " damage to ")
    (displayhandler losername 'line)))

(define (legal-combat? attacker defender)
  ;;A combat is legal if both characters are in the same room
  ;;if there are no npcs involved
  ;;and if both characters are still alive
  (let ((attacker-adt (get-character-adt attacker))
         (defender-adt (get-character-adt defender)))
    (cond ((not attacker-adt)
            (display "attacker ")
            (display attacker)
            (display-line " does not exist") #f)
      ((not defender-adt)
        (display "defender ")
        (display defender)
        (display-line " does not exist") #f)
      (else
        (let ((attackerlocation (get-character-location attacker-adt))
               (defenderlocation (get-character-location defender-adt)))
          (if (and (alive? attacker-adt)
                (alive? defender-adt)
                (not (isnpc? attacker-adt))
                (not (isnpc? defender-adt))
                (eq? attackerlocation defenderlocation))
            #t
            #f))))))

(define (convert value)
  (cond ((eq? value 'easy) 0 .5)
    ((eq? value 'normal) 1)
    ((eq? value 'hard) 2)
    (else (error "Wrong value to convert -- Convert" value))))

(define (combat attacker defender . options)

  ;;options are: '(displayhandler spellcast? difficulty)
  ;;Displayhandler can be used to suppress the output
  ;;spellcast decides whether a mage is allowed to cast spells in this fight
  ;;difficulty has 3 values: easy normal and hard and influences the monster's damage

  (display-player-icon attacker 'attack)

  (if (not (legal-combat? attacker defender)) ;;check for legal combat
    (display-line "Illegal Combat, ignoring command. ")

    (let ((displayhandler (if (null? options) display-on (car options)))
           (spellcast? (if (null? options) #t (cadr options)))
           (difficulty (if (null? options) (convert (global-options 'difficulty))
                         (cond ((eq? (caddr options) 'easy) 0 .5)
                           ((eq? (caddr options) 'normal) 1)
                           ((eq? (caddr options) 'hard) 2)
                           ((error "Wrong difficulty keyword -- Combat" options))))))

      ;;If the player is a spellcaster, he can cast a spell
      ;;instead of engaging melee combat
      (if (and spellcast? (spellcaster? attacker))
        (prompt-for-spell-cast attacker
          defender
          displayhandler
          spellcast?
          (if (null? options)
              'normal
            (caddr options)))

        (let* ((attacker-adt (get-character-adt attacker))
                (defender-adt (get-character-adt defender))
                (attacker-name (get-character-name attacker))
                (defender-name (get-character-name defender))
                (attacker-stats (get-statistics attacker-adt 'attacker))
                (defender-stats (get-statistics defender-adt 'defender))
                (results (calc-damage attacker-stats defender-stats))
                (winner (list-ref results 0)) ;;winner = attacker or defender
                (damage-to-deal (list-ref results 1)) ;;the damage
                (attackertype (list-ref results 2)) ;;type = player or monster
                (defendertype (list-ref results 3))
                (winner-adt (if (eq? winner 'attacker) attacker-adt defender-adt))
                (loser-adt (if (eq? winner 'attacker) defender-adt attacker-adt))
                (adjusted-damage (* difficulty damage-to-deal))
                (add-damage-action (lambda (damage) ;;enqueue the damage action
                                     (core-eventhandler 'add (create-action-adt 'deal-x-damage-to
                                                               loser-adt
                                                               (list damage)) 'hold)))
                (add-report-action (lambda (damage) ;;enqueue the report (display) action
                                     (core-eventhandler 'add (create-action-adt 'report-combat attacker-name
                                                               (list defender-name winner damage displayhandler)) 'hold))))

          (cond ((and (eq? attackertype 'player) ;;2 players
                   (eq? defendertype 'player)) ;;Execute the actions, and combat is over
                  (add-report-action damage-to-deal)
                  (add-damage-action damage-to-deal)
                  (execute-actions)
                  (distribute-heropoints attacker-adt defender-adt displayhandler))
            ;;If a player wins, he gains the other player's heropoints

            ((and (eq? attackertype 'monster) ;;attacker is a monster
               (eq? defendertype 'player)) ;;monster can perform an action
              (add-report-action damage-to-deal)
              (add-damage-action adjusted-damage)
              (execute-actions)
              (distribute-heropoints attacker-adt defender-adt displayhandler)
              (monster-AI-routine attacker-adt defender-adt difficulty displayhandler))

            ((and (eq? attackertype 'player) ;;defender is a monster
               (eq? defendertype 'monster)) ;;monster can perform an action
              (add-report-action damage-to-deal)
              (add-damage-action adjusted-damage)
              (execute-actions)
              (distribute-heropoints attacker-adt defender-adt displayhandler)
              ;;If the monster is dead, player receives points
              (monster-AI-routine defender-adt attacker-adt difficulty displayhandler))

            (else (add-report-action damage-to-deal)
              (add-damage-action damage-to-deal)
              (execute-actions)
              (distribute-heropoints attacker-adt defender-adt displayhandler)
              (monster-AI-routine defender-adt attacker-adt difficulty displayhandler)
              (monster-AI-routine attacker-adt defender-adt difficulty displayhandler))))))))

(define (deal-x-damage-to victim damage)
  (do-hitpoints-down damage victim))

(define (distribute-heropoints attacker defender . options)

  (define (report-heropoints-up player heropoints displayhandler)
    (displayhandler (get-character-name player))
    (displayhandler " gains ")
    (displayhandler heropoints)
    (displayhandler " heropoints." 'line))

  (let ((displayhandler (if (null? options) display-on (car options)))
         (attackerstatus (get-character-status attacker))
         (defenderstatus (get-character-status defender))
         (points-earned (get-character-heropoints defender)))
    (cond ((and (eq? attackerstatus 'alive)
             (eq? defenderstatus 'dead))
            (let ((points-earned (get-character-heropoints defender)))
              (do-heropoints-up points-earned attacker)
              (report-heropoints-up attacker points-earned displayhandler)))
      ((and (eq? attackerstatus 'dead)
         (eq? defenderstatus 'alive))
        (let ((points-earned (get-character-heropoints attacker)))
          (do-heropoints-up points-earned defender)
          (report-heropoints-up defender points-earned displayhandler)))
      (else 'done))))

(define (search-healing-item-in inventory)
  ;;Expects Inventory ADT
  (let ((choicelist '()))
    (inventory 'for-each
      (lambda (item-adt)
        (let ((item-name (get-object-name item-adt)))
          (if (memq item-name healing-items)
            ;;the item is a healing item
            (set! choicelist (cons item-adt choicelist))))))
    (if (null? choicelist)
      #f
      (let* ((choice (random (length choicelist)))
              (item-chosen (list-ref choicelist choice)))
        item-chosen))))

(define (search-healing-spell-in spellbook)
  ;;Expects Spellbook ADT
  (let ((choicelist '()))
    (spellbook 'for-each
      (lambda (spell-adt)
        (let ((spell-name (get-object-name spell-adt)))
          (if (memq spell-name healing-spells) ;;the spell is a healing spell
            (set! choicelist (cons spell-adt choicelist))))))
    (if (null? choicelist)
      #f
      (let* ((choice (random (length choicelist)))
              (spell-chosen (list-ref choicelist choice)))
        spell-chosen))))

(define (random-item character)
  (let* ((character-adt (get-character-adt character))
          (character-inv (get-character-inventory character-adt))
          (choicelist '()))
    (character-inv 'for-each
      (lambda (item-adt)
        (set! choicelist (cons item-adt choicelist))))
    (if (null? choicelist)
      #f
      (let* ((choice (random (length choicelist)))
              (item-chosen (list-ref choicelist choice)))
        item-chosen))))

(define (random-spell character)
  (let* ((character-adt (get-character-adt character))
          (character-spb (get-character-spellbook character-adt))
          (choicelist '()))
    (character-spb 'for-each
      (lambda (spell-adt)
        (set! choicelist (cons spell-adt choicelist))))
    (if (null? choicelist)
      #f
      (let* ((choice (random (length choicelist)))
              (spell-chosen (list-ref choicelist choice)))
        spell-chosen))))

;;Initialisation of the tables

(define the-actions-table (action-table))
(define the-class-table (class-table))
(define the-items-table (items-table))
(define the-locations-table (location-table))
(define the-npc-table (npc-table))
(define the-quest-table (quest-table))
(define the-spells-table (spell-table))
(define the-weapons-table (weapons-table))
(define the-character-table (character-table))
(define table-loader-dummy
  (begin
    (display "Tables Loaded")
    (newline)))

;;Essentials

;;Setting up options
(define global-options (the-options))

;;The eventhandler
(define core-eventhandler (create-eventhandler-ADT 0)) ;; <<== Changed!
(define print-eventhandler-dummy
  (begin
    (display "Eventhandler loaded")
    (newline)))

;;The Current Players
(define the-current-players (current-players-adt))
(define begin-players-dummy
  (begin (display "Current Player list loaded")
    (newline)))

;;Special abilities classes and special item lists
(define spellcaster-classes '(priest wizard monk druid necromancer paladin))
(define rogue-classes '(monk druid thief))
(define healing-items '(small-potion
                         medium-potion
                         large-potion
                         super-potion))
(define healing-spells '(heal-light-wounds
                          heal-medium-wounds
                          heal-large-wounds
                          heal-critical-wounds
                          blessing
                          greater-blessing))

;;This file contains the "driver loop" for the game
(define game-over? #f)

(define (start-the-game)
  (initialise-game)
  (input-loop))

(define (input-loop)
  (if game-over?
    (begin (display-line "Game Over, thanks for playing"))
    (if (playstyle-continuous? global-options)
      (execute-continuous-turn)
      (execute-turnbased-turn))))

(define (execute-continuous-turn)
  (display "(Continuous)Enter new command: ")
  (let ((user-input (read)))
    (newline)
    (cond ((action-execution? user-input)
            (core-eventhandler 'execute-step (cadr user-input)))
      ((game-command? user-input) ;;eg new game, new player etc
        (game-handler user-input))
      (else (execute-regular-command user-input)))
    (newline))
  (input-loop))

(define (execute-regular-command user-input . turnbased-player)
  (let ((turnbased? (if (null? turnbased-player) #f #t))
         (offset (global-options 'action-hold-offset))
         (eventhandler-time (core-eventhandler 'give-runtime)))
    (cond ((and (on-hold? user-input)
             (if turnbased?
               (check-player (caddr user-input) (car turnbased-player))
               (check-player (caddr user-input))))
            (command-handler (dump-hold user-input) (+ offset eventhandler-time (hold-time user-input)))
            (display-line "*** Action Held ***")) ;;action enqueued for the future
      ((if turnbased?
         (check-player (car user-input) (car turnbased-player))
         (check-player (car user-input)))
        (command-handler user-input 1))
      (else (display-line "That player does not exist or wrong player turn")))))

(define (check-player player-name . player-needed)
  (if (not (null? player-needed))
    (eq? player-name (car player-needed))
    (let ((active-players (the-current-players 'give-player-list)))
      (member player-name active-players))))

(define (turn-status)
  (let ((turncounter (global-options 'nr-of-actions-allowed))
         (listcounter 0))
    (define (dispatch m)
      (cond ((eq? m 'give-listcounter) listcounter)
        ((eq? m 'increment-listcounter)
          (if (= (+ listcounter 1) (length (the-current-players 'give-player-list)))
            (set! listcounter 0)
            (set! listcounter (+ 1 listcounter))))
        ((eq? m 'decrement-turncounter) (set! turncounter (- turncounter 1)))
        ((eq? m 'give-turncounter) turncounter)
        ((eq? m 'check-turncounter) (= turncounter 0))
        ((eq? m 'set-turncounter-to-0) (set! turncounter 0))
        ((eq? m 'reset-turncounter)
          (set! turncounter (global-options 'nr-of-actions-allowed)))
        (else (error "Wrong message -- Turn Status" m))))
    dispatch))

;;Object that keeps track of counting the turns and players
(define the-turn-status (turn-status))

(define (execute-turnbased-turn)

  (let* ((current-players (the-current-players 'give-player-list))
          (player-position (the-turn-status 'give-listcounter))
          (active-player (if (< player-position (length current-players))
                           (list-ref current-players player-position)
                           (car current-players))))

    (display "(Turnbased) ") (display active-player)
    (display "'s turn (") (display (the-turn-status 'give-turncounter)) (display "): ")

    (let ((user-input (read)))
      (newline)
      (cond ((skip-turn? user-input)
              (the-turn-status 'set-turncounter-to-0))
        ((action-execution? user-input)
          (core-eventhandler 'execute-step (the-keyword user-input)))
        ((game-command? user-input) ;;eg new game, new player etc
          (game-handler user-input))
        (else (execute-regular-command user-input active-player)
          (the-turn-status 'decrement-turncounter)
          (newline)))

      (if (the-turn-status 'check-turncounter)
        (begin (the-turn-status 'reset-turncounter)
          (the-turn-status 'increment-listcounter)
          (input-loop))
        (input-loop)))))

(define (initialise-game)
  (define (add-player-loop)
    (add-player-to-game)
    (display "Add another player? (yes/no): ")
    (let ((choice (read)))
      (cond ((eq? choice 'yes) (add-player-loop))
        ((eq? choice 'no) (display-line "Players added succesfully")
          (options?))
        (else (display-line "Wrong choice, try again")
          (add-player-loop)))))
  (display-line "Adding new players: ")
  (add-player-loop))

(define (options?)
  (display "Do you want to change the options? (yes/no): ")
  (let ((choice (read)))
    (cond ((eq? choice 'yes) (newline) (setup-options))
      ((eq? choice 'no) (newline))
      (else (display-line "Wrong choice, try again")
        (newline)
        (options?)))))

;;Abstractions
(define (action-execution? command)
  (and (list? command)
    (eq? (car command) 'go)
    (not (null? (cdr command)))
    (number? (cadr command))))

(define (game-command? command)
  (and (list? command)
    (eq? (car command) 'game)
    (not (null? (cdr command)))))

(define (skip-turn? command)
  (or (equal? command 'skip)
    (equal? command 'skip-turn)
    (equal? command '(skip turn))))

;;All aliases to enter game data are listed here

(define (create-item name description functions extra)
  (the-items-table 'add name description functions extra))

(define (create-weapon name description statistics actions extras)
  (the-weapons-table 'add name description statistics actions extras))

(define (create-spell name spelld castd actions nr-of-casts reset-time)
  (the-spells-table 'add name spelld castd actions nr-of-casts reset-time))

(define (create-location name
          monsterlist
          npclist
          descr
          alterdescr
          items
          weapons
          spells
          actions
          world)
  (let ((objects (append (map create-item-adt items)
                   (map create-weapon-adt weapons)
                   (map create-spell-adt spells))))
    (the-locations-table 'add name monsterlist npclist descr alterdescr objects actions)
    (let ((the-location (create-location-adt name)))
      (world 'insert-location the-location)
      the-location)))

(define (create-class name parameters hitpoints items weapons spells . respawn)
  (if (null? respawn)
    (the-class-table 'add name parameters hitpoints items weapons spells)
    (the-class-table 'add name parameters hitpoints items weapons spells respawn)))

(define (make-world-in world name startlocation)
  (let ((new-world (create-world-adt name startlocation)))
    (world 'insert-world new-world)
    new-world))

(define (make-road-in world from-location to-location direction . reversed)
  (if (null? reversed)
    (world 'insert-road from-location to-location direction)
    (world 'insert-road from-location to-location direction reversed)))

(define (create-quest name title description triggers heropoints)
  (the-quest-table 'add name title description triggers heropoints)
  (create-quest-adt name))

(define (create-npc name
          description
          start
          conversation
          items
          weapons
          questconditions
          quest-to-trigger)
  (the-npc-table 'add name
    description
    start
    conversation
    items
    weapons
    questconditions
    quest-to-trigger)
  (make-npc name))

(define (make-monster name class location . route)
  (if (null? route)
    (let ((the-monster (create-monster-adt name class location)))
      (the-character-table 'add the-monster)
      the-monster)
    (let ((the-monster (create-monster-adt name class location (car route))))
      (the-character-table 'add the-monster)
      the-monster)))

(define (make-npc name)
  (the-character-table 'add (create-npc-adt name)))

(define (display-icon command) ;; <<== Changed entire function.
  (define (display-pause x)
    (if (icons-on? global-options)
      (begin (display x)
        (display " "))
        'done))
  (define moveicon '<<shoes>>)
  (define fleeicon '<<witch-on-broom>>)
  (define lookicon '<<eye>>)
  (define talkicon '<<mouth>>)
  (define stealicon '<<black-gloves>>)
  (define casticon '<<scroll>>)
  (define geticon '<<yellow-gloves>>)
  (define dropicon '<<brown-gloves>>)
  (define useicon '<<potion-bottle>>)
  (define readicon '<<open-book>>)
  (define examineicon '<<open-bag>>)
  (define offenceicon '<<sword>>)
  (define defenseicon '<<shield>>)
  (define attackicon '<<morning-star>>)
  (define inventoryicon '<<chest>>)
  (define spellbookicon '<<spell-book>>)
  (define questlogicon '<<quest-scroll>>)

  (cond ((eq? command 'move) (display-pause moveicon))
    ((eq? command 'flee) (display-pause fleeicon))
    ((eq? command 'look) (display-pause lookicon))
    ((eq? command 'talk) (display-pause talkicon))
    ((eq? command 'steal) (display-pause stealicon))
    ((eq? command 'cast) (display-pause casticon))
    ((eq? command 'get) (display-pause geticon))
    ((eq? command 'drop) (display-pause dropicon))
    ((eq? command 'use) (display-pause useicon))
    ((eq? command 'read) (display-pause readicon))
    ((eq? command 'examine) (display-pause examineicon))
    ((eq? command 'equip-offence) (display-pause offenceicon))
    ((eq? command 'equip-defense) (display-pause defenseicon))
    ((eq? command 'attack) (display-pause attackicon))
    ((eq? command 'inventory) (display-pause inventoryicon))
    ((eq? command 'spellbook) (display-pause spellbookicon))
    ((eq? command 'questlog) (display-pause questlogicon))
    (else (error "Wrong Command -- Display-icon" command))))

;;All Class data is listed here
;;Expects: name parameters hitpoints items weapons spells description

(define classes-items-weapons-dummy
  (begin
    ;;Player Classes
    (create-class 'fighter '((12 . 20) (14 . 22) (5 . 8) (5 . 8) (4 . 8) (8 . 12)) '(100 . 180) '(strength-potion small-potion) '(long-sword leather-armor) '() '())
    (create-class 'priest '((6 . 14) (8 . 16) (8 . 10) (6 . 12) (14 . 22) (8 . 12)) '(55 . 150) '(wisdom-potion small-potion) '(short-staff leather-armor) '(heal-light-wounds blessing) '())
    (create-class 'monk '((12 . 20) (12 . 20) (4 . 6) (4 . 6) (4 . 8) (4 . 8)) '(40 . 150) '(wisdom-potion small-potion) '(long-staff leather-armor) '(heal-light-wounds) '())
    (create-class 'necromancer '((12 . 20) (12 . 20) (4 . 6) (4 . 6) (4 . 8) (4 . 8)) '(60 . 150) '(intelligence-potion small-potion) '(long-staff leather-armor) '(curse heal-light-wounds) '())
    (create-class 'ranger '((14 . 20) (16 . 22) (4 . 8) (4 . 6) (4 . 6) (2 . 6)) '(80 . 140) '(constitution-potion small-potion) '(bow leather-armor) '() '())
    (create-class 'wizard '((8 . 12) (10 . 16) (8 . 14) (10 . 14) (12 . 22) (6 . 12)) '(50 . 120) '(intelligence-potion small-potion) '(short-staff leather-armor) '(heal-light-wounds fireball) '())
    (create-class 'druid '((10 . 20) (10 . 20) (6 . 10) (8 . 12) (6 . 8) (14 . 22)) '(40 . 120) '(wisdom-potion small-potion) '(short-sword leather-armor) '(blessing) '())
    (create-class 'thief '((8 . 14) (12 . 16) (12 . 20) (5 . 8) (6 . 12) (2 . 6)) '(60 . 120) '(dexterity-potion small-potion) '(small-dagger leather-armor) '() '())
    (create-class 'paladin '((10 . 20) (12 . 20) (4 . 8) (6 . 12) (6 . 10) (8 . 14)) '(60 . 130) '(charisma-potion small-potion) '(long-sword shield) '(blessing) '())

    ;;Monster Classes
    (create-class 'zombie '((6 . 12) (6 . 10) (2 . 4) (1 . 6) (2 . 4) (1 . 4)) '(10 . 20) '() '(short-axe leather-armor) '() 15)
    (create-class 'zombie+ '((10 . 14) (10 . 12) (2 . 5) (1 . 8) (2 . 5) (1 . 6)) '(12 . 30) '() '(heavy-axe-++ leather-armor) '() 15)
    (create-class 'zombie++ '((12 . 18) (14 . 18) (2 . 6) (1 . 10) (2 . 6) (1 . 6)) '(16 . 34) '(large-potion) '(heavy-axe-++ helmet) '() 15)

    (create-class 'skeleton '((4 . 12) (4 . 8) (1 . 4) (1 . 4) (1 . 6) (1 . 4)) '(10 . 20) '() '(short-sword leather-armor) '() 20)
    (create-class 'skeleton+ '((6 . 14) (6 . 10) (1 . 4) (1 . 4) (1 . 6) (1 . 4)) '(14 . 24) '() '(short-sword-+ shield) '() 10)
    (create-class 'skeleton++ '((8 . 16) (8 . 12) (2 . 4) (2 . 4) (2 . 6) (2 . 4)) '(16 . 30) '() '(spear leather-armor) '() 10)

    (create-class 'vampire '((8 . 16) (2 . 8) (1 . 4) (1 . 4) (2 . 8) (4 . 10)) '(12 . 26) '() '(long-sword leather-armor) '(bloodlust) 50)
    (create-class 'aisgen-vampire '((12 . 20) (6 . 12) (1 . 2) (1 . 2) (2 . 8) (4 . 10)) '(16 . 40) '(small-potion) '(long-sword-+ cape) '(bloodlust))

    (create-class 'troll '((2 . 6) (12 . 16) (2 . 4) (2 . 4) (8 . 14) (2 . 4)) '(12 . 20) '() '(short-dagger leather-armor) '() 15)
    (create-class 'troll+ '((4 . 10) (14 . 18) (2 . 4) (2 . 4) (12 . 14) (2 . 4)) '(16 . 22) '(medium-potion) '(heavy-dagger leather-armor) '() 15)
    (create-class 'troll++ '((2 . 6) (12 . 16) (2 . 4) (2 . 4) (8 . 14) (4 . 10)) '(12 . 20) '(large-potion) '(short-dagger-+ bracers) '(shadowcast)
      15)

    (create-class 'gnoll '((4 . 10) (10 . 16) (4 . 6) (2 . 6) (6 . 10) (4 . 10)) '(8 . 18) '() '(halberd leather-armor) '())
    (create-class 'gnoll+ '((8 . 12) (12 . 16) (4 . 6) (6 . 10) (6 . 12) (4 . 10)) '(8 . 30) '() '(halberd leather-armor) '(fireball heal-light-wounds))
    (create-class 'gnoll++ '((12 . 18) (14 . 18) (2 . 6) (2 . 6) (8 . 10) (6 . 12)) '(14 . 40) '() '(spear shield) '(blessing))

    (create-class 'orc '((6 . 8) (8 . 16) (1 . 4) (1 . 6) (6 . 10) (1 . 2)) '(12 . 24) '() '(short-sword orcisch-armor) '() 30)
    (create-class 'orc+ '((10 . 14) (10 . 16) (1 . 4) (1 . 6) (8 . 10) (1 . 2)) '(16 . 30) '() '(heavy-axe-++ orcisch-armor) '() 30)
    (create-class 'orc++ '((12 . 20) (14 . 18) (2 . 4) (2 . 6) (8 . 12) (1 . 2)) '(20 . 45) '() '(short-sword-++ orcisch-armor) '(berserk) 30)
    (create-class 'orc-shaman '((10 . 14) (10 . 16) (6 . 12) (18 . 24) (6 . 8) (4 . 10)) '(20 . 35) '() '(heavy-dagger orcisch-armor)
        '(fireball heal-light-wounds berserk) 40)

    (create-class 'goblin '((2 . 4) (12 . 18) (2 . 4) (2 . 4) (14 . 24) (2 . 8)) '(6 . 26) '() '(short-sword orcisch-armor) '() 20)
    (create-class 'goblin+ '((4 . 6) (14 . 22) (4 . 6) (4 . 8) (16 . 30) (2 . 10)) '(6 . 30) '() '(short-sword-+ orcisch-armor) '() 15)
    (create-class 'goblin++ '((6 . 10) (16 . 24) (4 . 6) (6 . 8) (18 . 32) (2 . 8)) '(8 . 32) '(small-potion) '(short-axe orcisch-armor) '(heal-critical-wounds) 20)
    (create-class 'goblin-king '((8 . 10) (16 . 24) (4 . 8) (6 . 8) (30 . 60) (2 . 8)) '(8 . 32) '(small-potion) '(metal-staff goblin-kings-cape) '(heal-critical-wounds))

    (create-class 'werewolf '((18 . 24) (14 . 16) (4 . 8) (2 . 4) (14 . 24) (2 . 6)) '(12 . 30) '() '(claws leather-armor) '())
    (create-class 'greater-werewolf '((22 . 28) (16 . 18) (6 . 8) (2 . 4) (16 . 24) (2 . 6)) '(16 . 40) '() '(claws leather-armor) '())

    (create-class 'stone-golem '((18 . 24) (20 . 24) (1 . 8) (1 . 4) (4 . 8) (2 . 6)) '(30 . 60) '() '(heavy-axe-++ full-plate-armor) '(curse))
    (create-class 'clay-golem '((20 . 30) (14 . 16) (1 . 8) (1 . 4) (4 . 8) (2 . 6)) '(30 . 50) '() '(heavy-axe-++ full-plate-armor) '(judgement))

    (create-class 'dark-dwarf '((8 . 16) (14 . 24) (4 . 8) (8 . 14) (4 . 8) (2 . 6)) '(20 . 40) '(constitution-potion) '(heavy-axe-++ full-plate-armor) '() 30)
    (create-class 'drow-elf '((14 . 20) (16 . 22) (8 . 16) (12 . 16) (6 . 14) (8 . 22)) '(30 . 40) '(elven-potion) '(bow leather-armor) '() 40)
    (create-class 'elven-sorcerer '((6 . 10) (10 . 14) (10 . 12) (18 . 24) (4 . 6) (10 . 24)) '(20 . 30) '(small-potion) '(long-staff bracers) '(fireball brainstorm heal-medium-wounds lightning-bolt) 50)
    (create-class 'elven-priest '((6 . 12) (14 . 18) (18 . 32) (12 . 24) (8 . 12) (12 . 20)) '(25 . 35) '() '(long-staff cape) '(brainstorm heal-light-wounds blessing) 50)

    (create-class 'knight '((14 . 20) (18 . 24) (4 . 8) (6 . 10) (2 . 6) (10 . 16)) '(30 . 50) '() '(bastard-sword full-plate-armor) '() 60)
    (create-class 'mage '((6 . 10) (8 . 12) (6 . 12) (18 . 34) (6 . 10) (6 . 10)) '(20 . 40) '() '(metal-staff cape)
        '(icecone lightning-bolt heal-light-wounds) 60)
    (create-class 'diego '((18 . 20) (18 . 28) (4 . 8) (6 . 10) (4 . 10) (10 . 16)) '(40 . 70) '() '(diegos-dagger leather-armor) '())

    (create-class 'demon '((14 . 22) (12 . 24) (10 . 20) (10 . 20) (14 . 24) (16 . 32)) '(30 . 60) '() '(metal-staff-++ cape)
        '(magic-missile heal-critical-wounds curse))
    (create-class 'angel '((14 . 22) (12 . 24) (12 . 20) (8 . 20) (14 . 24) (16 . 32)) '(30 . 60) '() '(long-sword full-plate-armor)
        '(heal-critical-wounds judgement))
    (create-class 'imp '((6 . 6) (12 . 16) (6 . 8) (4 . 8) (4 . 6) (2 . 8)) '(6 . 26) '() '(short-staff bracers)
        '(icecone lightning-bolt))
    (create-class 'shadow '((14 . 30) (12 . 18) (2 . 4) (2 . 2) (16 . 30) (2 . 8)) '(12 . 40) '() '(claws leather-armor) '(bloodlust))

    (create-class 'dragon-welp '((18 . 24) (20 . 24) (4 . 8) (4 . 6) (12 . 20) (4 . 8)) '(35 . 60) '(dragon-water) '(claws dragon-armor) '(fireball) 200)
    (create-class 'dragon '((24 . 36) (40 . 46) (12 . 20) (8 . 16) (18 . 26) (10 . 14)) '(80 . 160) '(dragon-water) '(claws dragon-armor) '(inferno)
      250)
    (create-class 'dark-dragon '((30 . 36) (40 . 50) (14 . 28) (14 . 20) (16 . 20) (10 . 16)) '(100 . 160) '(dragon-water) '(claws dragon-armor) '(inferno)
      300)
    (create-class 'arch-dragon '((60 . 180) (50 . 80) (14 . 24) (18 . 28) (30 . 46) (12 . 18)) '(140 . 320) '(dragon-water) '(claws dragon-armor) '(inferno heal-critical-wounds))

    (display "Classes Loaded")
    (newline)

    ;;All items are listed here

    ;;create-item: name description functions extra

    ;;objects to alter stats
    (create-item 'small-potion "A green potion in a small bottle" '((hp-up . 20)) 'delete-after-use)
    (create-item 'medium-potion "A red potion in a bottle" '((hp-up . 40)) 'delete-after-use)
    (create-item 'large-potion "A blue potion in a glass bottle" '((hp-up . 60)) 'delete-after-use)
    (create-item 'super-potion "A white potion, it looks like water" '((hp-up . 80)) 'delete-after-use)

    (create-item 'constitution-potion "A Potion which strengthens your body" '((constitution-up . 3)) 'delete-after-use)
    (create-item 'dexterity-potion "A Potion which improves your agility" '((dexterity-up . 3)) 'delete-after-use)
    (create-item 'wisdom-potion "A Potion which enlightens your mind" '((wisdom-up . 3)) 'delete-after-use)
    (create-item 'intelligence-potion "A Potion which sharpens your thinking" '((intelligence-up . 3)) 'delete-after-use)
    (create-item 'strength-potion "A Potion that improves your muscles" '((strength-up . 3)) 'delete-after-use)
    (create-item 'charisma-potion "A Potion that improves your charisma" '((charisma-up . 3)) 'delete-after-use)
    (create-item 'potion-of-heroism "A Potion that gives you courage" '((heropoints-up . 30)) 'delete-after-use)

    (create-item 'potion-of-dragon-strength "A Potion that imbues you with the strength of a dragon"
        '((strength-up . 8) (constitution-up . 5)) 'delete-after-use)
    (create-item 'dragon-water "It is said dragons of ancient times washed themselves with this water"
        '((wisdom-up . 4) (intelligence-up . 4)) 'delete-after-use)
    (create-item 'elven-potion "The fluid of elves, it strengthens one, in all facets, just like the elves excell in all facets"
        '((strength-up . 2) (constitution-up . 2) (dexterity-up . 2) (charisma-up . 2) (wisdom-up . 2) (intelligence-up . 2)) 'delete-after-use)

    ;;objects related to quests
    (create-item 'old-stone "An old stone, it doesn't seem very special" '() '())
    (create-item 'a-flower "A purple flower, it smells good" '((charisma-up . 5)) 'delete-after-use)
    (create-item 'sundance-raider-book "It appears to be a book about tales from the desert" '() '())
    (create-item 'apocalypse-book "An enormeous book, guessing from the title, it must be another of one of those prophecy books" '() '())
    (create-item 'old-stone "An old stone, it doesn't seem very special" '() '())
    (create-item 'dragons-tail "The tail of the archdragon, it's green, and it's besmeared with blood. The spike on the end is lethal" '() '())
    (create-item 'dragons-treasure "The famous dragon treasure, more wealth than anyone can ever imagine. All gold an jewelry"
        '((heropoints-up . 20) (dexterity-down . 5)) '())
    (create-item 'ancient-tome "An old dusty tome, arcane signs are subscripted in it. It's all very...old" '() '())
    (create-item 'metal "Strong metal, and shiny too, it doesn't seem to rust that quick" '() '())
    (create-item 'keys "A golden ring with a set of keys tied to it" '() '())
    (create-item 'map-of-mountainvale "A map of the world" '() '())

    (display "Items Loaded")
    (newline)

    ;;All weapon data is listed here

    ;;expects name description statistics bonus extra
    ;;statistics is a list of: (number of dice . number of sides of the dice)

    ;;swords
    (create-weapon 'long-sword "A shiny long sword" '((2 . 10) (2 . 10) 15 5) '() 'none)
    (create-weapon 'broad-sword "A rusty broad sword" '((2 . 8) (2 . 12) 5 10) '() 'none)
    (create-weapon 'short-sword "A short but swift long sword" '((1 . 8) (1 . 8) 4 2) '() 'none)
    (create-weapon 'bastard-sword "A sword that requires some strength to handle" '((3 . 8) (2 . 8) 15 5) '() 'none)

    (create-weapon 'short-sword-+ "A better version of the short sword" '((2 . 8) (2 . 8) 8 4) '() 'none)
    (create-weapon 'long-sword-+ "A better version of the long sword" '((3 . 10) (3 . 10) 30 10) '() 'none)
    (create-weapon 'bastard-sword-+ "A better version of the bastard sword" '((2 . 10) (2 . 10) 15 5) '() 'none)
    (create-weapon 'broad-sword-+ "A better version of the broad sword" '((3 . 8) (3 . 12) 10 20) '() 'none)
    (create-weapon 'scimitar "An arabian sword" '((2 . 8) (3 . 10) 4 12) '() 'none)

    (create-weapon 'short-sword-++ "The best non-magical short sword there is" '((2 . 12) (2 . 12) 14 10) '() 'none)
    (create-weapon 'long-sword-++ "The best non-magical long sword there is" '((3 . 14) (3 . 14) 40 20) '() 'none)
    (create-weapon 'bastard-sword-++ "The best non-magical bastard sword there is" '((2 . 14) (2 . 14) 20 10) '() 'none)
    (create-weapon 'broad-sword-++ "The best non-magical broad sword there is" '((3 . 12) (3 . 14) 15 30) '() 'none)

    (create-weapon 'sword-of-fire "The sword is constantly burning" '((3 . 12) (3 . 12) 14 10) '((cast . fireball)) 'none)
    (create-weapon 'sword-of-ice "The sword has a blue color, it feels cold" '((3 . 12) (3 . 12) 14 10) '((cast . icecone)) 'none)
    (create-weapon 'sword-of-heroes "A sword used by many heroes, perhaps you're the next one?" '((3 . 6) (3 . 6) 20 8) '((strength-up . 20)) 'none)
    (create-weapon 'orc-reaper "A sword forged by Dwarves in their war against the orcs" '((3 . 12) (3 . 14) 15 30) '((damage-orc . 80)) 'none)
    (create-weapon 'sword-of-doom
      "A sword carried by the Elven knights in their war against the humans" '((5 . 8) (4 . 4) 35 20) '((cast . doom)) 'delete-after-use)
    (create-weapon 'excalibur "Legend has it that this sword was worn by a legendary king" '((3 . 12) (3 . 12) 4 10) '() 'none)
    (create-weapon 'sword-of-violence "A malificent sword, sharp and edgy" '((3 . 12) (1 . 6) 14 10) '() 'none)
    (create-weapon 'sword-of-chaos "A strange sword, be wary on how to use it" '((8 . 10) (1 . 6) 30 2) '((cast . teleport)) 'delete-after-use)
    (create-weapon 'elven-sword "A wooden sword made by elves, do not underestimate it" '((1 . 12) (5 . 12) 8 16) '((cast . shield)) 'none)
    (create-weapon 'defenders-mythe "The sword of true defenders" '((2 . 12) (6 . 12) 6 40) '((cast . shield)) 'none)

    ;;axes
    (create-weapon 'short-axe "A short and fragile axe" '((2 . 4) (1 . 12) 4 6) '() 'none)
    (create-weapon 'large-axe "A large, heavy axe" '((3 . 4) (1 . 12) 8 4) '() 'none)
    (create-weapon 'double-axe "Looking at this weapon gives you the creeps" '((6 . 4) (1 . 12) 14 4) '() 'none)

    (create-weapon 'short-axe-+ "A short and fragile axe, improved version" '((2 . 6) (1 . 14) 6 8) '() 'none)
    (create-weapon 'large-axe-+ "An improved large axe" '((3 . 6) (1 . 14) 12 4) '() 'none)
    (create-weapon 'double-axe-+ "A fierce weapon" '((6 . 6) (1 . 12) 18 6) '() 'none)

    (create-weapon 'short-axe-++ "The best non-magical short axe there is" '((3 . 6) (2 . 4) 8 10) '() 'none)
    (create-weapon 'large-axe-++ "The best non-magical large axe there is" '((4 . 6) (2 . 6) 14 6) '() 'none)
    (create-weapon 'double-axe-++ "The best non-magical double axe there is" '((8 . 6) (2 . 8) 20 8) '() 'none)
    (create-weapon 'heavy-axe-++ "The best non-magical heavy axe there is" '((10 . 8) (2 . 10) 18 6) '() 'none)

    (create-weapon 'short-axe-of-venom "A deadly, venomous weapon" '((3 . 6) (2 . 4) 8 10) '((cast . venom)) 'none)
    (create-weapon 'wanderers-axe "A swift axe, yet powerful" '((4 . 6) (3 . 6) 10 12) '((dext-up . 30)) 'delete-after-use)
    (create-weapon 'evils-eye "An axe used for rituals by Priests of Lech'nun" '((3 . 8) (1 . 6) 22 4) '((cast . summon-imp)) 'delete-after-use)
    (create-weapon 'axe-of-power "The ultimate axe, yet veary heavy to the wielder" '((6 . 22) (2 . 4) 50 4) '((strength-up . 40) (dext-down . 10)) 'none)
    (create-weapon 'dwarven-axe "Your typical dwarven axe" '((4 . 8) (3 . 4) 10 8) '((constitution-up . 20)) 'none)

    ;;staffs
    (create-weapon 'short-staff "A short wooden staff" '((1 . 8) (1 . 10) 6 6) '((hp-up . 30)) 'delete-after-use)
    (create-weapon 'wooden-staff "A wooden staff" '((1 . 10) (1 . 10) 4 6) '((hp-up . 30)) 'delete-after-use)
    (create-weapon 'long-staff "A long wooden staff" '((1 . 10) (1 . 12) 4 8) '((hp-up . 30)) 'delete-after-use)
    (create-weapon 'metal-staff "A short wooden staff" '((1 . 12) (2 . 6) 10 8) '((hp-up . 30)) 'delete-after-use)

    (create-weapon 'short-staff-+ "A better version of the short wooden staff" '((1 . 8) (1 . 10) 6 6) '((hp-up . 50) (cast . shield)) 'delete-after-use)
    (create-weapon 'long-staff-+ "A better version of the long wooden staff" '((1 . 10) (1 . 12) 4 8) '((hp-up . 50) (cast . shield)) 'delete-after-use)
    (create-weapon 'metal-staff-+ "A better version of the short wooden staff" '((1 . 12) (2 . 6) 10 8) '((hp-up . 50) (cast . shield)) 'delete-after-use)

    (create-weapon 'mystic-staff "A long, shiny staff" '((2 . 8) (2 . 10) 6 6) '((cast . teleport)) 'none)
    (create-weapon 'ice-staff "A frozen magical staff" '((2 . 10) (2 . 12) 4 8) '((cast . icecone)) 'none)
    (create-weapon 'fire-staff "The staff is constantly on fire" '((2 . 12) (2 . 12) 4 8) '((cast . fire)) 'none)
    (create-weapon 'wind-staff "A very long metallic staff" '((2 . 8) (2 . 10) 10 6) '((cast . tornado)) 'none)
    (create-weapon 'staff-of-nature "A long wooden staff, there are leaves growing on the staff" '((2 . 10) (1 . 12) 6 8) '((cast . force-of-nature)) 'none)
    (create-weapon 'staff-of-death "A black staff, made of a material you do not know" '((1 . 12) (4 . 6) 4 16) '((cast . summon-demon)) 'none)
    (create-weapon 'staff-of-elders "An old staff, weird-shaped in the form of an S" '((4 . 4) (4 . 4) 4 4) '((hp-up . 20)) 'none)
    (create-weapon 'gods-finger "The staff is completely white, and almost doens't weigh anything" '((2 . 22) (2 . 12) 10 22) '((cast . doom)) 'none)
    (create-weapon 'Arachronox "A very small staff, yet its powers are amazing" '((8 . 4) (12 . 2) 10 8) '((cast . shield) (cast . vision)) 'none)
    (create-weapon 'Archmaster "A metallic staff with golden encryptions" '((1 . 16) (4 . 8) 12 10)
        '((hp-up . 10) (strength-up . 10) (constitution-up . 10) (dext-down . 10)) 'none)
    (create-weapon 'Angel-whisper "A white staff, weird symbols are encrtypted " '((6 . 6) (8 . 6) 4 12) '((cast . summon-angel)) 'none)

    ;;Daggers
    (create-weapon 'small-dagger "A small, but sharp dagger" '((1 . 4) (1 . 8) 4 6) '() 'none)
    (create-weapon 'heavy-dagger "A large dagger" '((2 . 4) (2 . 8) 4 6) '() 'none)

    (create-weapon 'small-dagger-+ "A better version of the small dagger" '((2 . 4) (2 . 8) 8 4) '() 'none)
    (create-weapon 'heavy-dagger-+ "A better version of the heavy dagger" '((6 . 4) (4 . 4) 8 4) '() 'none)

    (create-weapon 'venomous-dagger "A small dagger, used with poision" '((1 . 4) (1 . 8) 4 6) '((cast . poison)) 'delete-after-use)
    (create-weapon 'magical-dagger "A blue glooming dagger" '((2 . 4) (2 . 8) 4 6) '((hp-up . 5)) 'none)
    (create-weapon 'dagger-of-sacrifice "A dagger, smeared with blood, in the shape of an S" '((4 . 4) (4 . 6) 10 4) '((cast . summon-zombie)) 'none)
    (create-weapon 'dagger-of-thieves "This dagger is known well in thieve guilds" '((4 . 4) (2 . 8) 10 14) '((dext-up . 20)) 'none)

    ;;Miscellaneous
    (create-weapon 'halberd "A large rusty Halberd" '((2 . 8) (1 . 8) 8 6) '() 'none)
    (create-weapon 'bow "A wooden bow, with arrows" '((1 . 8) (1 . 4) 10 2) '() 'none)
    (create-weapon 'spear "A long wooden spear, with a metal arrow at the end" '((2 . 8) (2 . 6) 6 6) '() 'none)
    (create-weapon 'claws "sharp claws of an animal" '((5 . 4) (3 . 4) 6 4) '() 'none)
    (create-weapon 'goblin-kings-cape "The red cape of the goblin king" '((5 . 4) (4 . 4) 6 10) '() 'none)
    (create-weapon 'diegos-dagger "The dagger of the Thief Diego" '((8 . 4) (3 . 4) 4 2) '() 'none)

    ;;Defense
    (create-weapon 'leather-armor "Normal leather armor" '((1 . 4) (2 . 8) 2 10) '() 'none)
    (create-weapon 'studded-armor "Normal studded armor" '((1 . 4) (2 . 10) 2 12) '() 'none)
    (create-weapon 'metal-plate-armor "Normal metal plate armor" '((1 . 4) (3 . 10) 2 14) '() 'none)
    (create-weapon 'scale-mail-armor "Normal scale mail armor" '((1 . 4) (2 . 10) 2 20) '() 'none)

    (create-weapon 'ring-mail-armor "Ring mail armor covering the entire body" '((1 . 4) (3 . 8) 2 10) '() 'none)
    (create-weapon 'full-plate-armor "Full plate mail, very solid" '((1 . 4) (4 . 8) 2 20) '() 'none)
    (create-weapon 'heavy-armor "Heavy leather armor" '((1 . 4) (2 . 8) 4 20) '() 'none)

    (create-weapon 'shield "A shield" '((2 . 4) (2 . 8) 4 20) '() 'none)
    (create-weapon 'helmet "A solid helmet" '((1 . 4) (1 . 6) 2 20) '() 'none)
    (create-weapon 'cape "A long mantle" '((1 . 4) (2 . 6) 2 16) '() 'none)
    (create-weapon 'bracers "Large Wrist Bracers" '((2 . 4) (2 . 6) 2 12) '() 'none)

    (create-weapon 'dragon-armor "This armor is made from the skin of a dragon" '((2 . 6) (4 . 12) 6 20) '() 'none)
    (create-weapon 'baldurans-armor "Armor worn by the hero Balduran" '((2 . 4) (4 . 10) 2 20) '((constitution-up . 10) (dext-down . 10)) 'none)
    (create-weapon 'orcisch-armor "Armor worn by the Orcs, it stinks" '((1 . 4) (2 . 6) 4 6) '() 'none)

    (display "Weapons Loaded")
    (newline)))

;;All specific spell casting actions are listed here

(define (cast-doom caster)
  ;;casts the spell doom
  (let ((target (select-target-in-room caster 'all))
         (damage (cons 2 4)))
    (if target
      (begin (strength-down target damage)
        (dexterity-down target damage)
        (constitution-down target damage)
        (charisma-down target damage)
        (display (get-character-name target))
        (display-line " is doomed")
        (let ((undo-action (create-action-adt 'undo-doom-effects target '()))
               (run-time (core-eventhandler 'give-runtime)))
          (core-eventhandler 'add undo-action (+ run-time 30)))))))

(define (cast-judgement caster)
  ;;casts the spell judgement
  (let* ((target (select-target-in-room caster 'all))
          (caster-charisma (get-character-charisma caster))
          (target-charisma (get-character-charisma target))
          (damage (- caster-charisma target-charisma)))
    (if target
      (if (< damage 0)
        (begin (display (get-character-name target))
          (display-line "has been judged positive"))
        (begin (strength-down target damage)
          (dexterity-down target damage)
          (constitution-down target damage)
          (charisma-down target damage)
          (display (get-character-name target))
          (display-line " is judged negative")
          (let ((undo-action (create-action-adt 'undo-doom-effects
                               target
                               (list damage)))
                 (run-time (core-eventhandler 'give-runtime)))
            (core-eventhandler 'add undo-action (+ run-time 30))))))))

(define (cast-curse caster)
  ;;casts the spell curse
  (let ((target (select-target-in-room caster 'all))
         (damage (round (/ (get-character-wisdom caster) 3))))
    (if target
      (begin (intelligence-down target damage)
        (wisdom-down target damage)
        (display (get-character-name target))
        (display-line " is cursed")
        (let ((undo-action (create-action-adt 'undo-curse-effects
                             target
                             (list damage)))
               (run-time (core-eventhandler 'give-runtime)))
          (core-eventhandler 'add undo-action (+ run-time 30)))))))

(define (summon caster creature-type)

  (define (conditions-fulfilled? player creature type)
    (let* ((the-player (get-character-adt player))
            (player-strength (get-character-strength the-player))
            (player-wisdom (get-character-strength the-player))
            (player-intelligence (get-character-strength the-player))
            (player-charisma (get-character-strength the-player))
            (creature-strength (get-character-strength creature))
            (creature-charisma (get-character-strength creature))
            (creature-wisdom (get-character-wisdom creature))
            (creature-intelligence (get-character-intelligence creature))
            (greater-strength? (> creature-strength player-strength))
            (greater-charisma? (> creature-charisma player-charisma))
            (greater-intelligence? (> creature-intelligence player-intelligence))
            (greater-wisdom? (> creature-wisdom player-wisdom)))
      (cond ((eq? type 'demon)
              (if (and greater-strength? greater-charisma?)
                (begin (combat creature caster) ;;demon turns agains his owner
                  #f)
                #t))
        ((eq? type 'angel)
          (if (and greater-strength? greater-charisma?)
            #f
            #t))
        ((eq? type 'imp)
          (if greater-charisma?
            #f
            #t))
        ((eq? type 'dragon)
          (if (and greater-strength?
                greater-charisma?
                greater-intelligence?
                greater-wisdom?)
            (begin (combat creature caster) ;;dragon turns againts his owner
              #f)
            (if (and greater-strength? greater-intelligence?)
              #f
              #t)))
        ((eq? type 'shadow)
          (if greater-wisdom?
            #f
            #t))
        (else #t))))

  (let* ((summon-location (get-character-location caster))
          (summoned-creature (make-monster 'summoned-monster creature-type summon-location))
          (unsummon-action (create-action-adt 'unsummon-monster
                               'summoned-monster
                             (list summoned-creature)))
          (run-time (core-eventhandler 'give-runtime)))
    (display "You summoned a ")
    (display-line creature-type)
    (display-line "Who do you want it to attack?")
    (let ((target (select-target-in-room caster 'all)))
      (if target
        (if (conditions-fulfilled? caster summoned-creature creature-type)
          ;;some creatures require conditions to be fulfilled
          (begin (combat summoned-creature target) ;;engage in combat
            (core-eventhandler 'add unsummon-action (+ run-time 5))
            (display-line "Creature will be unsummoned in 5 turns"))
          (display-line "Cannot summon: conditions for summoning such creature not fulfilled"))))))

(define (cast-force-drop caster)
  (let ((target (select-target-in-room caster 'all)))
    (if target
      (begin (drop-all target)
        (display-line "Your force drop spell was effective")))))

(define (cast-terror caster)
  ;;casts the spell terror
  (let ((target (select-target-in-room caster 'all))
         (damage (get-character-intelligence caster)))
    (if target
      (begin (strength-down target damage)
        (dexterity-down target damage)
        (constitution-down target damage)
        (charisma-down target damage)
        (intelligence-down target damage)
        (wisdom-down target damage)
        (drop-all target)
        (display (get-character-name target))
        (display-line " is terrorized")
        (let ((undo-action (create-action-adt 'undo-terror-effects
                             target
                             (list damage)))
               (run-time (core-eventhandler 'give-runtime)))
          (core-eventhandler 'add undo-action (+ run-time 40)))))))

;;undo effects:
(define (undo caster type)
  (let ((undo-action (create-action-adt type (get-character-name caster) '()))
         (run-time (core-eventhandler 'give-runtime)))
    (core-eventhandler 'add undo-action (+ run-time 30))))

(define (undo-doom-effects target)
  (strength-up target '(2 . 4))
  (dexterity-up target '(2 . 4))
  (constitution-up target '(2 . 4))
  (charisma-up target '(2 . 4))
  (display "Doom effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-judgement-effects target damage)
  (strength-up target damage)
  (dexterity-up target damage)
  (constitution-up target damage)
  (charisma-up target damage)
  (display "Judgement effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-curse-effects target damage)
  (intelligence-up target damage)
  (wisdom-up target damage)
  (display "Curse effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-terror-effects target damage)
  (strength-up target damage)
  (dexterity-up target damage)
  (constitution-up target damage)
  (charisma-up target damage)
  (intelligence-up target damage)
  (wisdom-up target damage)
  (display "Terror effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (unsummon-monster summoned-creature-name summoned-creature)
  (let ((monster-name (get-character-name summoned-creature)))
    (display "Unsummoning ")
    (display-line (get-character-class summoned-creature))
    (summoned-creature 'set-status! 'dead)
    (core-eventhandler 'delete-all monster-name)
    (the-character-table 'delete monster-name)))

(define (undo-blessing-effects target)
  (strength-down target '(2 . 4))
  (dexterity-down target '(2 . 4))
  (display "Blessing effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-greater-blessing-effects target)
  (strength-down target '(2 . 4))
  (dexterity-down target '(2 . 4))
  (wisdom-down target '(3 . 6))
  (display "Greater blessing effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-brainstorm-effects target)
  (intelligence-down target '(2 . 6))
  (wisdom-down target '(2 . 6))
  (display "Brainstorm effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-bloodlust-effects target)
  (strength-down target '(4 . 10))
  (constitution-up target '(4 . 10))
  (display "Bloodlust effects on ")
  (display (get-character-name target))
  (display-line " undone"))

(define (undo-berserk-effects target)
  (strength-down target '(4 . 10))
  (hp-up target '(3 . 10))
  (display "Berserk effects on ")
  (display (get-character-name target))
  (display-line " undone"))

;;All spell data is listed here
;;create-spell expects name spelld castd actions nr-of-casts reset-time

;;for reading comfort and convenience, the actions of the spells are defined seperately

;;the actions
(define heal-light-actions '((hp-up . (2 . 6))))
(define heal-medium-actions '((hp-up . (3 . 6))))
(define heal-large-actions '((hp-up . (4 . 6))))
(define heal-critical-actions '((hp-up . (6 . 6))))

(define blessing-actions '((strength-up . (2 . 4)) (dexterity-up . (2 . 4)) (undo . undo-blessing-effects)))
(define greater-blessing-actions '((strength-up . (2 . 6)) (dexterity-up . (2 . 6)) (wisdom-up . (3 . 6)) (undo . undo-greater-blessing-effects)))

(define doom-actions '((cast-doom)))
(define judgement-actions '((cast-judgement)))
(define curse-actions '((cast-curse)))

(define magic-missile-actions '((deal-x-damage-to-target . (3 . 8))))
(define brainstorm-actions '((wisdom-up . (2 . 6)) (intelligence-up . (2 . 6)) (undo . undo-brainstorm-effects)))

(define cast-demon-actions '((summon . demon)))
(define cast-angel-actions '((summon . angel)))
(define cast-imp-actions '((summon . imp)))
(define cast-dragon-actions '((summon . dragon)))
(define cast-shadow-actions '((summon . shadow)))

(define fireball-actions '((deal-x-damage-to-target . (3 . 6))))
(define lightning-bolt-actions '((deal-x-damage-to-target . (4 . 6))))
(define icecone-actions '((deal-x-damage-to-target . (2 . 9))))
(define icecone-actions '((deal-x-damage-to-target . (4 . 10))))

(define force-drop-actions '((cast-force-drop)))

(define bloodlust-actions '((strength-up . (4 . 10)) (constitution-down . (4 . 10)) (undo . undo-bloodlust-effects)))
(define berserk-actions '((strength-up . (4 . 10)) (hp-down . (3 . 10)) (undo . undo-berserk-effects)))

(define terror-actions '((cast-terror)))
(define inferno-actions '((deal-x-damage-to-target . (6 . 12))))

;;the spells
(define spells-dummy
  (begin
    (create-spell 'heal-light-wounds "Healing spell, heals a small amount of hitpoints" "A small blue vapor heals your wounds" heal-light-actions 4 20)
    (create-spell 'heal-medium-wounds "Healing spell, heals a medium amount of hitpoints" "A blue vapor heals your wounds" heal-medium-actions 3 30)
    (create-spell 'heal-large-wounds "Healing spell, heals a good amount of hitpoints" "A blue light flickers and heals your wounds" heal-large-actions 2 40)
    (create-spell 'heal-critical-wounds "Healing spell, heals most of your hitpoints" "A white light heals your wounds" heal-critical-actions 1 50)

    (create-spell 'blessing "Increases your strength and dexterity" "A ligth from the heavens strikes down, immediately you feel enlightened"
      blessing-actions 2 50)
    (create-spell 'greater-blessing "Increases your strength, dexterity and wisdom" "A bright aura surrounds you, you feel your strength increasing"
      greater-blessing-actions 1 50)

    (create-spell 'doom "Dooms an enemy, it lowers his stats" "A black cloud appears over the head of your target" doom-actions 2 30)
    (create-spell 'judgement "Judges an enemy by his charisma, it lowers his stats" "A Judgement hammer swings above the head of your target"
      judgement-actions 1 40)
    (create-spell 'curse "Curses your enemy, lowering his wisdom and intelligence" "Your opponent grows silent and dumb as you cast the spell"
      curse-actions 2 30)

    (create-spell 'magic-missile "A missile dealing 3d8 damage" "A shiny flaming arrow shoots forth from your hand" magic-missile-actions 2 80)
    (create-spell 'brainstorm "Increased intelligence and wisdom" "Your eyes start to flicker" brainstorm-actions 2 50)

    (create-spell 'summon-demon "Casts a demon that will aid, or destroy you"
      "As you cast the spell, the ground trembles and a fierce demon arises from the ground" cast-demon-actions 1 60)
    (create-spell 'summon-angel "Casts an angel, if you're lucky"
      "As you cast the spell, the heavens clear and an Angel flies down from the skies" cast-angel-actions 1 60)
    (create-spell 'summon-imp "Casts an imp, if he likes you"
      "As you cast the spell, a small little imp materialises before you" cast-imp-actions 1 50)
    (create-spell 'summon-dragon "Casts the most dangerous creature in the world, hard to control"
      "Suddenly, you hear the flapping of giant wings and the stench of sulfur, an enormous red dragon comes flying in from the horizon"
      cast-dragon-actions 1 500)
    (create-spell 'shadowcast "Casts a shadow attacking a target" "Your hands produce pure blackness... living blackness" cast-shadow-actions 1 80)

    (create-spell 'fireball "Casts a fireball dealing 3d6 damage" "A blazing fireball shoots from your fingertips" fireball-actions 2 30)
    (create-spell 'icecone "Casts a blue icepeak dealing 2d9 damage" "A blue peak slowly forms in your hands, then, you swing it at your target"
      icecone-actions 2 30)
    (create-spell 'lightning-bolt "Casts a lightning spark dealing 4d10 damage"
      "At the tips of your fingers, an electric charce forms and shoots at your target" lightning-bolt-actions 2 60)

    (create-spell 'force-drop "lets your target drop all of his possesions"
      "As you cast your spell, your opponents possesions grow very heavy, causing him to drop them all" force-drop-actions 1 50)

    (create-spell 'bloodlust "You gain an enormeous strength, but lose a lot of your constitution" "As you cast the spell, your hunger for blood increases"
      bloodlust-actions 2 60)
    (create-spell 'berserk "You gain an enormeous strength, but lose some Healthpoints, be careful"
      "As you cast the spell, you feel like you could slaughter an army" berserk-actions 2 60)

    (create-spell 'terror "You totally terrorize your opponent" "As you speak the spell words, your opponent gets scared, and starts to scream"
      terror-actions 1 100)
    (create-spell 'inferno "All goes up in fire with this one" "After the casting, fire is flowing from the core of the world itself, everything is burning"
      inferno-actions 1 200)

    (display "Spells Loaded")
    (newline)))

;;All worlds are listed here

;;The global world:
(define the-world (create-world-adt 'the-world 'warp-room))
(define warp-room (create-location 'warp-room '() '() "The Warproom"
                    "You start out in the warp room, select an exit" '() '() '() '() the-world))
(the-world 'set-startlocation! warp-room)

;;All major places in the world:
(define x 'temporary-placeholder)
(define northern-plains (make-world-in the-world 'northern-plains x))
(define dwarven-keep (make-world-in the-world 'dwarven-keep x))
(define wizard-school (make-world-in the-world 'wizard-school x))
(define dragonvale (make-world-in the-world 'dragonvale x))
(define goblin-encampment (make-world-in the-world 'goblin-encampment x))
(define elven-fortress (make-world-in the-world 'elven-fortress x))
(define dragons-island (make-world-in the-world 'dragons-island x))
(define ancient-graveyard (make-world-in the-world 'ancient-graveyard x))
(define temple-of-lune (make-world-in the-world 'temple-of-lune x))
(define athela (make-world-in the-world 'athela x))
(define ithilion (make-world-in the-world 'ithilion x))

(define forests-of-echnun (create-location 'forests-of-echnun '(a-goblin a-gnoll) '(an-elf)
                            "The lurky forests of Echnun, no one comes here nowadays, not in the least because of its danger"
                            "The trees are very dark here, yet, the leafs are green and all seems quiet"
                              '(small-potion constitution-potion) '(wooden-staff) '() '(monster-attack) the-world))

;;The interconnections (roads) between the worlds:
(define world-roads-dummy
  (begin
    (make-road-in the-world northern-plains wizard-school 'east 'reversed)
    (make-road-in the-world northern-plains dwarven-keep 'south 'reversed)
    (make-road-in the-world wizard-school forests-of-echnun 'east 'reversed)
    (make-road-in the-world wizard-school dragonvale 'south 'reversed)
    (make-road-in the-world forests-of-echnun athela 'south 'reversed)
    (make-road-in the-world dwarven-keep dragonvale 'east 'reversed)
    (make-road-in the-world dwarven-keep goblin-encampment 'south 'reversed)
    (make-road-in the-world dragonvale dragons-island 'south 'reversed)
    (make-road-in the-world dragonvale athela 'east 'reversed)
    (make-road-in the-world dragons-island ithilion 'east 'reversed)
    (make-road-in the-world dragons-island temple-of-lune 'south 'reversed)
    (make-road-in the-world goblin-encampment dragons-island 'east 'reversed)
    (make-road-in the-world goblin-encampment elven-fortress 'south 'reversed)
    (make-road-in the-world athela ithilion 'south 'reversed)
    (make-road-in the-world temple-of-lune ancient-graveyard 'east 'reversed)
    (make-road-in the-world ithilion ancient-graveyard 'south 'reversed)
    (make-road-in the-world elven-fortress temple-of-lune 'east 'reversed)

    (make-road-in the-world warp-room northern-plains 'west)
    (make-road-in the-world warp-room ithilion 'south)
    (make-road-in the-world warp-room forests-of-echnun 'east)))

;;the exits
(define exit-east (create-location 'exit-east '(east) '() " " " " '() '() '() '() the-world))
(define exit-west (create-location 'exit-west '(west) '() " " " " '() '() '() '() the-world))
(define exit-north (create-location 'exit-north '(north) '() " " " " '() '() '() '() the-world))
(define exit-south (create-location 'exit-south '(south) '() " " " " '() '() '() '() the-world))

(define (distribute-exits)
  ;;exits must be present in each world
  (define (distribute-loop worldlist)
    (if (null? worldlist)
        'done
      (begin ((car worldlist) 'insert-location exit-east)
        ((car worldlist) 'insert-location exit-west)
        ((car worldlist) 'insert-location exit-north)
        ((car worldlist) 'insert-location exit-south)
        (distribute-loop (cdr worldlist)))))
  (distribute-loop (list northern-plains
                     dwarven-keep
                     wizard-school
                     dragonvale
                     goblin-encampment
                     elven-fortress
                     dragons-island
                     ancient-graveyard
                     temple-of-lune
                     athela
                     ithilion)))

(define exits-dummy
  (begin
    (distribute-exits)

    (display "World Set")
    (newline)))

;;all locations of the world are listed here

;;create-location: name monsterlist npclist descr alterdescr items weapons spells actions world

;;Northern Plains
(define small-pool (create-location 'small-pool '() '(old-man) "A small pool" "You don't see anyone or anything around except for an old man" '() '() '() '() northern-plains))
(define tree-forest (create-location 'tree-forest '(drow-elf) '(nymf) "The trees are dense here, it's dark" "A nymf is standing by a tree" '() '(shield) '() '(monster-attack) northern-plains))
(define great-plain (create-location 'great-plain '() '(elven-ranger1 elven-ranger2) "The wide open plains, everything is quit there" "Nothing much to see, a few elven scouts are patrolling in the area" '(small-potion) '() '() '() northern-plains))
(define old-stone-cave (create-location 'old-stone-cave '() '(old-sage) "An old dark little cave, some daylight shines through" "An old sage with a long beard stares at you" '(old-stone) '() '() '() northern-plains))

(define northern-roads-dummy
  (begin
    (make-road-in northern-plains small-pool tree-forest 'north 'reversed)
    (make-road-in northern-plains small-pool great-plain 'west 'reversed)
    (make-road-in northern-plains great-plain old-stone-cave 'north 'reversed)
    (make-road-in northern-plains tree-forest old-stone-cave 'west 'reversed)

    (make-road-in northern-plains small-pool exit-south 'south)
    (make-road-in northern-plains tree-forest exit-east 'east)))

;;Forests of Echnun
;;See Mountainvale World

;;Wizard School
(define apprentices-square (create-location 'apprentices-square '() '(jolo apprentice) "A little square, some apprentices wander around" "An Idyllic sight, those apprentices have a nice backyard" '() '() '() '() wizard-school))
(define the-laboratory (create-location 'the-laboratory '(clay-golem) '() "On the left, some potions are warmed up, a lot of equipment is stored here, in the middle of the room, a large container is broken." "The rooms are dusty, you don't see any wizards"
                           '(super-potion intelligence-potion) '(broad-sword-++) '() '(monster-attack) wizard-school))
(define wizard-school-dorms (create-location 'wizard-school-dorms '() '(mage1 mage2) "The Wizard School Dormitory" "The beds seem ok, in front of each bed stands a coffin, probably full of books" '() '() '() '() wizard-school))
(define stairway (create-location 'stairway '() '() "A small stairway" "It's drafty in here" '() '() '() '() wizard-school))
(define the-garden (create-location 'the-garden '() '(azatar) "The Wizard School Gardens" "Lots of flowers, a few benches, and a wizard looking at you" '(a-flower) '() '() '() wizard-school))
(define masters-square (create-location 'masters-square '() '(master-wizard1 master-wizard2) "This Square is obviously larger than the Apprentices Square" "This is where the master wizards gather" '() '() '() '() wizard-school))
(define headmasters-tower (create-location 'headmasters-tower '() '(headmaster) "The Headmasters quarters. High and Dry." "Nice vieuw of the surrounding area... The headmaster appears busy" '() '() '() '() wizard-school))
(define basement (create-location 'basement '() '() "The Basement, what would they keep in here? Not much it seems." "Except for the cold temperature, nothing draws your attention." '() '(metal-staff-+) '() '() wizard-school))
(define wizard-school-library (create-location 'wizard-school-library '() '() "The Wizard School Library, the place to be if you are in search of books" "You'd expect more wizards in the library, its calm"
                                  '(sundance-raider-book apocalypse-book medium-potion) '()
                                  '(bloodlust summon-imp blessing) '() wizard-school))

(define wizard-roads-dummy
  (begin
    (make-road-in wizard-school apprentices-square the-laboratory 'east 'reversed)
    (make-road-in wizard-school apprentices-square stairway 'south 'reversed)
    (make-road-in wizard-school the-laboratory wizard-school-dorms 'east 'reversed)
    (make-road-in wizard-school the-laboratory the-garden 'south 'reversed)
    (make-road-in wizard-school stairway headmasters-tower 'south 'reversed)
    (make-road-in wizard-school the-garden masters-square 'east 'reversed)
    (make-road-in wizard-school the-garden basement 'south 'reversed)
    (make-road-in wizard-school masters-square wizard-school-library 'south 'reversed)
    (make-road-in wizard-school basement wizard-school-library 'east 'reversed)
    (make-road-in wizard-school headmasters-tower basement 'east 'reversed)
    (make-road-in wizard-school wizard-school-dorms masters-square 'south 'reversed)

    (make-road-in wizard-school apprentices-square exit-west 'west)
    (make-road-in wizard-school masters-square exit-east 'east)
    (make-road-in wizard-school wizard-school-library exit-south 'south)))

;;Dwarven Keep
(define dwarven-tavern (create-location 'dwarven-tavern '() '(drunken-dwarf dwarven-ward) "The Dwarven Tavern is the most crowded place in this entire Keep, it figures." "The Dwarves don't pay attention to you, they're too busy drinking and joking"
                           '(small-potion constitution-potion) '() '() '() dwarven-keep))
(define dwarven-keep-gates (create-location 'dwarven-keep-gates '() '(dwarven-guard-1 dwarven-guard-2) "The wooden gates that lead inward the Dwarven Keep" "Two Guards secure the entrance, they seem to be sleeping, but you can never be sure about that with dwarves" '() '() '() '() dwarven-keep))
(define hyagars-smythe (create-location 'hyagars-smythe '() '(hyagar) "The Dwarven Smythe... its HOT" "Hyagar is busy melting some old swords" '() '(double-axe-++) '(lightning-bolt) '() dwarven-keep))
(define dwarven-workshop (create-location 'dwarven-workshop '() '(dwarven-alchemist dwarven-carpenter) "The noise hurts your ears, everywhere dwarves are busy building tools" "The constant ticking of the hammers and the scratching of the saws makes you crazy" '()
                             '(heavy-axe-++) '(doom) '() dwarven-keep))
(define dwarven-caverns (create-location 'dwarven-caverns '(dwarven-warrior-1) '() "The Dwarven caverns, where dwarves feel at home" "Cold, dark and long hallways, just as the dwarves like them" '() '() '() '(monster-attack) dwarven-keep))
(define dwarven-weaponry (create-location 'dwarven-weaponry '(dwarven-warrior-2 dwarven-warrior-3) '() "The Dwarven weaponry, this is where they keep their famous axes" "The Room is loaded with weapons, all sorts of them, the Dwarven Smith must have his work"
                             '() '(dwarven-axe short-sword-++ orc-reaper fire-staff scale-mail-armor)
                             '(fireball) '(multiple-monster-attack) dwarven-keep))
(define dwarven-kings-room (create-location 'dwarven-kings-room '() '(king-keldorn) "Dwarven King Keldorn's palace" "The King is sitting on a throne in the middle of the room, it's strange there aren't any guards around." '() '(sword-of-chaos) '() '() dwarven-keep))

(define dwarven-roads-dummy
  (begin
    (make-road-in dwarven-keep dwarven-tavern dwarven-keep-gates 'east 'reversed)
    (make-road-in dwarven-keep dwarven-keep-gates dwarven-caverns 'south 'reversed)
    (make-road-in dwarven-keep dwarven-keep-gates hyagars-smythe 'east 'reversed)
    (make-road-in dwarven-keep dwarven-workshop dwarven-caverns 'east 'reversed)
    (make-road-in dwarven-keep dwarven-caverns dwarven-weaponry 'east 'reversed)
    (make-road-in dwarven-keep dwarven-caverns dwarven-kings-room 'south 'reversed)

    (make-road-in dwarven-keep dwarven-keep-gates exit-north 'north)
    (make-road-in dwarven-keep dwarven-weaponry exit-east 'east)
    (make-road-in dwarven-keep dwarven-kings-room exit-south 'south)))

;;Dragonvale
(define town-gates (create-location 'town-gates '() '(dragonvale-guard-1 dragonvale-guard-2) "The entrance to the town of Dragonvale" "Two Soldiers guard the small arc" '() '(long-sword-+) '() '() dragonvale))
(define dragon-museum (create-location 'dragon-museum '() '(supervisor) "This is the old museum, famous for it's collection of Dragon pieces" "The supervisor is keeping an eye on you"
                          '(potion-of-dragon-strength) '(dragon-armor claws) '() '() dragonvale))
(define lord-caevens-home (create-location 'lord-caevens-home '() '(lord-caeven) "The Lord's estate is quite impressive" "The rooms are nicely decorated, and the furniture is quite fancy." '() '() '() '() dragonvale))
(define governors-mansion (create-location 'governors-mansion '() '(the-governor) "The Mansion of the Governor of this province of Mountainvale" "The Governor is seated at his desk, with piles of paper in front of him" '(map-of-mountainvale) '() '() '() dragonvale))
(define dragons-fountain (create-location 'dragons-fountain '() '(a-dragonvale-commoner) "In the middle of the square stands a beautiful fountain." "The fountain is shaped like a dragon and water is flowing from its beak"
                             '(dragon-water) '(short-axe-++) '() '() dragonvale))
(define dragonvale-tavern (create-location 'dragonvale-tavern '(dragonvale-knight) '(a-drunk local-dragonvale-idiot dragonvale-inn-ward)
                            "A nice little tavern. Known for it's Dragon Ale" "Some townsfolk is socialising. The usual drunk is seated at the bar, and some idiot is trying to get some attention." '(small-potion) '() '() '() dragonvale))
(define yeruns-house (create-location 'yeruns-house '() '(yerun-the-dragonslayer) "Yerun's house isn't very large, but it is nicely decorated." "Everywhere you see scalps of Yerun's quests: dragon's wings, claws... even a dragon's eye" '(medium-potion)
                         '(long-sword-++ shield helmet) '() '() dragonvale))

(define dragonvale-roads-dummy
  (begin
    (make-road-in dragonvale town-gates dragon-museum 'east 'reversed)
    (make-road-in dragonvale town-gates governors-mansion 'south 'reversed)
    (make-road-in dragonvale dragon-museum dragons-fountain 'south 'reversed)
    (make-road-in dragonvale dragon-museum lord-caevens-home 'east 'reversed)
    (make-road-in dragonvale governors-mansion dragons-fountain 'east 'reversed)
    (make-road-in dragonvale dragons-fountain dragonvale-tavern 'east 'reversed)
    (make-road-in dragonvale dragons-fountain yeruns-house 'south 'reversed)

    (make-road-in dragonvale town-gates exit-west 'west)
    (make-road-in dragonvale town-gates exit-north 'north)
    (make-road-in dragonvale lord-caevens-home exit-east 'east)
    (make-road-in dragonvale dragonvale-tavern exit-east 'east)
    (make-road-in dragonvale yeruns-house exit-south 'south)))

;;Dragons Island
(define northern-beach (create-location 'northern-beach '(dark-dragon) '() "The Northern beach lies just behind a large cliff wall" "No one around here"
                           '(strength-potion dexterity-potion intelligence-potion) '(claws) '() '() dragons-island))
(define southern-beach (create-location 'southern-beach '(dragon-welp) '() "The Southern beach is less rocky than it's northern brother, still, quite an obscure beach" "No one to see"
                           '(strength-potion dexterity-potion wisdom-potion) '() '() '() dragons-island))
(define dragonlair (create-location 'dragonlair '(archdragon) '() "The lair of the most fiersome weapon in the land" "as your eyes grow weary of the darkness, you see an enormeous dragon lying on a huge treasure"
                       '(dragons-tail dragons-treasure) '(archmaster) '() '(monster-attack) dragons-island))

(define dragons-roads-dummy
  (begin
    (make-road-in dragons-island northern-beach dragonlair 'south 'reversed)
    (make-road-in dragons-island dragonlair southern-beach 'south 'reversed)

    (make-road-in dragons-island northern-beach exit-west 'west)
    (make-road-in dragons-island northern-beach exit-north 'north)
    (make-road-in dragons-island northern-beach exit-east 'east)
    (make-road-in dragons-island southern-beach exit-west 'west)
    (make-road-in dragons-island southern-beach exit-south 'south)
    (make-road-in dragons-island southern-beach exit-east 'east)))

;;Athela
(define city-gates (create-location 'city-gates '() '(athelian-guard-1 athelian-guard-2 gatekeeper) "The impressive entrance to the City of Athela" "The typical 2 guards stand next to the door" '() '(long-staff-+) '() '() athela))
(define athela-fountains (create-location 'athela-fountains '() '(athelian-commoner-1 athelian-commoner-2 lord-esthart)
                           "A series of beautiful fountains dominate the square" "Some commoners are wandering about" '(small-potion intelligence-potion) '(small-dagger) '() '() athela))
(define teraks-store (create-location 'teraks-store '() '(terak) "Terak's store: the best potions in the land" "Terak seems busy brewing some potions"
                         '(medium-potion dexterity-potion strength-potion)
                         '(long-sword-++ heavy-axe-++ short-sword-++ long-staff-+ heavy-dagger-+ metal-staff-+) '() '() athela))
(define courtyard (create-location 'courtyard '() '(nobleman noblewoman) "The athelian Courtyard, meeting place for the noble" "Some noble personae wandering about" '() '(cape) '(greater-blessing) '() athela))
(define athelian-market (create-location 'athelian-market '()
                            '(athelian-commoner-3 athelian-commoner-4 athelian-merchant zirtan) "The market, it's quite crowded, the merchants are having a good day" "A few commoners are discussing about some bought wares"
                            '(small-potion large-potion potion-of-heroism) '(large-axe-++ spear bow) '(heal-critical-wounds) '() athela))
(define athelian-tavern (create-location 'athelian-tavern '(athelian-knight) '(athelian-ward hislo) "The Tavern of Athela, the ward is serving some people" "not much people around here, they're all out at the market" '(small-potion) '() '() '() athela))
(define king-dahriens-castle (create-location 'king-dahriens-castle '()
                                 '(king-dahrien) "The castle of the King of the land" "It's your typical medieval Castle" '() '() '() '() athela))
(define athelian-barracks (create-location 'athelian-barracks '(captain-gerard athelian-soldier-1 athelian-soldier-2 athelian-mage-soldier)
                              '() "The atelian barracks, dorms for the soldiers" "Some beds, a table, and some weapons is all you see" '(strength-potion constitution-potion)
                              '(helmet shield spear long-sword-++ broad-sword-++) '(lightning-bolt) '(multiple-monster-attack) athela))
(define dyeraks-home (create-location 'dyeraks-home '() '(dyerak) "The house of an ordinary commoner" "Dyerak's home is nothing more than a little cabin in an obscure street" '() '() '() '() athela))
(define city-walls (create-location 'city-walls '() '(old-mage) "The City walls, they surround the entire city" "The walls are about 12 feet thick, large enough to stop a goblin raid" '() '(long-staff-+) '(terror judgement) '() athela))

(define athela-roads-dummy
  (begin
    (make-road-in athela city-gates athela-fountains 'south 'reversed)
    (make-road-in athela athela-fountains athelian-market 'south 'reversed)
    (make-road-in athela athelian-market courtyard 'west 'reversed)
    (make-road-in athela athelian-market athelian-tavern 'east 'reversed)
    (make-road-in athela king-dahriens-castle athelian-barracks 'east 'reversed)
    (make-road-in athela courtyard king-dahriens-castle 'south 'reversed)
    (make-road-in athela athelian-barracks dyeraks-home 'east 'reversed)
    (make-road-in athela athelian-tavern teraks-store 'north 'reversed)
    (make-road-in athela athelian-barracks city-walls 'south 'reversed)
    (make-road-in athela athelian-market athelian-barracks 'south 'reversed)

    (make-road-in athela city-gates exit-west 'west)
    (make-road-in athela city-gates exit-north 'north)
    (make-road-in athela city-walls exit-west 'west)
    (make-road-in athela city-walls exit-south 'south)))

;;Goblin Encampment
(define yislings-cave (create-location 'yislings-cave '() '(yisling) "A dark cave, in the middle, a pot is boiling on top of a fire" "A little goblin mage is brewing some redish drink in the pot" '() '() '() '() goblin-encampment))
(define goblin-barracks (create-location 'goblin-barracks '(goblin-warrior-1 goblin-warrior-2 orcish-warrior-1)
                            '() "The goblin army headquarters" "the walls are besmeared with blood, and everywhere on the floor lie bones and weapons" '() '(short-axe) '() '(multiple-monster-attack) goblin-encampment))
(define goblin-tavern (create-location 'goblin-tavern '(goblin-warrior-3 goblin-warrior-4) '() "The goblin tavern, meeting place for the goblins" "Not your typical Human Tavern"
                          '() '(short-staff) '() '(multiple-monster-attack) goblin-encampment))
(define goblin-warrens (create-location 'goblin-warrens
                           '(goblin-warrior-5) '() "The goblin warrens, sort of like a children's creche" "The warrens seem abandoned" '() '(short-sword-+) '() '(monster-attack) goblin-encampment))
(define goblin-kings-palace (create-location 'goblin-kings-palace
                                '(goblin-king goblin-warrior-6 goblin-warrior-7 goblin-warrior-8 orcish-warrior-2 orcish-warrior-3)
                                '() "The residence of the Goblin King" "It's not the kind of 'palace' humans are used to" '() '(gods-finger) '(summon-imp) '(multiple-monster-attack) goblin-encampment))

(define goblin-roads-dummy
  (begin
    (make-road-in goblin-encampment yislings-cave goblin-tavern 'south 'reversed)
    (make-road-in goblin-encampment goblin-barracks goblin-tavern 'east 'reversed)
    (make-road-in goblin-encampment goblin-tavern goblin-warrens 'east 'reversed)
    (make-road-in goblin-encampment goblin-kings-palace goblin-tavern 'north 'reversed)

    (make-road-in goblin-encampment yislings-cave exit-north 'north)
    (make-road-in goblin-encampment goblin-warrens exit-east 'east)
    (make-road-in goblin-encampment goblin-kings-palace exit-south 'south)))

;;Ithilion
(define gates-of-ithla (create-location 'gates-of-ithla '(evil-mage) '(ithilian-guard-1 ithilian-guard-2) "The gates of the city of Trade" "Although this city seems to be quite safe... looks can be deceiving"
                           '(wisdom-potion) '(heavy-dagger) '() '(monster-attack) ithilion))
(define ithilian-barracks (create-location 'ithilian-barracks '() '(captain-iomund) "The place to be for the Ithla military" "Except for the captain, you see no soldiers around, they must be out patrolling"
                              '() '(spear) '() '() ithilion))
(define city-keepers-mansion (create-location 'city-keepers-mansion '() '(city-keeper) "The mansion of the head of the city" "By the looks of his house, this man must be VERY rich"
                                 '(intelligence-potion) '() '() '() ithilion))
(define trademeet (create-location 'trademeet '() '(market-seller-1 market-seller-2) "The core of trading Ithla" "As always, trademeet is crowded, merchants are screaming and customers are bidding for the lowest prices"
                      '(small-potion) '(bow) '(magic-missile) '() ithilion))
(define commerce-district (create-location 'commerce-district '() '(market-seller-3 market-seller-4) "The commercer district, here, the goods are made" "Everyone is quite busy fabricating their wares to sell at the market"
                              '() '(ice-staff) '(heal-large-wounds) '() ithilion))
(define ithilian-market (create-location 'ithilian-market '() '(market-seller-5 peasant customer) "The greatest market in the land" "More people screaming about, telling their wares are the best and the cheapest."
                            '(large-potion metal) '(fire-staff) '() '() ithilion))
(define diegos-house (create-location 'diegos-house '(diego) '() "A typical thieve's house" "In a corner you see a large bag, must be one of his loots"
                         '(small-potion) '() '() '(monster-attack) ithilion))

(define ithilion-roads-dummy
  (begin
    (make-road-in ithilion gates-of-ithla trademeet 'south 'reversed)
    (make-road-in ithilion city-keepers-mansion trademeet 'east 'reversed)
    (make-road-in ithilion trademeet commerce-district 'east 'reversed)
    (make-road-in ithilion trademeet ithilian-market 'south 'reversed)
    (make-road-in ithilion ithilian-market diegos-house 'east 'reversed)
    (make-road-in ithilion ithilian-barracks commerce-district 'south 'reversed)

    (make-road-in ithilion gates-of-ithla exit-north 'north)
    (make-road-in ithilion gates-of-ithla exit-west 'west)
    (make-road-in ithilion ithilian-market exit-south 'south)))

;;Elven Fortress
(define elven-woods (create-location 'elven-woods '(drow-elve elven-warrior) '() "The elven woods, the darkest yet most beautiful woods" "Between the trees, you see the Fortress, completely made of wood"
                        '() '() '(summon-angel) '(multiple-monster-attack) elven-fortress))
(define elven-tower (create-location 'elven-tower '() '(irgis) "The elven tower is used as a lookout, orcish troops are always about" "The tower is a solid wooden construction, like everything else in the fortress" '() '() '() '() elven-fortress))
(define elven-barracks (create-location 'elven-barracks '() '(elven-scout) "The barracks of the elven scouts" "The walls are decorated with animal skins"
                           '() '(halberd bow) '() '() elven-fortress))
(define elven-dorms (create-location 'elven-dorms '() '(eregion) "The elven dormitory, the beds are all empty" "An elf is sitting at a table, reading a book"
                        '(intelligence-potion wisdom-potion dexterity-potion)
                        '(elven-sword) '() '() elven-fortress))

(define fortress-roads-dummy
  (begin
    (make-road-in elven-fortress elven-woods elven-tower 'east 'reversed)
    (make-road-in elven-fortress elven-tower elven-barracks 'east 'reversed)
    (make-road-in elven-fortress elven-tower elven-dorms 'south 'reversed)

    (make-road-in elven-fortress elven-tower exit-north 'north)
    (make-road-in elven-fortress elven-barracks exit-east 'east)))

;;Temple of Lune
(define lune-fountain (create-location 'lune-fountain '() '() "The lune fountain" "The water in the lune fountain is the clearest you've ever seen" '(potion-of-heroism) '() '() '() temple-of-lune))
(define lune-altar (create-location 'lune-altar '(elven-priest-1 stone-golem) '() "The lune altar" "The priests of Lune are not known for their friendy behaviour, as you have noticed" '() '() '() '(monster-attack) temple-of-lune))
(define mage-conclave (create-location 'mage-conclave '(elven-sorcerer-1 elven-sorcerer-2)
                          '() "The mage conclave is a meeting place for elven sorcerers" "The sorcerers aren't very happy with unexpected visitors" '(wisdom-potion) '(mystic-staff staff-of-nature)
                          '(shadowcast) '(multiple-monster-attack) temple-of-lune))
(define elven-throne (create-location 'elven-throne '(elven-priest-2) '(elven-king) "The elven throne, the throne of the Elven King" "The king seems to be in deep thoughts" '(elven-potion) '() '() '() temple-of-lune))

(define temple-roads-dummy
  (begin
    (make-road-in temple-of-lune lune-fountain lune-altar 'south 'reversed)
    (make-road-in temple-of-lune lune-altar mage-conclave 'east 'reversed)
    (make-road-in temple-of-lune lune-altar elven-throne 'south 'reversed)

    (make-road-in temple-of-lune lune-fountain exit-north 'north)
    (make-road-in temple-of-lune lune-fountain exit-west 'west)
    (make-road-in temple-of-lune lune-fountain exit-east 'east)))

;;Ancient Graveyard
(define tombstones (create-location 'tombstones '(zombie-1 zombie-2 vampire-1)
                       '() "A large number of tombstones" "This place is creepy" '() '(metal-plate-armor) '() '(multiple-monster-attack) ancient-graveyard))
(define chapel (create-location 'chapel '(zombie-3 vampire-2 skeleton-1 skeleton-2)
                   '() "A small chapel, the stairs lead down to an underground passage" "You hear strange voices as you let your eyes get used to the darkness" '() '() '(summon-demon) '(multiple-monster-attack) ancient-graveyard))
(define unholy-graves (create-location 'unholy-graves '(aisgen-vampire-1 aisgen-vampire-2 aisgen-vampire-3)
                          '() "The Unholy Graves" "You seem to have disturbed a group of vampires" '(ancient-tome) '(wind-staff) '() '(multiple-monster-attack) ancient-graveyard))

(define graveyard-roads-dummy
  (begin
    (make-road-in ancient-graveyard tombstones chapel 'south 'reversed)
    (make-road-in ancient-graveyard chapel unholy-graves 'east 'reversed)

    (make-road-in ancient-graveyard tombstones exit-north 'north)
    (make-road-in ancient-graveyard tombstones exit-west 'west)))

;;Set the startlocations
(define init-locations-dummy
  (begin
    (northern-plains 'set-startlocation! small-pool)
    (wizard-school 'set-startlocation! apprentices-square)
    (dwarven-keep 'set-startlocation! dwarven-keep-gates)
    (dragonvale 'set-startlocation! town-gates)
    (dragons-island 'set-startlocation! northern-beach)
    (athela 'set-startlocation! city-gates)
    (goblin-encampment 'set-startlocation! goblin-tavern)
    (ithilion 'set-startlocation! gates-of-ithla)
    (elven-fortress 'set-startlocation! elven-woods)
    (temple-of-lune 'set-startlocation! lune-fountain)
    (ancient-graveyard 'set-startlocation! tombstones)

    (display "Locations Initialised")
    (newline)))

;;The actions table
(define init-actions-dummy
  (begin
    (the-actions-table 'add 'show-targets-in-room show-targets-in-room)
    (the-actions-table 'add 'hp-up hp-up)
    (the-actions-table 'add 'heropoints-up heropoints-up)
    (the-actions-table 'add 'strength-up strength-up)
    (the-actions-table 'add 'dexterity-up dexterity-up)
    (the-actions-table 'add 'constitution-up constitution-up)
    (the-actions-table 'add 'wisdom-up wisdom-up)
    (the-actions-table 'add 'intelligence-up intelligence-up)
    (the-actions-table 'add 'charisma-up charisma-up)
    (the-actions-table 'add 'hp-down hp-down)
    (the-actions-table 'add 'strength-down strength-down)
    (the-actions-table 'add 'dexterity-down dexterity-down)
    (the-actions-table 'add 'constitution-down constitution-down)
    (the-actions-table 'add 'wisdom-down wisdom-down)
    (the-actions-table 'add 'intelligence-down intelligence-down)
    (the-actions-table 'add 'charisma-down charisma-down)
    (the-actions-table 'add 'deal-x-damage-to-target deal-x-damage-to-target)
    (the-actions-table 'add 'display-spell display-spell)
    (the-actions-table 'add 'monster-travelling monster-travelling)
    (the-actions-table 'add 'monster-attack monster-attack)
    (the-actions-table 'add 'multiple-monster-attack multiple-monster-attack)
    (the-actions-table 'add 'report-combat report-combat)
    (the-actions-table 'add 'deal-x-damage-to deal-x-damage-to)
    (the-actions-table 'add 'reset-spell reset-spell)
    (the-actions-table 'add 'exit-world exit-world)
    (the-actions-table 'add 'move move)
    (the-actions-table 'add 'combat combat)
    (the-actions-table 'add 'steal steal)
    (the-actions-table 'add 'flee flee)
    (the-actions-table 'add 'look look)
    (the-actions-table 'add 'get get)
    (the-actions-table 'add 'drop drop)
    (the-actions-table 'add 'erase erase)
    (the-actions-table 'add 'get-all get-all)
    (the-actions-table 'add 'drop-all drop-all)
    (the-actions-table 'add 'equip-offence equip-offence)
    (the-actions-table 'add 'equip-defense equip-defense)
    (the-actions-table 'add 'teleport teleport)
    (the-actions-table 'add 'converse converse)
    (the-actions-table 'add 'cast cast)
    (the-actions-table 'add 'use use)
    (the-actions-table 'add 'player-read player-read)
    (the-actions-table 'add 'player-examine player-examine)
    (the-actions-table 'add 'show-status show-status)
    (the-actions-table 'add 'show-inventory show-inventory)
    (the-actions-table 'add 'show-spellbook show-spellbook)
    (the-actions-table 'add 'show-questlog show-questlog)
    (the-actions-table 'add 'show-exits show-exits)
    (the-actions-table 'add 'show-actions show-actions)
    (the-actions-table 'add 'cancel-actions cancel-actions)
    (the-actions-table 'add 'undo undo)
    (the-actions-table 'add 'respawn-monster respawn-monster)
    (the-actions-table 'add 'undo-blessing-effects undo-blessing-effects)
    (the-actions-table 'add 'cast-doom cast-doom)
    (the-actions-table 'add 'cast-judgement cast-judgement)
    (the-actions-table 'add 'cast-curse cast-curse)
    (the-actions-table 'add 'summon summon)
    (the-actions-table 'add 'cast-force-drop cast-force-drop)
    (the-actions-table 'add 'cast-terror cast-terror)
    (the-actions-table 'add 'undo-doom-effects undo-doom-effects)
    (the-actions-table 'add 'undo-judgement-effects undo-judgement-effects)
    (the-actions-table 'add 'undo-curse-effects undo-curse-effects)
    (the-actions-table 'add 'undo-terror-effects undo-terror-effects)
    (the-actions-table 'add 'unsummon-monster unsummon-monster)
    (the-actions-table 'add 'undo-blessing-effects undo-blessing-effects)
    (the-actions-table 'add 'undo-greater-blessing-effects undo-greater-blessing-effects)
    (the-actions-table 'add 'undo-brainstorm-effects undo-brainstorm-effects)
    (the-actions-table 'add 'undo-bloodlust-effects undo-bloodlust-effects)
    (the-actions-table 'add 'undo-berserk-effects undo-berserk-effects)

    (display "Actions loaded")
    (newline)))

;;these functions are necessary to determine whether
;;the player fullfilled the actions
;;for a given quest

(define (player-posseses key object)
  (if (eq? key 'spell)
    (lambda (player)
      ;;all actions eventually returned must take one parameter: a player
      (give-spell-from-character object player))
    (lambda (player)
      (give-item-from-character object player))))

(define (player-posseses-item item)
  (player-posseses 'item item))

(define (player-posseses-weapon item)
  (player-posseses 'weapon item))

(define (player-posseses-spell item)
  (player-posseses 'spell item))

(define (player-has-stats key amount)
  (cond ((eq? key 'strength)
          (lambda (player)
            (> (get-character-strength player) amount)))
    ((eq? key 'constitution)
      (lambda (player)
        (> (get-character-constitution player) amount)))
    ((eq? key 'dexterity)
      (lambda (player)
        (> (get-character-dexterity player) amount)))
    ((eq? key 'wisdom)
      (lambda (player)
        (> (get-character-wisdom player) amount)))
    ((eq? key 'intelligence)
      (lambda (player)
        (> (get-character-intelligence player) amount)))
    ((eq? key 'charisma)
      (lambda (player)
        (> (get-character-charisma player) amount)))
    ((eq? key 'heropoints)
      (lambda (player)
        (> (get-character-heropoints player) amount)))
    (else (error "Wrong key -- Player Has Stats" key))))

(define (more-strength-than amount)
  (player-has-stats 'strength amount))

(define (more-constitution-than amount)
  (player-has-stats 'constitution amount))

(define (more-wisdom-than amount)
  (player-has-stats 'wisdom amount))

(define (more-intelligence-than amount)
  (player-has-stats 'intelligence amount))

(define (more-dexterity-than amount)
  (player-has-stats 'dexterity amount))

(define (more-charisma-than amount)
  (player-has-stats 'charisma amount))

(define (more-heropoints-than amount)
  (player-has-stats 'heropoints amount))

;;All NPC's are listed here
;;create-npc: name description start conversation items weapons questconditions quest-to-trigger

(define init-npc-dummy
  (begin
    ;;Northern Plains
    (create-npc 'old-man "An old man" small-pool "Please, sir, i've lost a most precious stone, please find it" '() '() (list (player-posseses-item 'old-stone)) 'old-man-stone)
    (create-npc 'nymf "A woodnymf" tree-forest "Hello, traveller, make sure you don't get lost in the woods" '() '() '() 'none)
    (create-npc 'elven-ranger1 "An elven ranger" tree-forest "Greetings" '() '(short-sword) '() 'none)
    (create-npc 'elven-ranger2 "Another elven ranger" tree-forest "We're patrolling the area, nowadays, the orcs and goblins are everywhere" '(small-potion) '(short-staff) '() 'none)
    (create-npc 'old-sage "An old man with a large white beard" old-stone-cave "Traveller, do you see that stone, it might not seem special to you, but looks can be deceiving" '() '(wooden-staff) '() 'none)

    ;;Forests of Echnun
    (create-npc 'an-elf "An elven warrior" forests-of-echnun "Greetings. I need your help, I lost my sword, called The Sword of Doom, i really need it, i'd appreciate it if you'd find it." '() '(short-sword-+) (list (player-posseses-weapon 'sword-of-doom)) 'elf-and-sword)

    ;;Wizard School
    (create-npc 'jolo "A young apprentice" apprentices-square "Hello. You seem like a hero te me, perhaps you could help me. I'm currently researching a strange culture, and in order to proceed with my studies, I need two objects: a scimitar, and a book, called sundance raiders, could you get that for me?" '() '()
      (list (player-posseses-item 'sundance-raider-book) (player-posseses-weapon 'scimitar)) 'jolo-desert)
    (create-npc 'apprentice "Just another Wizard Apprentice" apprentices-square "Good day" '() '(wooden-staff) '() 'none)
    (create-npc 'mage1 "A mage teacher at the wizard school" wizard-school-dorms "Have you seen my students?" '(wisdom-potion) '(metal-staff) '() 'none)
    (create-npc 'mage2 "Another teacher" wizard-school-dorms "My friend here has lost his students again, it's always the same... sigh" '() '(metal-staff cape) '() 'none)
    (create-npc 'azatar "A wise looking mage" the-garden "Greetings, visitor. I have a quest for you, i am in search for the hide of an Orc, for study of course, i'd appreciate you looking into the matter. Good Day." '() '(metal-staff-+) (list (player-posseses-weapon 'orcish-armor)) 'azatar-orc)
    (create-npc 'master-wizard1 "A wise wizard" masters-square "Ah, a visitor, if you're a true hero, then i might have something for you" '() '(fire-staff) (list (more-heropoints-than 200)) 'master-wizard-offer)
    (create-npc 'master-wizard2 "A master wizard" masters-square "My friend here is always on the lookout for heroes to train and students to teach" '() '(ice-staff) '() 'none)
    (create-npc 'headmaster "The headmaster of the Wizard School" headmasters-tower "Hello. I've been expecting you. My studies have lead me to the search of a book, an ancient tome, that I need for my research. Unfortunately our library doesn't posess it. I will reward you if you can find that book for me" '() '(staff-of-elders) (list (player-posseses-item 'ancient-tome)) 'headmaster-book)

    ;;Dwarven Keep
    (create-npc 'drunken-dwarf "A drunken dwarf" dwarven-tavern "lemme alone" '() '() '() 'none)
    (create-npc 'dwarven-ward "The ward of the tavern, a big dwarf" dwarven-tavern "Oi visitor, want some ale?" '() '() '() 'none)
    (create-npc 'dwarven-guard-1 "This dwarf's beard is extermely long" dwarven-keep-gates "Get going visitor" '() '(dwarven-axe shield) '() 'none)
    (create-npc 'dwarven-guard-2 "The dwarf is resting on his axe" dwarven-keep-gates "zzzzzzzz" '() '(spear scale-mail-armor) '() 'none)
    (create-npc 'hyagar "A fierce dwarf, he seems quite strong" hyagars-smythe "I'VE HAD ENOUGH. This metal is worth nothing... when are my dwarves are going to import some decent metal! I beg of you, if you ever find some good metal, be sure to send me some." '(strength-potion) '() (list (player-posseses-item 'metal)) 'hyagars-metal)
    (create-npc 'dwarven-alchemist "The local alchemist" dwarven-workshop "You need some potions visitor?" '(constitution-potion) '() '() 'none)
    (create-npc 'dwarven-carpenter "The carpenter, he's busy" dwarven-workshop "Sorry visitor, haven't got time for chit chat" '() '(leather-armor) '() 'none)
    (create-npc 'king-keldorn "The king is seated in his throne" dwarven-kings-room "Welcome to the dwarven Keep visitor. I need your help, a favor... The Orcs are slaughtering my troops... As a favor, and as a form of friendship to the elves, i want you to get me an Orc's hide. Then you shall always be welcome in my castle" '() '(dwarven-axe helmet cape small-dagger)
      (list (player-posseses-weapon 'orcish-armor)) 'keldorn-revenge)

    ;;Dragonvale
    (create-npc 'dragonvale-guard-1 "A strong guard, equipped with spear and shield" town-gates "Move along citizen" '() '(spear shield) '() 'none)
    (create-npc 'dragonvale-guard-2 "Looks just like his collegue" town-gates "..." '() '(spear shield) '() 'none)
    (create-npc 'supervisor "The Museum Supervisor" dragon-museum "Hello, visitor, take a look around, but do not touch anything" '(potion-of-heroism) '() '() 'none)
    (create-npc 'lord-caeven "A famous lord here in Dragonvale" lord-caevens-home "Good day, hero. Say... can you wield my sword? If you think you can, let me know... i'll reward the man who can wield my sword" '() '() (list (more-strength-than 80)) 'caeven-sword)
    (create-npc 'the-governor "The Governor of the province" governors-mansion "You want something sir?" '() '() '() 'none)
    (create-npc 'a-dragonvale-commoner "Just a local" dragons-fountain "Great weather isn't it?" '() '() '() 'none)
    (create-npc 'a-drunk "The typical drunk" dragonvale-tavern "...." '() '() '() 'none)
    (create-npc 'local-dragonvale-idiot "A guy in need of some attention" dragonvale-tavern "Welwelwel, what have we here? A hero. A hero, don't make me laugh" '() '() '() 'none)
    (create-npc 'dragonvale-inn-ward "The ward of the dragonvale Inn" dragonvale-tavern "Can I fetch you a drink?" '() '() '() 'none)
    (create-npc 'yerun-the-dragonslayer "The famous dragonslayer, Yerun the Great" yeruns-house "You look like an adventurer. I have a quest for you. Probably your last quest... If you can kill the Archdragon in the centre of the world, then you will become the world's most famous hero. Off you go." '(dexterity-potion) '(long-sword-++)
      (list (player-posseses-item 'map-of-dragonvale) (player-posseses-item 'dragons-tail)) 'yerun-dragon)

    ;;Athela
    (create-npc 'athelian-guard-1 "The Guard isn't looking too happy" city-gates "I'm watching you" '() '(spear shield) '() 'none)
    (create-npc 'athelian-guard-2 "Another Guard" city-gates "Don't mind him" '() '(spear shield) '() 'none)
    (create-npc 'gatekeeper "The gatekeeper of the city of Athela" city-gates "Hello, Hero, I need your help, I lost my keys, probably left them at my mate's house Dyerak. I would be grateful if you'd get them for me" '(medium-potion) '() (list (player-posseses-item 'keys)) 'gatekeeper-keys)
    (create-npc 'athelian-commoner-1 "A local commoner" athela-fountains "Good Day adventurer" '() '() '() 'none)
    (create-npc 'athelian-commoner-2 "Another local commoner" athela-fountains "Greetings" '() '() '() 'none)
    (create-npc 'lord-esthart "A noble lord from Athela" athela-fountains "Good Day. Do you think you are wise enough to handle this spell?" '(large-potion) '() (list (more-intelligence-than 40)) 'esthart-reward)
    (create-npc 'terak "A storekeeper" teraks-store "Hello customer, need somethin'?" '() '() '() 'none)
    (create-npc 'nobleman "A noble from Athela" courtyard "You're an adventurer, i don't like your kind" '() '() '() 'none)
    (create-npc 'noblewoman "Another noble" courtyard "What he said" '() '() '() 'none)
    (create-npc 'athelian-commoner-3 "Another athelian commoner" athelian-market "Sorry, no time for talkin'" '(small-potion) '() '() 'none)
    (create-npc 'athelian-commoner-4 "One of many commoners" athelian-market "Good Day" '() '() '() 'none)
    (create-npc 'athelian-merchant "An athelian merchant selling his wares" athelian-market "Hellow adventurer, take a look at my wares" '() '() '() 'none)
    (create-npc 'zirtan "A herald from the north" athelian-market "Greetings, hero, I need your assistance. My sword was stolen by an Elf, if you can bring back my Sword of Chaos I will be most grateful" '() '() (list (player-posseses-weapon 'sword-of-chaos)) 'zirtans-sword)
    (create-npc 'athelian-ward "The ward of this tavern" athelian-tavern "Good day, are you a hero? If you're a true hero, then you're quite welcome, then I can make you famous *good for my commerce gheghe*" '() '() (list (more-heropoints-than 100)) 'wards-offer)
    (create-npc 'hislo "An old adventurer" athelian-tavern "Ah a hero, I remember the days I ventured through the world. Enjoy it while it lasts." '() '() '() 'none)
    (create-npc 'king-dahrien "The king of northeastern Mountainvale" king-dahriens-castle "Hero, I need your assistance. Many have I asked, to go and eliminate my opponent, the Goblin King. Many have failed. Kill that goblin maniac and bring me his cape as evidence." '()
        '(long-sword cape) (list (player-posseses-weapon 'goblin-kings-cape)) 'dahrien-cape)
    (create-npc 'dyerak "A young man" dyeraks-home "What's your business here?" '() '() '() 'none)
    (create-npc 'old-mage "An old army mage" city-walls "The time has come for me to quit the army. I'm far too old for this job" '() '(metal-staff-+) '() 'none)

    ;;Goblin Encampment
    (create-npc 'yisling "A small goblin mage" yislings-cave "Gjee, what are you doing here in old Yisling's cave. Anyway, if you're here, can you find me a Heal Greater Wounds spell? I need it for my little drink gjegjeh" '() '() (list (player-posseses-spell 'heal-greater-wounds)) 'yisling-spell)

    ;;Ithilion
    (create-npc 'ithilian-guard-1 "An Ithilian Guard" gates-of-ithla "Welcome to the city of Ithilion." '() '(spear shield) '() 'none)
    (create-npc 'ithilian-guard-2 "Another Ithilian Guard" gates-of-ithla "I hope you have good intentions, troublemakers will be punished" '() '(short-sword helmet) '() 'none)
    (create-npc 'captain-iomund "The Captain of the Ithilian army" ithilian-barracks "Hello Adventurer, I'm in need of some assistance. As you see, all my soldiers are out patrolling. There's a bunch of thieves in the city, robbing the merchants. One of them is Diego. My men are having a hard time catching him, if you can kill him and give me his dagger, i'd be most thankful." '() '() (list (player-posseses-weapon 'diegos-dagger)) 'iomund-thief)
    (create-npc 'city-keeper "The Head of the City" city-keepers-mansion "Mmmhellow, if you want to ask me something, make an appointment, i'm rather busy at the moment" '(super-potion) '() '() 'none)
    (create-npc 'market-seller-1 "A typical market seller" trademeet "Look at these wondrous potions, buy some, they're cheap" '(medium-potion) '() '() 'none)
    (create-npc 'market-seller-2 "A typical market seller" trademeet "Want some books? I have books, or potions, or swords, you name it, I've got it!" '(wisdom-potion) '() '() 'none)
    (create-npc 'market-seller-3 "A typical market seller" commerce-district "Hey adventurer, need a new armor? Or interested in a new helmet?" '(intelligence-potion) '() '() 'none)
    (create-npc 'market-seller-4 "A typical market seller" commerce-district "Bah, I ain't sellin' anything today, lousy stinkin' customers, ain't never happy 'bout my wares" '(strength-potion) '() '() 'none)
    (create-npc 'market-seller-5 "A typical market seller" ithilian-market "Times are tough with all those thieves in the area, stealing our wares" '(dexterity-potion) '() '() 'none)
    (create-npc 'peasant "A farmer from the region" ithilian-market "I'm buying some food, that's right, the life of a farmer ain't what it used to be" '() '() '() 'none)
    (create-npc 'customer "A lady buying wares" ithilian-market "Look at these fine wares I bought today!" '() '() '() 'none)

    ;;Elven Fortress
    (create-npc 'irgis "An elven scout, spotting in the tower" elven-tower "This is an excellent outpost, you can see the entire woods from here" '() '() '() 'none)
    (create-npc 'elven-scout "An elf, getting ready to leave the fortress" elven-barracks "If you'll excuse me, I need to prepare myself for another journey" '() '() '() 'none)
    (create-npc 'eregion "An old, wise Elf" elven-dorms "It's been a long time since we've had foreign visitors. Can you do me a favor, and head to the Wizard School in the north, to get a book, called Apocalypse, I would like to study it." '() '() (list (player-posseses-item 'apocalypse-book)) 'eregion-book)

    ;;Temple Of Lune
    (create-npc 'elven-king "The king of Elves" elven-throne "Hero, you've come this far. If you can prove you are worthy of be�ng called a true hero, then I shall give you a gift, of most powerful value" '() '() (list (more-heropoints-than 1000)) 'elven-king-gift)

    (display "NPC's Loaded")
    (newline)))

;;All Monsters's are listed here
;;make-monster: name class location route

(define init-monsters-dummy
  (begin
    ;;Northern Plains
    (make-monster 'drow-elf 'drow-elf tree-forest)

    ;;Forests of Echnun
    (make-monster 'a-goblin 'goblin forests-of-echnun)
    (make-monster 'a-gnoll 'gnoll forests-of-echnun)

    ;;Wizard School
    (make-monster 'clay-golem 'clay-golem the-laboratory)

    ;;Dwarven Keep
    (make-monster 'dwarven-warrior-1 'dark-dwarf dwarven-caverns (list dwarven-caverns dwarven-tavern))
    (make-monster 'dwarven-warrior-2 'dark-dwarf dwarven-weaponry)
    (make-monster 'dwarven-warrior-3 'dark-dwarf dwarven-caverns (list dwarven-weaponry dwarven-caverns))

    ;;Dragon's Island
    (make-monster 'dragon-welp 'dragon-welp southern-beach)
    (make-monster 'dark-dragon 'dark-dragon northern-beach)
    (make-monster 'archdragon 'arch-dragon dragonlair)

    ;;Athela
    (make-monster 'athelian-knight 'knight athelian-tavern)
    (make-monster 'captain-gerard 'knight athelian-barracks)
    (make-monster 'athelian-soldier-1 'knight athelian-barracks)
    (make-monster 'athelian-soldier-2 'knight athelian-barracks)
    (make-monster 'athelian-mage-soldier 'mage athelian-barracks)

    ;;Goblin Encampment
    (make-monster 'goblin-warrior-1 'goblin+ goblin-barracks)
    (make-monster 'goblin-warrior-2 'goblin+ goblin-barracks)
    (make-monster 'orcish-warrior-1 'orc+ goblin-barracks)
    (make-monster 'goblin-warrior-3 'goblin+ goblin-tavern (list goblin-tavern goblin-barracks))
    (make-monster 'goblin-warrior-4 'goblin+ goblin-tavern (list goblin-tavern goblin-barracks))
    (make-monster 'goblin-warrior-5 'goblin++ goblin-warrens)
    (make-monster 'goblin-warrior-6 'goblin++ goblin-kings-palace)
    (make-monster 'goblin-warrior-7 'goblin++ goblin-kings-palace)
    (make-monster 'goblin-warrior-8 'goblin++ goblin-kings-palace)
    (make-monster 'goblin-king 'goblin-king goblin-kings-palace)
    (make-monster 'orcish-warrior-2 'orc++ goblin-kings-palace)
    (make-monster 'orcish-warrior-3 'orc++ goblin-kings-palace)

    ;;Ithilion
    (make-monster 'evil-mage 'mage gates-of-ithla)
    (make-monster 'diego 'diego diegos-house)

    ;;Elven Fortress
    (make-monster 'drow-elve 'drow-elf elven-woods)
    (make-monster 'elven-warrior 'drow-elf elven-woods (list elven-woods elven-tower))

    ;;Temple Of Lune
    (make-monster 'elven-priest-1 'elven-priest lune-altar)
    (make-monster 'stone-golem 'stone-golem lune-altar)
    (make-monster 'elven-sorcerer-1 'elven-sorcerer mage-conclave)
    (make-monster 'elven-sorcerer-2 'elven-sorcerer mage-conclave (list mage-conclave lune-altar))
    (make-monster 'elven-priest-2 'elven-priest elven-throne)

    ;;Ancient Graveyard
    (make-monster 'zombie-1 'zombie tombstones)
    (make-monster 'zombie-2 'zombie+ tombstones)
    (make-monster 'vampire-1 'vampire tombstones)
    (make-monster 'zombie-3 'zombie++ chapel)
    (make-monster 'vampire-2 'vampire chapel)
    (make-monster 'skeleton-1 'skeleton chapel)
    (make-monster 'skeleton-2 'skeleton+ chapel)
    (make-monster 'aisgen-vampire-1 'aisgen-vampire unholy-graves)
    (make-monster 'aisgen-vampire-2 'aisgen-vampire unholy-graves)
    (make-monster 'aisgen-vampire-3 'aisgen-vampire unholy-graves)

    (display "Monsters Loaded")
    (newline)))

;;The functions for Quests are listed here

(define (get-and-delete-object-from-player object-name get-object-fct delete-object-fct)
  (lambda (player)
    (let ((the-object (get-object-fct object-name player)))
      (if the-object
        (begin (delete-object-fct object-name player)
          the-object)
        (error "Bad Quest: Player needs object to fulfill quest" object-name)))))

(define (get-and-delete-item-from-player item-name)
  (get-and-delete-object-from-player item-name
    give-item-from-character
    delete-item-for-character))

(define (get-and-delete-spell-from-player spell-name)
  (get-and-delete-object-from-player spell-name
    give-spell-from-character
    delete-spell-for-character))

(define (give-item-from-npc-to-player item)
  (lambda (player)
    (add-item-for-character item player)))

(define (give-spell-from-npc-to-player spell)
  (lambda (player)
    (add-spell-for-character spell player)))

(define (give-item-to-npc npc item)
  (lambda (player)
    (add-item-for-character item npc)))

(define (give-spell-to-npc npc spell)
  (lambda (player)
    (add-spell-for-character spell npc)))

(define (swap-item npc item)
  (lambda (player)
    ((give-item-to-npc npc ((get-and-delete-item-from-player item) player)) player)))

(define (swap-spell npc spell)
  (lambda (player)
    ((give-spell-to-npc npc ((get-and-delete-spell-from-player spell) player)) player)))

(define (player-set-stats key amount)
  (cond ((eq? key 'hp)
          (lambda (player)
            (do-hitpoints-up amount player)))
    ((eq? key 'strength)
      (lambda (player)
        (do-strength-up amount player)))
    ((eq? key 'constitution)
      (lambda (player)
        (do-constitution-up amount player)))
    ((eq? key 'dexterity)
      (lambda (player)
        (do-dexterity-up amount player)))
    ((eq? key 'wisdom)
      (lambda (player)
        (do-wisdom-up amount player)))
    ((eq? key 'intelligence)
      (lambda (player)
        (do-intelligence-up amount player)))
    ((eq? key 'charisma)
      (lambda (player)
        (do-charisma-up amount player)))
    ((eq? key 'heropoints)
      (lambda (player)
        (do-heropoints-up amount player)))
    (else (error "Wrong key -- Player Set Stats" key))))

(define (bonus-hp amount)
  (player-set-stats 'hp amount))

(define (bonus-strength amount)
  (player-set-stats 'strength amount))

(define (bonus-constitution amount)
  (player-set-stats 'constitution amount))

(define (bonus-wisdom amount)
  (player-set-stats 'wisdom amount))

(define (bonus-intelligence amount)
  (player-set-stats 'intelligence amount))

(define (bonus-dexterity amount)
  (player-set-stats 'dexterity amount))

(define (bonus-charisma amount)
  (player-set-stats 'charisma amount))

(define (bonus-heropoints amount)
  (player-set-stats 'heropoints amount))

(define (change-conversation npc text)
  (let ((the-npc (get-character-adt npc)))
    (lambda (player)
      (the-npc 'set-conversation! text))))

(define (wards-offer) ;;specific quest test
  (lambda (player)
    (let ((heropoints (get-character-heropoints player)))
      (cond ((> heropoints 200) ((bonus-heropoints 100) player))
        ((> heropoints 500) ((bonus-heropoints 200) player))
        (else 'done)))))

;;All Quest Data is listed here

;;create-quest: name title description triggers heropoints

;;Northern Plains
(define old-man-stone (create-quest 'old-man-stone "Retrieve an old stone"
                        "You have to find an old stone. It is of some value for an old man you met in the Northern Plains"
                        (list (bonus-charisma 3) (swap-item 'old-man 'old-stone)
                          (change-conversation 'old-man "I've got my stone now, i'm very happy")) 20))

;;Forests of Echnun
(define elf-and-sword (create-quest 'elf-and-sword "Get the Elf's sword back"
                        "An Elf you met in the Forests wants his sword, sword of chaos, back"
                        (list (bonus-wisdom 3) (bonus-strength 3) (swap-item 'an-elf 'sword-of-doom)
                          (change-conversation 'an-elf "Now that i've got my precious sword back, i can finally kick Orc butt again")) 50))

;;Wizard School
(define jolo-desert (create-quest 'jolo-desert "Get some items from the far deserts"
                      "An apprentice has asked you to find a book, Sundance Raiders, and a Scimitar Sword"
                      (list (swap-item 'jolo 'scimitar) (swap-item 'jolo 'sundance-raider-book)
                        (change-conversation 'jolo "Now that i've got my items, I can start experimenting again")) 60))

(define azatar-orc (create-quest 'azatar-orc "Get an Orcish Hide for Azatar"
                     "Azatar, a master wizard from the wizard school, wants some orcish leather, for experiments no doubt"
                     (list (bonus-intelligence 3) (swap-item 'azatar 'orcish_armor)
                       (change-conversation 'azatar "Mmmm yes, interesting hide, I will start my studies immediately")) 30))

(define master-wizard-offer (create-quest 'master-wizard-offer "A master Wizard's offer"
                              "If you've become a true hero, the master wizards will reward you."
                              (list (bonus-charisma 3) (give-spell-from-npc-to-player 'doom) (give-item-from-npc-to-player 'sword-of-heroes)
                                (change-conversation 'master-wizard1 "True heroes must be rewarded.")) 20))

(define headmaster-book (create-quest 'headmaster-book "The Headmaster's book"
                          "Get an old book for the Headmaster of the Wizard School"
                          (list (bonus-intelligence 3) (swap-item 'headmaster 'ancient-tome)
                            (change-conversation 'headmaster
                              "A most interesting book, this one here, i've been looking for it for a long time")) 60))

;;Dwarven Keep
(define hyagars-metal (create-quest 'hyagars-metal "Hyagar's search for decent metal"
                        "The Dwarven Smith, Hyagar, wants you to look for some good metal for his weapons."
                        (list (swap-item 'hyagar 'metal) (give-spell-from-npc-to-player 'inferno) (give-item-from-npc-to-player 'sword-of-fire)
                          (change-conversation 'hyagar "This metal is EXCELLENT! I can finally create descent material again")) 50))

(define keldorn-revenge (create-quest 'keldorn-revenge "The Dwarven king's revenge"
                          "Keldorn, the dwarven king, wants revenge for his losses. Avenge his people by killing some Orcs"
                          (list (swap-item 'king-keldorn 'orcish-armor) (give-item-from-npc-to-player 'sword-of-ice)
                            (change-conversation 'king-keldorn "This is just the beginning. More Orc Blood will flow")) 20))

;;Dragonvale
(define caeven-sword (create-quest 'caeven-sword "Caeven's test"
                       "Only the most strengthened can wield Caeven's Sword"
                       (list (give-item-from-npc-to-player 'defenders-mythe)
                         (change-conversation 'lord-caeven "My sword now lies in the hands of a strong and mighty hero")) 10))

(define yerun-dragon (create-quest 'yerun-dragon "The Archdragon's Death"
                       "The Quest of true heroes: killing the dragon which terrorises the land"
                       (list (bonus-charisma 5) (bonus-wisdom 2) (bonus-strength 5) (give-item-from-npc-to-player 'baldurans-armor)
                         (change-conversation 'yerun-the-dragonslayer
                           "The Dragon is dead! This is truly a most glorious day, alert the heralds!")) 300))

;;Athela
(define gatekeeper-keys (create-quest 'gatekeeper-keys "Finding The Keys"
                          "The Gatekeeper of Athela wants you to look for the keys of the gates, he lost them."
                          (list (change-conversation 'gatekeeper "Ahh my keys, thank god, I can sleep safe again.")) 40))

(define esthart-reward (create-quest 'esthart-reward "Esthart's questions"
                         "All strength is not important, the smart ones will conquer"
                         (list (give-spell-from-npc-to-player 'curse)
                           (change-conversation 'lord-esthart "The cunning will conquer you all, a true hero is a wise hero.")) 20))

(define zirtans-sword (create-quest 'zirtans-sword "Zirtan lost his sword"
                        "Zirtan, a Herald residing in Athela, has lost his precious sword of chaos, stolen by an Elf"
                        (list (swap-item 'zirtan 'sword-of-chaos)
                          (change-conversation 'zirtan "If I ever find that Elf again, i'll sure rip his...")) 100))

(define wards-offer (create-quest 'wards-offer "The Athelian Ward's offer"
                      "The ward of the tavern of Athela likes heroes in his tavern."
                      (list (bonus-charisma 3) (wards-offer)
                        (change-conversation 'athelian-ward "More Heroes please...*it's good for my business*")) 100))

(define dahrien-cape (create-quest 'dahrien-cape "The Striding Kings"
                       "King Dahrien wants you to destroy his opponent, the Goblin King"
                       (list (swap-item 'king-dahrien 'goblin-kings-cape) (give-item-from-npc-to-player 'angel-whisper)
                         (change-conversation 'king-dahrien
                           "My opponent is out of the way, i'm glad at least some heroes are capable of doing the job.")) 80))

;;Goblin Encampment
(define yisling-spell (create-quest 'yisling-spell "A Goblin mage's quest"
                        "Yisling, a goblin mage, wants you to look for a certain spell, heal greater wounds"
                        (list (swap-spell 'yisling 'heal-greater-wounds) (give-spell-from-npc-to-player 'magic-missile)
                          (change-conversation 'yisling "Gyiiiiaaaaaaaaaa, this spell will make me strongggg yessss")) 30))

;;Ithilion
(define iomund-thief (create-quest 'iomund-thief "Get the Thief"
                       "Iomund wants to get rid of the Thief Diego, get his dagger to prove you killed him."
                       (list (give-spell-from-npc-to-player 'summon-imp) (give-item-from-npc-to-player 'wanderers-axe)
                         (change-conversation 'captain-iomund "This was only one of many thieves who will bite the dust")) 50))

;;Elven Fortress
(define eregion-book (create-quest 'eregion-book "Eregion's book"
                       "Eregion the Elf needs a book from the Wizard School, named Apocalypse"
                       (list (give-spell-from-npc-to-player 'force-drop) (give-item-from-npc-to-player 'excalibur)
                         (change-conversation 'eregion "Ahh, finally, some reading pleasure, most excellent literature.")) 50))

;;Temple Of Lune
(define elven-king-gift (create-quest 'elven-king-gift "The Elven King's gift"
                          "True heroes are rear these days, you think you're one?"
                          (list (give-spell-from-npc-to-player 'summon-dragon) (give-item-from-npc-to-player 'short-axe-of-venom)
                            (give-item-from-npc-to-player 'dragon-armor)
                            (change-conversation 'elven-king "A land like this needs strong heroes.")) 100))

;;The Players
(define amurath (create-player-adt 'amurath 'fighter))
(define lethnan (create-player-adt 'lethnan 'wizard))
(define yeran (create-player-adt 'yeran 'thief))

;;The character table keeps track of all the characters (NPCs, Monsters and players) in the world
(the-character-table 'add amurath)
(the-character-table 'add lethnan)
(the-character-table 'add yeran)

;;to speed up input data
(define (solve-first-quest)
  (define (break)
    (newline)
    (display "Enter something to continue ")
    (read)
    (newline))

  (display-line "This will solve the first quest, as an example of the game")
  (display-line "amurath asks locations")
  (show-exits 'amurath)
  (break)
  (display-line "amurath moves west")
  (move 'amurath 'west)
  (break)
  (display-line "amurath talks")
  (converse 'amurath)
  (break)
  (display-line "amurath now knows about the quest")
  (display-line "amurath reads the quest: ")
  (player-read 'amurath 'old-man-stone)
  (break)
  (display-line "amurath moves north")
  (move 'amurath 'north)
  (display-line "amurath engaged in combat")
  (break)
  (display-line "amurath moves west")
  (move 'amurath 'west)
  (break)
  (display-line "amurath gets the old stone")
  (get 'amurath 'old-stone)
  (break)
  (display-line "amurath travels back to the old man")
  (move 'amurath 'east)
  (break)
  (move 'amurath 'south)
  (break)
  (display-line "amurath talks to the old man again")
  (converse 'amurath)
  (break)
  (display-line "The quest is now solved")
  (show-status 'amurath))

(display "Initialising Data Done")
(newline)
(display-line "Welcome to mountainvale")
(start-the-game)