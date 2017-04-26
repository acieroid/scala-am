
(define (create-queue)
  (let ((front '())
        (rear '()))
    (define (empty?)
      (null? front))
    (define (enqueue element-list)
      (if (null? element-list)
          #t
          (begin
            (cond
              ((null? front)
               (set! front (list (car element-list)))
               (set! rear front))
              (else
                (set-cdr! rear (list (car element-list)))
                (set! rear (cdr rear))))
            (enqueue (cdr element-list)))))
    (define (dequeue)
      (if (null? front)
          (error "Can't front. The queue is empty.")
          (let ((temp (car front)))
            (set! front (cdr front))
            temp)))
    (define (serve)
      (if (null? front)
          (error "Can't serve. The queue is empty.")
          (car front)))
    (define (dispatch msg . args)
      (cond
        ((eq? msg 'empty?) (empty?))
        ((eq? msg 'enqueue) (enqueue args)) 
        ((eq? msg 'dequeue) (dequeue))
        ((eq? msg 'serve) (serve))
        (else 
          (error "unknown request -- create-queue" msg))))
    dispatch))

(define (breadth-first-traversal graph
                                 start-label
                                 make-queue-element
                                 eq-queue-element
                                 access-label
                                 action)
  (define (processed? label)
    (eq? (graph 'lookup-node-status label) 'P))
  (if (graph 'empty?)
      #f
      (let ((queue (create-queue)))
        (define (iter)
          (cond
            ((queue 'empty?) #t)
            (else
              (let ((item (queue 'dequeue)))
                (graph 'change-node-status (access-label item) 'P)
                (action item)
                (graph 'foreach-neighbour
                       (access-label item)
                       (lambda (from-label from-info to-label to-info edge-info)
                         (if (not (processed? to-label))
                             (begin
                               (graph 'change-node-status to-label 'R)
                               (queue 'enqueue (make-queue-element from-label
                                                                   from-info
                                                                   to-label
                                                                   to-info
                                                                   edge-info))))))
                (iter)))))
        (graph 'foreach-node (lambda (label info) (graph 'change-node-status label 'W)))
        (queue 'enqueue (make-queue-element #f #f start-label
                                            (graph 'lookup-node-info start-label) #f))
        (iter))))


(define belgie (create-graph eq? #f #f))
(belgie 'insert-node 'antwerpen 1)
(belgie 'insert-node 'w-vlaanderen 2)
(belgie 'insert-node 'o-vlaanderen 3)
(belgie 'insert-node 'brabant 4)
(belgie 'insert-node 'limburg 5)
(belgie 'insert-node 'henegouwen 6)
(belgie 'insert-node 'luik 7)
(belgie 'insert-node 'luxemburg 8)
(belgie 'insert-node 'namen 9)

(belgie 'insert-edge 'antwerpen 'brabant)
(belgie 'insert-edge 'antwerpen 'o-vlaanderen)
(belgie 'insert-edge 'antwerpen 'limburg)
(belgie 'insert-edge 'brabant 'o-vlaanderen)
(belgie 'insert-edge 'brabant 'limburg)
(belgie 'insert-edge 'brabant 'henegouwen)
(belgie 'insert-edge 'brabant 'luik)
(belgie 'insert-edge 'brabant 'namen)
(belgie 'insert-edge 'o-vlaanderen 'w-vlaanderen)
(belgie 'insert-edge 'o-vlaanderen 'henegouwen)
(belgie 'insert-edge 'w-vlaanderen 'henegouwen)
(belgie 'insert-edge 'henegouwen 'namen)
(belgie 'insert-edge 'namen 'luik)
(belgie 'insert-edge 'namen 'luxemburg)
(belgie 'insert-edge 'luxemburg 'luik)
(belgie 'insert-edge 'luik 'limburg)