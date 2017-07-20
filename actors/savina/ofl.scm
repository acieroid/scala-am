(define Alpha 2.0)
(define CutoffDepth (int-top))
(define Seed (int-top))
(define GridSize (int-top)) ;; was 500
(define F (* (sqrt 2) GridSize))
(define NumPoints (int-top))

(define foldl
  (lambda (f base lst)
    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
            base
            (foldl-aux (f base (car lst)) (cdr lst)))))
    (foldl-aux base lst)))
(define (filter f l)
  (if (null? l)
      l
      (if (f (car l))
          (cons (car l) (filter f (cdr l)))
          (filter f (cdr l)))))

(define (for-each f l)
  (if (null? l)
      #t
      (if (pair? l)
          (begin (f (car l)) (for-each f (cdr l)))
          (error "Cannot for-each over a non-list"))))
;; Points
(define (point x y)
  (list 'point x y))
(define (point-x p)
  (cadr p))
(define (point-y p)
  (caddr p))
(define (random-point)
  (point (* (exact->inexact (/ (random 100) 100)) GridSize)
         (* (exact->inexact (/ (random 100) 100)) GridSize)))
;; (define (find-center points)
;;   (letrec ((loop (lambda (ps sum-x sum-y)
;;                    (if (pair? ps)
;;                        (let ((point (car ps)))
;;                          (loop (cdr ps)
;;                                (+ sum-x (point-x point))
;;                                (+ sum-y (point-y point))))
;;                        (let ((num-points (length points)))
;;                          (point (/ sum-x num-points) (/ sum-y num-points)))))))
;;     (loop points 0 0)))
(define (get-distance p1 p2)
  (let ((x-diff (- (point-x p1) (point-x p2)))
        (y-diff (- (point-y p1) (point-y p2))))
    (sqrt (+ (* x-diff x-diff) (* y-diff y-diff)))))

;; Boxes
(define (box x1 y1 x2 y2)
  (list 'box x1 y1 x2 y2))
(define (box-x1 box)
  (cadr box))
(define (box-y1 box) (caddr box))
(define (box-x2 box) (cadddr box))
(define (box-y2 box)
  (cadr (cdddr box)))
(define (box-contains box p)
  (and (<= (box-x1 box) (point-x p))
       (<= (box-y1 box) (point-y p))
       (<= (point-x p) (box-x2 box))
       (<= (point-y p) (box-y2 box))))
(define (mid-point box)
  (point (exact->inexact (/ (+ (box-x1 box) (box-x2 box)) 2))
         (exact->inexact (/ (+ (box-y1 box) (box-y2 box)) 2))))

;; Positions
(define Unknown -2)
(define Root -1)
(define TopLeft 0)
(define TopRight 1)
(define BotLeft 2)
(define BotRight 3)

;; Facilities
;; (define (facility center distance max-distance points)
;;   (list 'facility center distance max-distance points))
;; (define (facility-center fac) (cadr fac))
;; ;(define (facility-distance fac) (caddr fac))
;; ;(define (facility-max-distance fac) (cadddr fac))
;; ;(define (facility-points fac) (cadr (cdddr fac)))
;; (define (facility-num-points fac)
;;   (length (facility-points fac)))
;; (define (facility-total-distance fac) (facility-distance fac))

(define producer-actor
  (a/actor "producer-actor" (consumer items-produced)
            (start ()
                   (a/send consumer customer a/self (random-point))
                   (a/become producer-actor consumer (+ items-produced 1)))
            (next-customer ()
                           (if (< items-produced NumPoints)
                               (begin
                                 (a/send consumer customer a/self (random-point))
                                 (a/become producer-actor consumer (+ items-produced 1)))
                               (begin
                                 (a/send consumer request-exit)
                                 (a/terminate))))))

(define (safely-exit parent children children-facilities facility-customers support-customers)
  ;; savina updates num-facilities, num-customers to print them, but we don't do it
  (if parent
      (let ((num-facilities (if (null? children) (+ children-facilities 1) children-facilities))
            (num-customers (+ facility-customers (length support-customers))))
        (a/send parent confirm-exit num-facilities num-customers))
      #f)
  (a/terminate))

(define (create-child self boundary position customers threshold depth local-facilities known-facilities max-depth)
  (let ((customers (filter (lambda (c) (box-contains boundary c)) customers)))
    (a/create quadrant-actor self position boundary threshold (+ depth 1)
              local-facilities known-facilities max-depth customers
              '() ;; children
              '() ;; children boundaries
              0 ;; children facilities
              0 ;; facility customers
              0 ;; terminated child count
              (foldl (lambda (c acc)
                       (+ acc (foldl (lambda (result fac)
                                       (let ((distance (get-distance fac point)))
                                         (if (< distance result)
                                             distance
                                             result)))
                                     0
                                     local-facilities)))
                     customers 0.0))))
(define quadrant-actor
  (a/actor "quadrant-actor"
           (parent position-relative-to-parent boundary threshold
                   depth local-facilities
                   known-facilities
                   max-depth-of-known-open-facility
                   customers

                   children
                   children-boundaries
                   children-facilities

                   facility-customers

                   terminated-child-count
                   total-cost)
           (customer (producer point)
                      (if (not parent) ;; done in the end of the processing in the original benchmark, but should'nt change anything
                          (a/send producer next-customer)
                          #f)
                      (if (null? children)
                          (begin
                            (let ((cost (foldl (lambda (result fac)
                                                 (let ((distance (get-distance fac point)))
                                                   (if (< distance result)
                                                       distance
                                                       result)))
                                               0
                                               local-facilities)))
                              (if (> (+ total-cost cost) threshold)
                                  ;; partition
                                  (let ((facility (mid-point bounding-box))
                                        (max-depth (max max-depth-of-known-open-facility depth)))
                                    ;; notifyParentOfFacility(...)
                                    (if parent
                                        (a/send parent facility position-relative-to-parent facility depth #t)
                                        #f)
                                    (let ((first-boundary (box (box-x1 boundary) (point-y facility) (point-x facility) (box-y2 boundary)))
                                          (second-boundary (box (point-x facility) (point-y facility) (box-x2 boundary) (box-y2 boundary)))
                                          (third-boundary (box (box-x1 boundary) (box-y1 boundary) (point-x facility) (point-y facility)))
                                          (fourth-boundary (box (point-x facility) (box-y1 boundary) (box-x2 boundary) (point-y facility))))
                                      (let ((first-child (create-child a/self first-boundary TopLeft customers threshold depth local-facilities known-facilities max-depth))
                                            (second-child (create-child a/self second-boundary TopRight customers threshold depth local-facilities known-facilities max-depth))
                                            (third-child (create-child a/self third-boundary BotLeft customers threshold depth local-facilities known-facilities max-depth))
                                            (fourth-child (create-child a/self fourth-boundary BotRight customers threshold depth local-facilities known-facilities max-depth)))
                                        (a/become quadrant-actor parent
                                                  position-relative-to-parent boundary threshold
                                                  depth local-facilities known-facilities max-depth
                                                  '() ;; no more customers, have been distributed
                                                  (list first-child second-child third-child fourth-child)
                                                  (list first-boundary second-boundary third-boundary fourth-boundary)
                                                  children-facilities
                                                  facility-customers
                                                  terminated-child-count
                                                  total-cost))))

                                  ;; else
                                  (a/become quadrant-actor parent
                                            position-relative-to-parent boundary threshold
                                            depth local-facilities known-facilities
                                            max-depth-of-known-open-facility
                                            (cons point customers)
                                            children children-boundaries children-facilities
                                            facility-customers terminated-child-count
                                            (+ total-cost cost)))))
                          (letrec ((loop (lambda (index)
                                           (if (<= index 4)
                                               (let ((loop-child-boundary (list-ref children-boundaries index)))
                                                 (if (box-contains loop-child-boundary point)
                                                     (a/send (list-ref children index) customer producer point)
                                                     (loop (+ index 1))))
                                               #t))))
                            (loop 0)
                            (a/become quadrant-actor parent
                                      position-relative-to-parent boundary threshold
                                      depth local-facilities known-facilities
                                      max-depth-of-known-open-facility
                                      customers
                                      children children-boundaries children-facilities
                                      facility-customers terminated-child-count
                                      total-cost))))
            (facility (child-pos recv-depth point from-child)
                      (if from-child
                          (let ((sibling-pos (if (= child-pos TopLeft) BotRight
                                                 (if (= child-pos TopRight) BotLeft
                                                     (if (= child-pos BotRight TopLeft) TopLeft
                                                         TopRight)))))
                            ;; notifyParentOfFacility(point, facility.depth)
                            (if parent
                                (a/send parent facility position-relative-to-parent recv-depth point #t)
                                #f)
                            ;; children(siblingPos).send(...)
                            (a/send (list-ref children sibling-pos) facility Unknown depth point #f)
                            (a/become quadrant-actor parent
                                      position-relative-to-parent boundary threshold
                                      depth (cons point local-facilities) (+ known-facilities 1)
                                      (if (> recv-depth max-depth-of-known-open-facility) recv-depth max-depth-of-known-open-facility)
                                      customers
                                      children children-boundaries children-facilities
                                      facility-customers terminated-child-count
                                      total-cost))
                          (begin
                            (for-each (lambda (loop-child) (a/send loop-child facility Unknown depth point #f)) children)
                                (a/become quadrant-actor parent
                                          position-relative-to-parent boundary threshold
                                          depth (cons point local-facilities) (+ known-facilities 1)
                                          max-depth-of-known-open-facility
                                          customers
                                          children children-boundaries children-facilities
                                          facility-customers terminated-child-count
                                          total-cost)
                            )))
            (request-exit ()
                          (if (null? children)
                              (begin
                                (for-each (lambda (loop-child) (a/send loop-child request-exit)) children)
                                (a/become quadrant-actor parent
                                      position-relative-to-parent boundary threshold
                                      depth local-facilities known-facilities
                                      max-depth-of-known-open-facility
                                      customers
                                      children children-boundaries children-facilities
                                      facility-customers terminated-child-count
                                      total-cost))
                              (safely-exit parent children children-facilities facility-customers customers)))
            (confirm-exit (facilities support-customers)
                          (if (= (+ 1 terminated-child-count) 4)
                              (safely-exit parent children children-facilities facility-customers support-customers)
                              (a/become quadrant-actor parent
                                      position-relative-to-parent boundary threshold
                                      depth local-facilities known-facilities
                                      max-depth-of-known-open-facility
                                      customers
                                      children children-boundaries (+ children-facilities facilities)
                                      (+ facility-customers support-customers) (+ terminated-child-count 1)
                                      total-cost)))))
(define threshold (* Alpha F))
(define bounding-box (box 0 0 GridSize GridSize))

(define root-quadrant (a/create quadrant-actor
                                #f ;; no parent
                                Root ;; root position
                                bounding-box ;; boundary
                                threshold ;; threshold
                                0 ;; depth
                                (list (mid-point bounding-box)) ;; local facilities
                                1 ;; known-facitilies
                                -1 ;; max depth of known facility
                                '() ;; customers
                                '() ;; children
                                '() ;; children boundaries
                                0 ;; children facilities
                                0 ;; facility customers
                                0 ;; terminated child count
                                0.0 ;; total cost
                                ))
(define producer (a/create producer-actor root-quadrant 0))
(a/send producer start)
