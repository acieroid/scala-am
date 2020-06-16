;; Implementation of STM
(define (mc-ref val)
  (atom (cons val 0)))

(define (mc-deref tx ref)
  (if tx
      (tx-read tx ref)
      (car (read ref))))

(define (mc-ref-set tx ref newval)
  (if tx
      (tx-write tx ref newval)
      (error "can't set mc-ref outside of a transaction")))

(define (mc-alter tx ref fun)
  (mc-ref-set tx ref (fun (mc-deref tx ref))))

(define (mc-commute tx ref fun)
  (mc-alter tx ref fun))

(define (mc-ensure tx ref)
  (mc-alter tx ref (lambda (x) x)))

(define NEXT_TRANSACTION_ID (atom 0))

(define (make-transaction)
  (list
   (swap! NEXT_TRANSACTION_ID (lambda (i) (+ i 1)))
   (atom '())           ;; map: ref -> any value
   (atom '())           ;; set of refs
   (atom '())           ;; map: ref -> revision id
   ))

(define (set-add set element)
  (if (member element set)
      set
      (cons element set)))

(define (map-contains key map)
  (if (null? map)
      #f
      (if (equal? (caar map) key)
          #t
          (map-contains key (cdr map)))))

(define (trn-id t) (car t))
(define (trn-in-tx-values t) (cadr t))
(define (trn-written-refs t) (caddr t))
(define (trn-last-seen-rev t) (cadddr t))

(define (tx-read tx ref)
  (let* ((in-tx-values (trn-in-tx-values tx))
         (element-in-tx (assoc ref (read in-tx-values))))
    (if element-in-tx
        (cdr element-in-tx)
        (let* ((l (read ref))
               (in-tx-value (car l))
               (read-revision (cdr l)))
          (swap! in-tx-values (lambda (v) (cons (cons ref in-tx-value) v)))
          (swap! (trn-last-seen-rev tx) (lambda (v) (cons (cons ref read-revision) v)))
          in-tx-value))))

(define (tx-write tx ref val)
  (swap! (trn-in-tx-values tx) (lambda (v) (cons (cons ref val) v)))
  (swap! (trn-written-refs tx) (lambda (v) (set-add v ref)))
  (if (not (map-contains ref (read (trn-last-seen-rev tx))))
      (swap! (trn-last-seen-rev tx) (lambda (v) (cons (cons ref (cdr (read ref))) v)))
      #t)
  val)

(define COMMIT_LOCK (t/new-lock))

(define (keys m)
  (map car m))

(define (every? f set)
  (if (null? set)
      #t
      (if (f (car set))
          (every? f (cdr set))
          #f)))

(define (tx-commit tx)
  (define (validate refs)
    (every? (lambda (ref) (= (cdr (read ref))
                             (cdr (assoc ref (read (trn-last-seen-rev tx))))))
            refs))
  (t/acquire COMMIT_LOCK)
  (let* ((in-tx-values (read (trn-in-tx-values tx)))
         (success (validate (keys in-tx-values))))
    (map (lambda (ref)
           (swap! ref (lambda (v) (cons (cdr (assoc ref in-tx-values))
                                             (trn-id tx)))))
         (read (trn-written-refs tx)))
    (t/release COMMIT_LOCK)
    success))

(define (tx-run tx fun)
  (let ((result (fun tx)))
    (if (tx-commit tx)
        result
        (tx-run (make-transaction) fun))))

(define (mc-sync tx fun)
  (if (not tx)
      (tx-run (make-transaction) fun)
      (fun tx)))

(define (replicate n v)
  (if (= n 0)
      '()
      (cons v (replicate (- n 1) v))))

(define (range n)
  (letrec ((loop (lambda (i acc)
                   (if (= i 0)
                       acc
                       (loop (- i 1) (cons i acc))))))
    (loop n '())))

(define (test-stm nitems nthrds niters)
  (let* ((refs (map mc-ref (replicate nitems 0)))
         (tasks (map (lambda (t)
                       (lambda ()
                         (letrec ((loop (lambda (n)
                                          (if (= n niters)
                                              'done
                                              (begin
                                                (mc-sync #f
                                                 (lambda (tx)
                                                   (for-each (lambda (r)
                                                               (mc-alter tx r (lambda (v) (+ v 1 t))))
                                                             refs)
                                                   ))
                                                (loop (+ n 1)))))))
                           (loop 0))))
                     (range nthrds)))
         (thrds (map (lambda (t) (future (t))) tasks)))
    (map (lambda (t) (deref t)) thrds)
    (map (lambda (r) (mc-deref #f r)) refs)))

(test-stm 10 10 1000)
