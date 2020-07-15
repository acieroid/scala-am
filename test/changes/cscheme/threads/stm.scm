;; Implementation of STM

(define (atom v)
  (cons (ref v)
        (new-lock)))

(define (atom-deref a)
  (deref (car a)))

(define (atom-swap! a f)
  (acquire (cdr a))
  (let* ((v (deref (car a))))
    (ref-set (car a) (f v))
    (release (cdr a))
    v))

(define (mc-ref val)
  (atom (cons val 0)))

(define (mc-deref tx ref)
  (if tx
      (tx-read tx ref)
      (car (atom-deref ref))))

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
   (atom-swap! NEXT_TRANSACTION_ID (lambda (i) (+ i 1)))
   (atom '())           ;; map: ref -> any value
   (atom '())           ;; set of refs
   (atom '())           ;; map: ref -> revision id
   ))

(define (set-add set element)
  (if (member element set)
      set
      (cons element set)))

(define (map-contains key map)
  (<change> ; <=========================================================================================================
    (if (null? map)
        #f
        (if (equal? (caar map) key)
            #t
            (map-contains key (cdr map))))
    (and (pair? map)
         (or (equal? (caar map) key)
             (map-contains key (cdr map))))))

(define (trn-id t) (car t))
(define (trn-in-tx-values t) (cadr t))
(define (trn-written-refs t) (caddr t))
(define (trn-last-seen-rev t) (cadddr t))

(define (tx-read tx ref)
  (let* ((in-tx-values (trn-in-tx-values tx))
         (element-in-tx (assoc ref (atom-deref in-tx-values))))
    (if element-in-tx
        (cdr element-in-tx)
        (let* ((l (atom-deref ref))
               (in-tx-value (car l))
               (read-revision (cdr l)))
          (atom-swap! in-tx-values (lambda (v) (cons (cons ref in-tx-value) v)))
          (atom-swap! (trn-last-seen-rev tx) (lambda (v) (cons (cons ref read-revision) v)))
          in-tx-value))))

(define (tx-write tx ref val)
  (atom-swap! (trn-in-tx-values tx) (lambda (v) (cons (cons ref val) v)))
  (atom-swap! (trn-written-refs tx) (lambda (v) (set-add v ref)))
  (if (not (map-contains ref (atom-deref (trn-last-seen-rev tx))))
      (atom-swap! (trn-last-seen-rev tx) (lambda (v) (cons (cons ref (cdr (atom-deref ref))) v)))
      #t)
  val)

(define COMMIT_LOCK (new-lock))

(define (keys m)
  (map car m))

(define (every? f set)
  (<change> ; <=========================================================================================================
    (if (null? set)
      #t
      (if (f (car set))
        (every? f (cdr set))
        #f))
    (or (null? set)
        (and (f (car set))
             (every? f (cdr set))))))

(define (tx-commit tx)
  (define (validate refs)
    (every? (lambda (ref) (= (cdr (atom-deref ref))
                             (cdr (assoc ref (atom-deref (trn-last-seen-rev tx))))))
            refs))
  (acquire COMMIT_LOCK)
  (let* ((in-tx-values (atom-deref (trn-in-tx-values tx)))
         (success (validate (keys in-tx-values))))
    (map (lambda (ref)
           (atom-swap! ref (lambda (v) (cons (cdr (assoc ref in-tx-values))
                                             (trn-id tx)))))
         (atom-deref (trn-written-refs tx)))
    (release COMMIT_LOCK)
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

(define (test-stm nitems nthreads niters)
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
                     (range nthreads)))
         (threads (map (lambda (t) (fork (t))) tasks)))
    (map (lambda (t) (join t)) threads)
    (map (lambda (r) (mc-deref #f r)) refs)))

(test-stm 10 10 1000)
