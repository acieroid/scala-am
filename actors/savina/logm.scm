(define NumSeries (int-top))
(define NumComputers NumSeries)
(define NumWorkers NumSeries)
(define StartRate (int-top))
(define Increment (int-top))

(define (build-vector n f)
  (letrec ((v (make-vector n #f))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))

(define (vector-foreach f v)
  (letrec ((loop (lambda (i)
                   (if (< i (vector-length v))
                       (begin
                         (f (vector-ref v i))
                         (loop (+ i 1)))
                       'done))))
    (loop 0)))

(define master-init
  (a/actor "master-init" ()
           (start ()
                  (let* ((computers
                          (build-vector NumComputers
                                        (lambda (i)
                                          (let* ((rate (+ StartRate (* i Increment)))
                                                 (actor (a/create rate-computer rate)))
                                            actor))))
                         (workers
                          (build-vector NumWorkers
                                        (lambda (i)
                                          (let* ((rate-computer
                                                 (vector-ref computers (modulo i NumComputers)))
                                                 (start-term (* i Increment))
                                                 (actor (a/create series-worker a/self rate-computer start-term)))
                                            (a/send actor next-term)
                                            (a/send actor get-term)
                                            actor
                                            )))))
                    (a/become master computers workers NumWorkers 0 0)))))
(define master
  (a/actor "master" (computers workers num-work-requested num-work-received terms-sum)
           (result (term)
                   (if (= (+ num-work-received 1) num-work-requested)
                       (begin
                         (vector-foreach (lambda (a) (a/send a stop)) computers)
                         (vector-foreach (lambda (a) (a/send a stop)) workers)
                         (a/terminate))
                       (a/become master computers workers num-work-requested (+ num-work-received 1) (+ terms-sum term))))))

(define series-worker-wait
  (a/actor "series-worker-wait" (master computer)
         (next-term ()
                    (a/send a/self next-term)
                    (a/become series-worker-wait master computer))
         (result (term)
                 (a/become series-worker master computer term))
         (get-term ()
                   (a/send a/self get-term)
                   (a/become series-worker-wait master computer))
         (stop ()
               (a/send a/self stop)
               (a/become series-worker-wait master computer))))
(define series-worker
  (a/actor "series-worker" (master computer cur-term)
           (next-term ()
                      (a/send computer compute cur-term a/self)
                      ;; (a/become series-worker master computer cur-term)
                      (a/become series-worker-wait master computer)
                      )
           (result (term)
                   (a/become series-worker master computer term))
           (get-term ()
                     (a/send master result cur-term)
                     (a/become series-worker master computer cur-term))
           (stop () (a/terminate))))

(define (compute-next-term cur rate)
  (* rate cur (- 1 cur)))

(define rate-computer
  (a/actor "rate-computer" (rate)
           (compute (term sender)
                    (a/send sender result (compute-next-term term rate))
                    (a/become rate-computer rate))
           (stop ()
                 (a/terminate))))

;; (define master-actor (a/create master #f #f 0 0 0))
(define master-actor (a/create master-init))
(a/send master-actor start)
