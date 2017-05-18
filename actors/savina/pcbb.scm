(define BufferSize (int-top))
(define NumProducers (int-top))
(define NumConsumers (int-top))
(define NumItemsPerProducer (int-top))
(define BufferSize (int-top))

(define ConsCost 40)
(define ProdCost 40)
(define AdjustedBufferSize (- BufferSize NumProducers))

(define (for-each f l)
  (if (null? l)
      #t
      (if (pair? l)
          (begin (f (car l)) (for-each f (cdr l)))
          (error "Cannot for-each over a non-list"))))

(define (spawn-producers manager)
  (letrec ((loop (lambda (i)
                   (if (= i NumProducers)
                       '()
                       (let ((p (a/create producer i manager 0 0)))
                         (a/send p produce-data)
                         (cons p (loop (+ i 1))))))))
    (loop 0)))

(define (spawn-consumers manager)
  (letrec ((loop (lambda (i)
                   (if (= i NumConsumers)
                       '()
                       (let ((c (a/create consumer i manager 0)))
                         (cons c (loop (+ i 1))))))))
    (loop 0)))

(define manager (a/actor "manager" (producers consumers available-producers available-consumers pending-data terminated-producers)
                         (start ()
                                (let ((producers (spawn-producers self))
                                      (consumers (spawn-consumers self)))
                                  (a/become manager producers consumers '() consumers pending-data terminated-producers)))
                         (data-item (producer data)
                                    (if (null? available-consumers)
                                        (if (> (length pending-data) AdjustedBufferSize)
                                            (a/become manager producers consumers (cons producer available-producers) available-consumers (cons data pending-data) terminated-producers)
                                            (begin
                                              (a/send producer produce-data)
                                              (a/become manager producers consumers available-producers available-consumers (cons data pending-data) terminated-producers)))
                                        (begin
                                          (a/send (car available-consumers) data-item data)
                                          (a/become manager producers consumers
                                                    (if (> (length pending-data) AdjustedBufferSize)
                                                        (cons producer available-producers)
                                                        (begin
                                                          (a/send producer produce-data)
                                                          available-producers))
                                                    available-consumers (cons data pending-data) terminated-producers))))
                         (consumer-available (consumer)
                                             (if (null? pending-data)
                                                 (if (and (= terminated-producers NumProducers) (= (+ 1 (length available-consumers) NumConsumers)))
                                                     (begin
                                                       (for-each (lambda (c) (a/send c exit)) consumers)
                                                       (a/terminate))
                                                     (a/become manager producers consumers available-producers (cons consumer available-consumers) pending-data terminated-producers))
                                                 (begin
                                                   (a/send consumer data-item (car pending-data))
                                                   (if (not (null? available-producers))
                                                       (begin
                                                         (a/send (car available-producers) produce-data)
                                                         (a/become manager producers consumers (cdr available-producers) available-consumers (cdr pending-data) terminated-producers))
                                                       (a/become manager producers consumers available-producers available-consumers (cdr pending-data) terminated-producers)))))
                         (exit () (if (and (= (+ 1 terminated-producers) NumProducers) (= (length available-consumers) NumConsumers))
                                      (begin
                                        (for-each (lambda (c) (a/send c exit)) consumers)
                                        (a/terminate))
                                      (a/become manager producers consumers available-producers available-consumers pending-data (+ 1 terminated-producers))))))

(define (process-item item cost) (random 10)) ; not modeled as in Savina, but doesn't change anything for static analysis

(define producer (a/actor "producer" (id manager items-produced prod-item)
                          (produce-data ()
                                        (if (= items-produced NumItemsPerProducer)
                                            (begin
                                              (a/send manager exit)
                                              (a/terminate))
                                            (begin
                                              (let ((prod-item2 (process-item prod-item ProdCost)))
                                                (a/send manager data-item self prod-item)
                                                (a/become producer id manager (+ items-produced 1) prod-item2)))))))
(define consumer (a/actor "consumer" (id manager cons-item)
                          (data-item (data)
                                     (let ((cons-item2 (process-item (+ cons-item data) ConsCost)))
                                       (a/send manager consumer-available self)
                                       (a/become consumer id manager cons-item2)))
                          (exit () (a/terminate))))
(define m (a/create manager '() '() '() '() '() 0))
(a/send m start)
