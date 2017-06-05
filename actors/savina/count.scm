;; Adapted from Savina benchmarks ("Counting Actor" benchmarks, coming from Theron)
;; counting-actor will receive 10 increment and one retreive: bound is 11 (or N+1)
(define N (int-top))
(define producer-actor
  (a/actor "producer" (counter)
           (increment ()
                      (letrec ((loop (lambda (n)
                                       (if (> n 0)
                                           (begin
                                             (a/send counter increment)
                                             (loop (- n 1)))
                                           'done))))
                        (loop N)
                        (a/send counter retrieve self)
                        (a/become producer-actor counter)))
           (result (count)
                   (if (= count N)
                       (display "Success!")
                       (error "Error!"))
                   (a/terminate))))
(define counting-actor
 (a/actor "counting" (count)
          (increment ()
                     (a/become counting-actor (+ count 1)))
          (retrieve (to)
                    (a/send to result count)
                    (a/terminate))))
(define counter (a/create counting-actor 0))
(define producer (a/create producer-actor counter))
(a/send producer increment)
