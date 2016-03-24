;; Inspired from Fig. 4 of "Learning from Mistakes -- A Comprehensive Study on Real World Concurrency Bug Characteristics"
(letrec ((io-pending #f)
         (loop (lambda ()
                 (if io-pending
                     (loop))))
         (thread1 (lambda ()
                    (set! io-pending #t)
                    (loop)))
         (thread2 (lambda ()
                    (set! io-pending #f)))
         (t1 (spawn (thread1)))
         (t2 (spawn (thread2))))
  (join t1) (join t2))
