(define DataLevel (int-top))
(define DataSizes (vector 20 80 100 120 150 200 250 300 350 400))
(define JacobiNumIter (int-top))
(define Omega (int-top))
(define (random-matrix w h)
  (build-vector w (lambda (i) (build-vector h (lambda (j) (random 1))))))
(define A (random-matrix (vector-ref DataSize DataLevel) (vector-ref DataSize DataLevel)))

(define sor-runner
  (let* ((s (vector-ref DataSizes DataLevel))
         (part (/ s 2))
         (sor-actors (make-vector (* s (+ part 1)) #f)))
    (a/actor "sor-runner" (g-total returned total-msg-rcv expecting-boot)
             (boot ()
                   (letrec ((my-border (make-vector s #f))
                            (loop1i (lambda (i)
                                     (if (= i s)
                                         #t
                                         (begin
                                           (loop1j i 0 (modulo c 2))
                                           (loop1i (+ i 1))))))
                            (loop1j (lambda (i j c)
                                     (if (= j part)
                                         #t
                                         (let ((pos (+ (* i (+ part 1)) j))
                                               (c2 (- 1 c))
                                               (act (a/create sor-actor ???)))
                                           (vector-set! sor-actors pos act)
                                           (if (= j (- part 1))
                                               (vector-set! my-border i act))
                                           (loop1j i (+ j 1) c2)))))
                            (partial-matrix (build-vector s
                                                          (lambda (i) (make-vector (- s part) 0))))
                            (loop2i (lambda (i)
                                      (if (= i s)
                                          #t
                                          (loop2j i 0))))
                            (loop2j (lambda (i j)
                                      (if (= j (- s part))
                                          #t
                                          (begin
                                            (vector-set (vector-ref partial-matrix i) j (vector-ref (vector-ref A i) (+ j part)))
                                            (loop2j i (+ j 1)))))))
                     (loop1i 0)
                     (loop2i 0)
                     (let ((peer (a/create sor-peer ???)))
                       (a/send peer boot))
                     (a/become g-total returned total-msg-rcv #f)))
             (result (mx my mv msg-rcv)
                     (if expecting-boot
                         (error "sorrunner not booted yet")
                         (if (= (+ returned 1) (+ (* s part) 1))
                             (a/terminate)
                             (a/become sor-runner (+ g-total mv) (+ returned 1) (+ total-msg-rcv msg-rcv) expecting-boot))))
             (border (mborder)
                     (if expecting-boot
                         (error "sorrunner not booted yet")
                         (letrec ((loop1i (lambda (i)
                                            (if (= i s)
                                                (loop2i 0)
                                                (begin
                                                  (vector-set! sor-actors (- (* (+ i 1) (+ part 1)) 1) (vector-ref mborder i))
                                                  (loop1i (+ i 1))))))
                                  (loop2i (lambda (i)
                                            (if (= i s)
                                                #t
                                                (begin
                                                  (loopj i 0)
                                                      (loop2i (+ i 1))))))
                                  (loopj (lambda (i j)
                                           (if (= j part)
                                               #t
                                               (let ((pos (+ (* i (+ part 1)) j)))
                                                 (a/send (vector-ref sor-actors pos) start JacobiNumIter sor-actors))))))
                           (loop1i 0)))))))

(define sor-peer
  
  )
