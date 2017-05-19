(define NumRounds (int-top))
(define NumSmokers (int-top))

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

(define (notify-random-smoker smoker-actors)
  (a/send (vector-ref smoker-actors (random NumSmokers)) start-smoking (+ (random 1000) 10)))
(define arbitrator
  (a/actor "arbitrator" (smoker-actors rounds-so-far)
           (create-smokers ()
                           (a/become arbitrator (build-vector NumSmokers (lambda (i) (a/create smoker self))) 0))
           (start ()
                  (notify-random-smoker smoker-actors)
                  (a/become arbitrator smoker-actors rounds-so-far))
           (started-smoking ()
                            (if (= (+ rounds-so-far 1) NumRounds)
                                (begin
                                  (vector-foreach (lambda (s) (a/send s exit)) smoker-actors)
                                  (a/terminate))
                                (begin
                                  (notify-random-smoker smoker-actors)
                                  (a/become arbitrator smoker-actors (+ rounds-so-far 1)))))))

(define smoker
  (a/actor "smoker" (arbitrator)
           (start-smoking (wait)
                          (a/send arbitrator started-smoking)
                          (a/become smoker arbitrator))
           (exit () (a/terminate))))

(define arbitrator-actor (a/create arbitrator #f 0))
(a/send arbitrator-actor create-smokers)
(a/send arbitrator-actor start)
