;; Inspired from http://alexsclassblogg.blogspot.be/2013/11/python-alternating-bit-socket.html
(define (build-vector n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))

(define CLOSING -1)
(define (server data in in-lock out out-lock seqnumber nexttosend)
  (let ((v (begin
             (acquire in-lock)
             (let ((r (deref in)))
               (ref-set in #f)
               (release in-lock)
               r))))
    ;(sleep 1)
    (if v
        (begin
          ;; received something
          (if (= v CLOSING)
              'done
              (if (= v seqnumber)
                  ;; expecting what we were
                  (let ((seqnumber2 (if (= seqnumber 0) 1 0))
                        (nexttosend2 (+ nexttosend 1)))
                    (acquire out-lock)
                    (ref-set out (cons (vector-ref data nexttosend2) seqnumber2))
                    (release out-lock)
                    (server data in in-lock out out-lock seqnumber2 nexttosend2))
                  ;; Not expecting
                  (let ((seqnumber2 (if (= seqnumber 0) 1 0))
                        (nexttosend2 (- nexttosend 1)))
                    (acquire out-lock)
                    (ref-set out (cons (vector-ref data nexttosend2) seqnumber2))
                    (release out-lock)
                    (server data in in-lock out out-lock seqnumber2 nexttosend2)))))
        ;; not received anything
        (server data in in-lock out out-lock seqnumber nexttosend))))

(define (client i in in-lock out out-lock ack)
  (let ((v (begin
             (acquire in-lock)
             (let ((r (deref in)))
               (ref-set in #f)
               (release in-lock)
               r))))
    ;(sleep 1)
    (if v
        (begin
          ;; received something
          (if (= (cdr v) ack)
              (let ((ack2 (if (= ack 0) 1 0)))
                (if (>= (+ i 1) N)
                    (begin
                      (acquire out-lock)
                      (ref-set out CLOSING)
                      (release out-lock))
                    (begin
                      (acquire out-lock)
                      (ref-set out ack)
                      (release out-lock)
                      (client (+ i 1) in in-lock out out-lock ack2))))
              (begin
                (acquire out-lock)
                (ref-set out ack)
                (release out-lock)
                (client i in in-lock out out-lock ack))))
        ;; not received anything
        (client i in in-lock out out-lock ack))))

(define N (random 42))
(define data-to-send (build-vector N 0 (lambda (i) (random 100))))
; (printf "data to send: ~a~n" data-to-send)
(define client->server (ref 0))
(define client->server-lock (new-lock))
(define server->client (ref #f))
(define server->client-lock (new-lock))
(define s (fork (server data-to-send client->server client->server-lock server->client server->client-lock 0 -1)))
(define c (fork (client 0 server->client server->client-lock client->server client->server-lock 1)))
(join s)
(join c)

(display-recorded)
