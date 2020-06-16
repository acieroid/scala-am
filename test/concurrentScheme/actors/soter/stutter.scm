;; From SOTER benchmarks (stutter). Description from SOTER:
;; A process running stutter discards a message in the mailbox and feeds the next to its functional argument and so on in a loop. Another process sends a `bad argument' and a good one in alternation such that only the good ones are fed to the function. The property is that the function is never called with a bad argument. This cannot be proved because sequential information of the mailboxes, which is essential for the verification, is lost in the counter abstraction.
;; This example shows some limitations of Soter: no data/message abstraction supported by Soter can get rid of the spurious traces.
(letrec ((stutter (actor "stutter" (f)
                         (message (x)
                                  (become unstutter f))))
         (unstutter (actor "unstutter" (f)
                           (message (x)
                                    (f x)
                                    (become stutter f))))
         (p (create stutter (lambda (msg) (dosmt msg))))
         (dosmt (lambda (x) (if (= x 0) (error "We abhorr '0's") "we love not-0")))
         (sendA (lambda (p) (send p message 0) (sendB p)))
         (sendB (lambda (p) (send p message 1) (sendA p))))
  (sendA p))
