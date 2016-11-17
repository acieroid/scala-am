;; From SOTER benchmarks (unsafe_send). Description from SOTER:
;; This example illustrates the "Verify Absence-of-Errors" mode. The server expects a tuple {REQUEST,PID-OF-SENDER} but the main sends to it an atom instead of its pid, then generating an exception when the server tries to send back a response to what he assumes to be a pid.
;; The verification step discovers a genuine counter-example. To inspect the error trace run bfc on the generated model and look at the trace alongside the dot model of the ACS.
(let* ((server-actor (actor "server" ()
                            (message (x p)
                                     (send p message x)
                                     (become server-actor))
                            (bye () (terminate))))
       (server (create server-actor)))
  (send server message 'hi 'bye))
