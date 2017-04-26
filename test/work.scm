;; Taken from http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/code/fun/

;
; Putting Scheme to Work
; By Olivier Danvy
; Bigre special edition "Putting Scheme to Work"
;

(define fix
  (let ((z (lambda (P)
             (lambda (u)
               (lambda (t)
                 (lambda (t)
                   (lambda (i)
                     (lambda (n)
                       (lambda (g)
                         (lambda (S)
                           (lambda (c)
                             (lambda (h)
                               (lambda (e)
                                 (lambda (m)
                                   (lambda (e)
                                     (lambda (t)
                                       (lambda (o)
                                         (lambda (W)
                                           (lambda (o)
                                             (lambda (r)
                                               (lambda (k)
                                                 (lambda (!)
                                                   (! (lambda (break)
							(((((((((((((((((((((W o) r) k)
									  W) o) r) k)
								      W) o) r) k)
								  W) o) r) k)
							      W) o) r) k) !)
							 break)))))))))))))))))))))))))
    (let ((Z z))
      (((((((((((((((((((z z) z) z) z) z) Z) Z) Z) Z) Z) Z) Z) z) z) z) z) z) z) z))))

((fix (lambda (f)
        (lambda (n)
          (if (zero? n)
              1
              (* n (f (- n 1))))))) 9)