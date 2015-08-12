;; Taken from https://github.com/jensnicolay/abstractmemo
;; Expected result: 550
(letrec ((lp1 (lambda (i x)
                (letrec ((a (= 0 i )))
                  (if a
                      x
                    (letrec ((lp2 (lambda (j f y)
                                    (letrec ((b (= 0 j )))
                                      (if b
                                          (lp1 (- i 1 ) y )
                                        (lp2 (- j 1 ) f (f y)))))))
                      (lp2 10 (lambda (n) (+ n i )) x )))))))
  (lp1 10 0 ))
