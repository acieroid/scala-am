;; Taken from https://github.com/jensnicolay/abstractmemo
;; Expected result: "hallo"
(letrec ((rotate (lambda (n x y z)
                   (if (= n 0)
                       x
                     (rotate (- n 1) y z x)))))
  (rotate 41 5 #t "hallo"))
