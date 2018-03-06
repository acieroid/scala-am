(letrec ((cell
          (a/actor "cell" (content)
                 (put (newcontent) (a/become cell newcontent))
                 (get (act) (a/send act value content) (a/become cell content))))
         (display-actor
          (a/actor "display" ()
                 (value (x) (if (= x 2) (a/terminate) (error "Error!")))))
         (disp (a/create display-actor))
         (c1 (a/create cell 1))
         (c2 (a/create cell 2)))
  (a/send c1 put 2)
  (a/send c2 put 5)
  (a/send c1 get disp))
