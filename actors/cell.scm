(letrec ((cell
          (actor "cell" (content)
                 (put (newcontent) (become cell newcontent))
                 (get (act) (send act value content) (become cell content))))
         (display-actor
          (actor "display" ()
                 (value (x) (display x))))
         (disp (create display-actor))
         (c1 (create cell 1))
         (c2 (create cell 2)))
  (send c1 put 2)
  (send c2 put 5)
  (send c1 get disp))
