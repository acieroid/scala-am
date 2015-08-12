(letrec ((h (lambda ()
              nil))
         (i 1)
         (res (begin
               (h)
               i)))
  res)
