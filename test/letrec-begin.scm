(letrec ((h (lambda ()
              '()))
         (i 1)
         (res (begin
               (h)
               i)))
  res)
