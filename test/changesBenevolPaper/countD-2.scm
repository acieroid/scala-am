(letrec ((count (lambda (n) (if (= n 0) #f (count (- n 1))))))
  	(count 10))