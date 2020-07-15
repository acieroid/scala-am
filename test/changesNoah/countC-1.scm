(letrec ((count (lambda (n) (if (= n 0) "done" (count (- n 1)))))
		 (count-alias (lambda (n) (count n))))
  	(count 10))
