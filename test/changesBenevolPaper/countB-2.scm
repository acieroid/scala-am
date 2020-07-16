(let ((input 10))
	(letrec ((count2 (lambda (n) (if (= n 0) "done" (count2 (- n 1))))))
  		(count2 input)))
