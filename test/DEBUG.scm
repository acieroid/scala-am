(define (super-merge-n lsts)
  (if (null? lsts)
      '()
    (append '()
      (super-merge-n
        (append (cdr lsts) '())))))
(super-merge-n '(() ()))