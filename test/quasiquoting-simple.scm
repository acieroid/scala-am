;; Tests for simple quasiquoting (no splicing, no nested quasiquoting/unquoting)
;; Examples taken from
;; * https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Quoting.html
;; * ftp://ftp.cs.utexas.edu/pub/garbage/cs345/schintro-v13/schintro_129.html
;; * http://www.scheme.com/csug7/control.html

;; Reverse not supported
;; (exp6 '(a b (e d c) f g))
;; (res6a `(a b ,(reverse '(c d e)) f g))
;; (res6b (quasiquote (a b (unquote (reverse '(c d e))) f g)))


(define day-of-week 'Sunday)
(define (make-greeting)
  `(Welcome to the FooBar system!  We hope you
            enjoy your visit on this fine ,day-of-week))

(let ((exp1 '(list 3 4))
      (res1a `(list ,(+ 1 2) 4))
      (res1b (quasiquote (list (unquote (+ 1 2)) 4)))
      (exp2 5)
      (res2a `,(+ 2 3))
      (res2b (quasiquote (unquote (+ 2 3))))
      (exp3 '(Welcome to the FooBar system! We hope you enjoy your visit on this
                      fine Sunday))
      (res3 (make-greeting))
      (exp4 '(+ 2 3))
      (res4a `(+ 2 3))
      (res4b (quasiquote (+ 2 3)))
      (exp5 '(+ 2 12))
      (res5a `(+ 2 ,(* 3 4)))
      (res5b (quasiquote (+ 2 ,(* 3 4)))))
  (and (equal? exp1 res1a)
       (equal? exp1 res1b)
       (= exp2 res2a)
       (= exp2 res2b)
       (equal? exp3 res3)
       (equal? exp4 res4a)
       (equal? exp4 res4b)
       (equal? exp5 res5a)
       (equal? exp5 res5b)))
