;; Meta-circular evaluator with support for threads
(define (range a b)
  (letrec ((loop (lambda (i acc)
                   (if (< i a)
                       acc
                       (loop (- i 1) (cons i acc))))))
    (loop (- b 1) '())))



(define (env-lookup env var)
  (if (null? env)
      (error "unbound variable")
      (if (eq? (caar env) var)
          (cdar env)
          (env-lookup (cdr env) var))))

(define (env-extend env bindings)
  (if (null? bindings)
      env
      (cons (car bindings) (env-extend env (cdr bindings)))))

(define (apply-proc f args env)
  (let ((fval (eval f env))
        (argsval (map (lambda (arg) (eval arg env)) args)))
    (fval (car argsval) (cadr argsval))))

(define (ev-lambda var1 var2 body env)
  (lambda (v1 v2) (eval body (env-extend env (list (cons var1 v1) (cons var2 v2))))))

(define (ev-let bindings body env)
  (let ((env2 (env-extend env
                          (map (lambda (b)
                                 (cons (car b) (eval (cadr b) env)))
                               bindings))))
    (eval body env2)))

(define (ev-if condition then else env)
  (if (eval condition env)
      (eval then env)
      (eval else env)))

(define (ev-fork body env)
  (fork (eval body env)))

(define (ev-join t env)
  (let ((thread (eval t env)))
    (join thread)))

(define (eval exp env)
  (if (number? exp)
      exp
      (if (symbol? exp)
          (env-lookup env exp)
          (if (pair? exp)
              (let ((op (car exp)))
                (if (eq? op 'lambda)
                    (ev-lambda (caadr exp) (cadadr exp) (caddr exp) env)
                    (if (eq? op 'let)
                        (ev-let (cadr exp) (caddr exp) env)
                        (if (eq? op 'if)
                            (ev-if (cadr exp) (caddr exp) (cadddr exp) env)
                            (if (eq? op 'fork)
                                (ev-fork (cadr exp) env)
                                (if (eq? op 'join)
                                    (ev-join (cadr exp) env)
                                    (apply-proc (car exp) (cdr exp) env)))))))
              (error "unknown expression")))))

(define N (int-top))
(define initial-environment
  (list (cons 'n N)
        (cons '+ +)
        (cons '< <)))

(eval '(join (fork ((lambda (x y) (+ x y)) n n)))
      initial-environment)
