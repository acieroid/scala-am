(define result '())
(define display (lambda (i) (set! result (cons i result))))
(define newline (lambda () (set! result (cons 'newline result))))

(define (display-all . args)
  (for-each display args))
(define (display-all-sep args)
  (for-each (lambda (arg) (display arg) (display " "))
            args))

(define (make-tweet username text tags)

  (define (display-tweet)
    (display-all "Tweet from " username "\n" text "\nTags: ")
    (display-all-sep tags)
    (newline))

  (define (dispatch msg)
    (cond ((eq? msg 'text) text)
          ((eq? msg 'tags) tags)
          ((eq? msg 'username) username)
          ((eq? msg 'display) display-tweet)
          (else (display "error - wrong msg ") (display msg))))

  (if (> (string-length text) 140)
      #f
      dispatch))

(define (make-account name username)
  (let ((followers '())
        (tweets '())
        (tweet-wall '()))

    (define (follow account)
      ((account 'add-follower) dispatch))

    (define (add-follower account)
      (set! followers (cons account followers)))

    (define (tweet text . tags)
      (let ((tweet-obj (make-tweet username text tags)))
        (set! tweets (cons tweet-obj tweets))
        (set! tweet-wall (cons tweet-obj tweet-wall))
        (for-each (lambda (follower)
                    ((follower 'add-tweet-to-wall) tweet-obj))
                  followers)))

    (define (add-tweet-to-wall tweet)
      (set! tweet-wall (cons tweet tweet-wall)))

    (define (display-account symbol)
      (cond ((eq? symbol 'wall) (display-wall))
            ((eq? symbol 'followers) (display-followers))
            ((eq? symbol 'account) (display-entire-account))
            (else (display "wrong symbol given"))))

    (define (display-wall)
      (display "TWEET WALL") (newline)
      (for-each (lambda (tweet) ((tweet 'display)) (newline))
                tweet-wall))

    (define (display-followers)
      (display "FOLLOWERS") (newline)
      (for-each (lambda (follower)
                  (display (follower 'username)) (display " "))
                followers))

    (define (display-entire-account)
      (display-all "Twitter name " username "\n"
                   "Name " name "\n")
      (display-wall)
      (display-followers)
      (newline) (newline))

    (define (dispatch msg)
      (cond ((eq? msg 'name)                           name)
            ((eq? msg 'username)                   username)
            ((eq? msg 'display)             display-account)
            ((eq? msg 'follow)                       follow)
            ((eq? msg 'add-follower)           add-follower)
            ((eq? msg 'tweet)                         tweet)
            ((eq? msg 'add-tweet-to-wall) add-tweet-to-wall)
            (else (display "error - wrong msg ") (display msg))))
    dispatch))

(define my-tweet (make-tweet "madewael" "Racket is cool!" (list "#Racket" "#Scheme")))
(define res1 (equal? (my-tweet 'username) "madewael"))
((my-tweet 'display))

(define (make-account name username)
  (let ((followers '())
        (tweets '())
        (tweet-wall '()))

    (define (follow account)
      ((account 'add-follower) dispatch))

    (define (add-follower account)
      (set! followers (cons account followers)))

    (define (tweet text . tags)
      (let ((tweet-obj (make-tweet username text tags)))
        (set! tweets (cons tweet-obj tweets))
        (set! tweet-wall (cons tweet-obj tweet-wall))
        (for-each (lambda (follower)
                    ((follower 'add-tweet-to-wall) tweet-obj))
                  followers)))

    (define (add-tweet-to-wall tweet)
      (set! tweet-wall (cons tweet tweet-wall)))

    (define (display-account symbol)
      (cond ((eq? symbol 'wall) (display-wall))
            ((eq? symbol 'followers) (display-followers))
            ((eq? symbol 'account) (display-entire-account))
            (else (display "wrong symbol given"))))

    (define (display-wall)
      (display "TWEET WALL") (newline)
      (for-each (lambda (tweet) ((tweet 'display)) (newline))
                tweet-wall))

    (define (display-followers)
      (display "FOLLOWERS") (newline)
      (for-each (lambda (follower)
                  (display (follower 'username)) (display " "))
                followers))

    (define (display-entire-account)
      (display-all "Twitter name " username "\n"
                   "Name " name "\n")
      (display-wall)
      (display-followers)
      (newline) (newline))

    (define (dispatch msg)
      (cond ((eq? msg 'name)                           name)
            ((eq? msg 'username)                   username)
            ((eq? msg 'display)             display-account)
            ((eq? msg 'follow)                       follow)
            ((eq? msg 'add-follower)           add-follower)
            ((eq? msg 'tweet)                         tweet)
            ((eq? msg 'add-tweet-to-wall) add-tweet-to-wall)
            (else (display "error - wrong msg ") (display msg))))
    dispatch))

(define accountE (make-account "Eline Philips" "ephilips"))
(define accountM (make-account "Mattias De Wael" "madewael"))
((accountE 'follow) accountM)
((accountM 'tweet) "Racket is cool!" "#Racket" "#Scheme")
((accountE 'tweet) "Hello World!")
((accountE 'display) 'account)
((accountM 'display) 'account)
(and res1
     (equal? result '(newline
                      newline
                      " "
                      "ephilips"
                      newline
                      "FOLLOWERS"
                      newline
                      newline
                      " "
                      "#Scheme"
                      " "
                      "#Racket"
                      "\nTags: "
                      "Racket is cool!"
                      "\n"
                      "madewael"
                      "Tweet from "
                      newline
                      "TWEET WALL"
                      "\n"
                      "Mattias De Wael"
                      "Name "
                      "\n"
                      "madewael"
                      "Twitter name "
                      newline
                      newline
                      newline
                      "FOLLOWERS"
                      newline
                      newline
                      " "
                      "#Scheme"
                      " "
                      "#Racket"
                      "\nTags: "
                      "Racket is cool!"
                      "\n"
                      "madewael"
                      "Tweet from "
                      newline
                      newline
                      "\nTags: "
                      "Hello World!"
                      "\n"
                      "ephilips"
                      "Tweet from "
                      newline
                      "TWEET WALL"
                      "\n"
                      "Eline Philips"
                      "Name "
                      "\n"
                      "ephilips"
                      "Twitter name "
                      newline
                      " "
                      "#Scheme"
                      " "
                      "#Racket"
                      "\nTags: "
                      "Racket is cool!"
                      "\n"
                      "madewael"
                      "Tweet from ")))