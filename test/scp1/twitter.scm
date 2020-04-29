(define result '())
(define output (lambda (i) (set! result (cons i result))))
(define linebreak (lambda () (set! result (cons 'linebreak result))))

(define (output-all . args)
  (for-each output args))
(define (output-all-sep args)
  (for-each (lambda (arg) (output arg) (output " "))
            args))

(define (make-tweet username text tags)

  (define (output-tweet)
    (output-all "Tweet from " username "\n" text "\nTags: ")
    (output-all-sep tags)
    (linebreak))

  (define (dispatch msg)
    (cond ((eq? msg 'text) text)
          ((eq? msg 'tags) tags)
          ((eq? msg 'username) username)
          ((eq? msg 'output) output-tweet)
          (else (output "error - wrong msg ") (output msg))))

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

    (define (output-account symbol)
      (cond ((eq? symbol 'wall) (output-wall))
            ((eq? symbol 'followers) (output-followers))
            ((eq? symbol 'account) (output-entire-account))
            (else (output "wrong symbol given"))))

    (define (output-wall)
      (output "TWEET WALL") (linebreak)
      (for-each (lambda (tweet) ((tweet 'output)) (linebreak))
                tweet-wall))

    (define (output-followers)
      (output "FOLLOWERS") (linebreak)
      (for-each (lambda (follower)
                  (output (follower 'username)) (output " "))
                followers))

    (define (output-entire-account)
      (output-all "Twitter name " username "\n"
                   "Name " name "\n")
      (output-wall)
      (output-followers)
      (linebreak) (linebreak))

    (define (dispatch msg)
      (cond ((eq? msg 'name)                           name)
            ((eq? msg 'username)                   username)
            ((eq? msg 'output)             output-account)
            ((eq? msg 'follow)                       follow)
            ((eq? msg 'add-follower)           add-follower)
            ((eq? msg 'tweet)                         tweet)
            ((eq? msg 'add-tweet-to-wall) add-tweet-to-wall)
            (else (output "error - wrong msg ") (output msg))))
    dispatch))

(define my-tweet (make-tweet "madewael" "Racket is cool!" (list "#Racket" "#Scheme")))
(define res1 (equal? (my-tweet 'username) "madewael"))
((my-tweet 'output))

(define accountE (make-account "Eline Philips" "ephilips"))
(define accountM (make-account "Mattias De Wael" "madewael"))
((accountE 'follow) accountM)
((accountM 'tweet) "Racket is cool!" "#Racket" "#Scheme")
((accountE 'tweet) "Hello World!")
((accountE 'output) 'account)
((accountM 'output) 'account)
(and res1
     (equal? result '(linebreak
                      linebreak
                      " "
                      "ephilips"
                      linebreak
                      "FOLLOWERS"
                      linebreak
                      linebreak
                      " "
                      "#Scheme"
                      " "
                      "#Racket"
                      "\nTags: "
                      "Racket is cool!"
                      "\n"
                      "madewael"
                      "Tweet from "
                      linebreak
                      "TWEET WALL"
                      "\n"
                      "Mattias De Wael"
                      "Name "
                      "\n"
                      "madewael"
                      "Twitter name "
                      linebreak
                      linebreak
                      linebreak
                      "FOLLOWERS"
                      linebreak
                      linebreak
                      " "
                      "#Scheme"
                      " "
                      "#Racket"
                      "\nTags: "
                      "Racket is cool!"
                      "\n"
                      "madewael"
                      "Tweet from "
                      linebreak
                      linebreak
                      "\nTags: "
                      "Hello World!"
                      "\n"
                      "ephilips"
                      "Tweet from "
                      linebreak
                      "TWEET WALL"
                      "\n"
                      "Eline Philips"
                      "Name "
                      "\n"
                      "ephilips"
                      "Twitter name "
                      linebreak
                      " "
                      "#Scheme"
                      " "
                      "#Racket"
                      "\nTags: "
                      "Racket is cool!"
                      "\n"
                      "madewael"
                      "Tweet from ")))