;; Copyright 2016 Jens Van der Plas
;;
;; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;; 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer
;;    in the documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
;; OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
;; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Notice: this file contains a modified version of the original Railway coordination program (phase 1) written by Jens Van der Plas.

;; Dummy functions for static analyser.
(define default-random-source 'source)
(define (random-source-randomize! source) (display "randomised ") (display source) (newline))
(define random-integer random)
(define (random-real) (/ (random 100) 100))

(define milliseconds 0)
(define (current-inexact-milliseconds)
  (set! milliseconds (+ milliseconds (random 1000000)))
  milliseconds)

(define (make-queue) (cons 'queue '()))
(define (enqueue! q e) (set-cdr! q (cons e (cdr q))))
(define (dequeue! q)
  (let ((r (reverse (cdr q))))
    (set-cdr! q (reverse (cdr r)))
    (car r)))
(define (queue-empty? q) (null? (cdr q)))

(define (mutable-set) (cons 'mutable-set '()))
(define (set-member? s el)
  (let loop ((els (cdr s)))
    (if (null? els)
      #f
      (if (equal? (car els) el)
        #t
        (loop (cdr els))))))
(define (set-add! s e)
  (if (set-member? s e)
    #t
    (set-cdr! s (cons e (cdr s)))))

(define (make-tile w h . bm-msk)
  (let ((x 0)
         (y 0)
         (scale 1)
         (update-callback (lambda () #t)))

    ; Set the X position on the screen
    ; Integer -> void
    (define (set-x! new_x)
      (set! x new_x)
      (update-callback))

    ; Set the Y position on the screen
    ; Integer -> void
    (define (set-y! new_y)
      (set! y new_y)
      (update-callback))

    (define (set-scale! s)
      (set! scale s)
      (update-callback))

    (define (clear)
      (display "clearing") (newline)
      (update-callback))

    (define (draw-text . t)
      (for-each display t) (newline)
      (update-callback))

    (define (draw . args)
      (display "Drawing tile!") (newline))

    (define (dispatch msg . args)
      (cond
        ; Not to be called manually
        ((eq? msg 'draw) draw)
        ((eq? msg 'set-on-update!) set-on-update!)

        ; Getters and setters
        ((eq? msg 'set-x!) set-x!)
        ((eq? msg 'set-y!) set-y!)
        ((eq? msg 'get-x) x)
        ((eq? msg 'get-y) y)
        ((eq? msg 'get-w) w)
        ((eq? msg 'get-h) h)
        ((eq? msg 'set-scale!) set-scale!)
        ((eq? msg 'clear) (clear))
        ((eq? msg 'draw-text) draw-text)
        (else (display "wrong message in tile") (display msg) (newline))))
    dispatch))

(define (make-layer)
  (let ((drawables '()))

    (define (add-drawable d)
      (set! drawables (cons d drawables))
      (for-each (lambda (tile) ((tile 'draw) 'bitmap-dc)) drawables))

    (define (remove-drawable d)
      (let loop ((lst drawables))
        (if (null? lst)
            '()
          (if (equal? d (car lst))
            (cdr lst)
            (cons (car lst)
              (loop (cdr lst))))))
      (for-each (lambda (tile) ((tile 'draw) 'bitmap-dc)) drawables))

    (define (draw)
      (display "Drawing!")
      (newline)
      (for-each (lambda (tile) ((tile 'draw) 'bitmap-dc)) drawables))

    (define (dispatch msg)
      (cond ((eq? msg 'add-drawable)  add-drawable)
        ((eq? msg 'remove-drawable) remove-drawable)
        ((eq? msg 'draw) draw)
        (else (display "wrong message sent to layer: ") (display msg) (newline))))
    dispatch))

(define (make-window w h title)
  (let ((layers '())
         (update-callback (lambda () #t))
         (keyboard-callback (lambda () #t)))

    (define (add-layer)
      (define layer (make-layer))
      (set! layers (cons layer layers))
      layer)

    (define (set-background! bg)
      (display "background set to ")
      (display bg)
      (newline))

    (define (start)
      (define commands (list 'left 'left 'left 'left 'left
                           'right 'right 'right 'right 'right 'right
                           'up 'up 'up 'up 'up
                           'home 'home
                           'end
                         #\newline #\newline ; Orignally #\return
                         #\space #\space))
      (define (random-command)
        (list-ref commands (random (length commands))))
      (let loop ((i 0))
        (if (< i 100)
          (begin
            (keyboard-callback (random-command))
            (update-callback (random 150))
            (loop (+ i 1))))))

    (define (dispatch msg)
      (cond ((eq? msg 'make-layer) (add-layer))
        ((eq? msg 'set-background!) set-background!)
        ((eq? msg 'set-key-callback!) (lambda (eh) (set! keyboard-callback eh)))
        ((eq? msg 'set-update-callback!) (lambda (gl) (set! update-callback gl)))
        ((eq? msg 'start) (start))
        (else (display "wrong message sent to window: ") (display msg) (newline))))

    dispatch))

(define (sendm object message . args)
  (if (null? args)
    ((object message))
    (apply (object message) args)))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;             Definitions
; ***********************************

; Dit bestand bevat allerlei kleine abstracties die veelvuldig gebruikt zullen worden in alle andere bestanden.

; *************************************************************************************************************************

; Een klein ADT op de positie van een trein bij te houden.
; De positie van een trein wordt bepaald door de huidige sectie, de vorige sectie en de afgelegde weg op de huidige sectie.
(define (pos current-section previous-section distance)
  (vector current-section previous-section distance))
(define (curr-s pos)(vector-ref pos 0))
(define (prev-s pos)(vector-ref pos 1))
(define (dist pos)(vector-ref pos 2))
(define (curr-s! pos c)(vector-set! pos 0 c))
(define (prev-s! pos p)(vector-set! pos 1 p))
(define (dist! pos d)(vector-set! pos 2 d))

; Laat toe de twee posities van een trein tezamen door te geven.
(define combine-positions cons)
(define position-front car)
(define position-rear cdr)

; *************************************************************************************************************************

; Symbolen en constanten

(define undefined 'undefined)
(define free 'free)
(define occupied 'occupied)
(define green 'green)
(define red 'red)
(define train 'train)
(define locomotive 'locomotive)
(define carriage 'carriage)
(define section 'section)
(define switch 'switch)
(define dswitch 'double-slip-switch) ; Engels wissel.
(define up 'up)
(define down 'down)

(define A 'A)
(define B 'B)
(define single 'single)
(define multi 'multi)

(define infinity 999999e99) ; We gaan ervan uit dat infinity de grooste waarde is die in het programma kan voorkomen.

; *************************************************************************************************************************

; Macro gebruikt voor message passing
;(define-syntax sendm
;  (syntax-rules ()
;    ((sendm object message argument ...)
;      ((object message) argument ...))
;    ((sendm object message)
;      ((object message)))))

; *************************************************************************************************************************

; Functionaliteit voor het aanmaken van unieke id's door middel van een klein objectje.
(define (make-id-generator)
  (let ((last-used-id 0))
    (lambda ()
      (set! last-used-id (+ last-used-id 1))
      last-used-id)))

; De aanroep van generate-id levert een unieke id op.
(define generate-id (make-id-generator))

; *************************************************************************************************************************

; Definities voor het voorstellen van de spoorweg.

(define (make-railway elements connections width height A0X A0Y B0X B0Y)
  (vector elements connections width height A0X A0Y B0X B0Y))
(define (read-railway-elements r)
  (vector-ref r 0))
(define (read-railway-connections r)
  (vector-ref r 1))
(define (read-railway-width r)
  (vector-ref r 2))
(define (read-railway-height r)
  (vector-ref r 3))
(define (read-railway-Ax r)
  (vector-ref r 4))
(define (read-railway-Ay r)
  (vector-ref r 5))
(define (read-railway-Bx r)
  (vector-ref r 6))
(define (read-railway-By r)
  (vector-ref r 7))

(define describe-elements vector)
(define describe-connections vector)
(define get-elements-count vector-length) ; Krijg de hoeveelheid gespecifieerde secties/wissels.
(define get-connection-count vector-length) ; Krijg de hoeveelheid gespecifieerde connecties.
(define read-element vector-ref) ; Krijg de beschrijving van een element.
(define read-description vector-ref) ; Krijg de beschrijving van een connectie.

; *************************************************************************************************************************

; Definities voor het beschrijven van de elementen van de spoorweg.

; sectie = #(section lengte max-speed components)
; wissel = #(switch max-speed type)
; Engels wissel = #(dswitch max-speed)
(define (rail-section length max-speed components)
  (vector section length max-speed components))
(define (section-length s)
  (vector-ref s 1))
(define (section-components s)
  (vector-ref s 3))
(define (section-speed s)
  (vector-ref s 2))

(define (rail-switch max-speed type)
  (vector switch max-speed type))
(define (switch-type s)
  (vector-ref s 2))
(define (switch-speed s)
  (vector-ref s 1))

(define (rail-dswitch max-speed)
  (vector dswitch max-speed))
(define (dswitch-speed d)
  (vector-ref d 1))

(define (rail-type item)
  (vector-ref item 0))

; *************************************************************************************************************************

; Definities voor het beschrijven van de connecties tussen de elementen van de spoorweg.

; Wissels hebben soms meerdere uitgangen aan een A of B-zijde. Hiervoor kan multi-connection gebruikt worden.
; Voor zijdes met een enkel uiteinde wordt connection gebruikt.
; connection = #(type side other-part dual-connection?)
; -> side = A/B
; -> other-part = onderdeel dat verbonden moet worden.
; -> dual-connection? geeft aan welke zijde (A/B) van other-part met part verbonden moet worden of #f indien multi-connection gebruikt moet worden.
(define (connection part side other-part other-part-connection?)
  (vector single part side other-part other-part-connection?))

; Voor meerdere uitgangen aan één zijde, zoals bv. bij een wissel.
(define (multi-connection part side . otherparts-connections)
  (vector multi part side otherparts-connections))

; Accessoren
(define (connection-part v)
  (vector-ref v 1))
(define (connection-type v)
  (vector-ref v 0))
(define (connection-side v)
  (vector-ref v 2))
(define (connection-other-part v)
  (vector-ref v 3))
(define (connection-other-part-to v)
  (vector-ref v 4))

; *************************************************************************************************************************

; Definities voor het aanmaken van locomotieven en wagons

(define (new_carriage length mass use)
  (vector use mass length))
(define (new_locomotive length mass max-speed acc brake)
  (vector locomotive mass length max-speed acc brake))

(define get_mass (lambda (v)(vector-ref v 1)))
(define get_length (lambda (v)(vector-ref v 2)))
(define get_use (lambda (v)(vector-ref v 0)))
(define get_mspd (lambda (v)(vector-ref v 3)))
(define get_acc (lambda (v)(vector-ref v 4)))
(define get_brk (lambda (v)(vector-ref v 5)))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;             Section-ADT
; ***********************************

; #| SECTION-ADT |#

(define (make-section id length max-speed components log)
  ; Onderstaande lokale variabelen worden allemaal op standaardwaarden geïnitialiseerd.
  (let ((status free)
         (neighbour-A undefined)
         (neighbour-B undefined)
         (signal green)
         (reservation #f))

    ; #| Gespecialiseerde procedures |#

    ; Accessoren
    (define (get-next-section section)
      (cond ((eq? section neighbour-B) neighbour-A)
        ((eq? section neighbour-A) neighbour-B)
        (else (error 'get-next-section "logical error: wrong section provided to section ~a by ~a ~a." id (sendm section 'get-type) (sendm section 'get-id)))))

    ; #| Dispatch |#

    (define (section-dispatch message)
      (cond

        ; Accessoren
        ; Voor ieder opvraagbaar gegeven wordt er een procedure voorzien die dat gegeven teruggeeft.
        ((eq? message 'get-id)                (lambda () id))
        ((eq? message 'get-length)            (lambda () length))
        ((eq? message 'get-components)        (lambda () components))
        ((eq? message 'get-max-speed)         (lambda () max-speed))
        ((eq? message 'get-neighbour-A)       (lambda () neighbour-A))
        ((eq? message 'get-neighbour-B)       (lambda () neighbour-B))
        ((eq? message 'get-next-section)      get-next-section)
        ((eq? message 'get-signal)            (lambda () signal))
        ((eq? message 'get-type)              (lambda () section))

        ; Predicaten
        ; Voor ieder predicaat wordt er een procedure voorzien die een boolean teruggeeft.
        ((eq? message 'free?)                 (lambda () (eq? status free)))
        ((eq? message 'occupied?)             (lambda () (not (eq? status free))))
        ((eq? message 'reserved?)             (lambda () reservation))

        ; Mutatoren
        ; Voor iedere mutator wordt er een procedure voorzien die toelaat data aan te passen.
        ((eq? message 'set-max-speed!)        (lambda (speed)(set! max-speed speed)))
        ((eq? message 'set-neighbour-A!)      (lambda (new-A)(set! neighbour-A new-A)))
        ((eq? message 'set-neighbour-B!)      (lambda (new-B)(set! neighbour-B new-B)))
        ((eq? message 'set-signal!)           (lambda (s)    (set! signal s)))
        ((eq? message 'reserve!)              (lambda (t-id)
                                                (set! reservation t-id)
                                                (sendm log 'add-to-log (string-append "Section " (number->string id) " reserved for " (number->string t-id)))))
        ((eq? message 'end-reservation!)      (lambda ()
                                                (set! reservation #f)
                                                (sendm log 'add-to-log (string-append "Reservation ended for section " (number->string id)))))
        ; Het bezetten of vrijgeven van een sectie past automatisch de signalisatie van die sectie aan.
        ((eq? message 'occupy!)               (lambda (train)
                                                (set! status (sendm train 'get-id))
                                                (set! reservation (sendm train 'get-id))
                                                (set! signal red)
                                                (sendm log 'add-to-log (string-append "Section occupied: " (number->string id)))))
        ((eq? message 'release!)              (lambda ()
                                                (set! status free)
                                                (set! signal green)
                                                (set! reservation #f)
                                                (sendm log 'add-to-log (string-append "Section free: " (number->string id)))))

        (else (error 'make-section "message error: invalid message sent to section ~a: ~a." id message))))

    section-dispatch))

; *************************************************************************************************************************

(define END-OF-RAILWAY (make-section -1 0 0 '() (lambda (message) 'void))) ; We maken een speciale sectie om aan te duiden dat een bepaalde sectie doodloopt. Deze speciale sectie heeft id -1 en krijgt een "fake" log mee.
(sendm END-OF-RAILWAY 'set-neighbour-A! END-OF-RAILWAY)
(sendm END-OF-RAILWAY 'set-neighbour-B! END-OF-RAILWAY)

(define (end-of-railway? section)
  (= -1 (sendm section 'get-id)))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;             Switch-ADT
; ***********************************

; #| SWITCH-ADT |#

; Dit ADT werkt voor "één-naar-veelwissels", zoals een gewone wissel, een driewegwissel, een Y-wissel, ...

(define (make-switch id max-speed lengths-and-curves part log)
  ; lengths-and-curves is een lijst die voor iedere uitgang (B-buur) een lengte en een curve bevat, part is bv. WL of WR.
  (let ((status free)
         (neighbour-A undefined)
         (neighbours-B undefined) ; Hier wordt een vector verwacht.
         (current-state 0) ; Hier wordt een index uit bovenstaande vector verwacht.
         ; Deze index duidt aan in welke stand de wissel zich bevindt.
         (B-arity undefined) ; Hoeveel B-buren worden er verwacht?
         (lengths undefined)
         (curves undefined)
         (signal green)
         (reservation #f))

    ; #| Gespecialiseerde procedures |#

    ; Accessoren
    (define (get-neighbour-B)
      (vector-ref neighbours-B current-state))

    ; Afhankelijk van welke sectie/wissel meegegeven werd als argument, wordt de andere sectie/wissel teruggegeven.
    ; Als argument wordt dus de sectie/wissel verwacht waar de trein van komt.
    (define (get-next-section section)
      (cond ((eq? section neighbour-A) (get-neighbour-B))
        ((or (eq? section (vector-ref neighbours-B 0))
           (eq? section (vector-ref neighbours-B 1)))
          neighbour-A)
        (else (error 'get-next-section "logical error: wrong section provided to switch ~a by ~a ~a." id (sendm section 'get-type)(sendm section 'get-id)))))

    ; Volgende predicaten ondersteunen de procedures change-up! en change-down!.
    ; Ze controleren of de wissel vrij is en nog in een bepaalde richting bewogen kan worden.
    (define (can-change-up?)
      (and (eq? status free)
        (> B-arity (+ current-state 1))))

    (define (can-change-down?)
      (and (eq? status free)
        (< 0 current-state)))

    ; Mutatoren
    (define (set-neighbours-B-side! B) ; B moet een vector zijn.
      ; !! De buren moeten in juiste volgorde in de argumentlijst staan! Vanuit het standpunt van kant A moeten ze van links naar rechts opgelijst zijn.
      (if (= B-arity (vector-length B))
        (set! neighbours-B B)
        (error 'set-neighbours-B-side! "logical error: wrong amount of sections provided to ~a-ary switch ~a." B-arity id)))

    ; Volgende procedures zullen de staat van de wissel aanpassen.
    ; We voorzien twee prodecures om de "tongen" van de wissels in beide richtingen te kunnen verplaatsen.
    (define (change-up!)
      (cond ((can-change-up?)
              (set! current-state (+ current-state 1))
              (sendm log 'add-to-log (string-append "Switch state changed: " (number->string id))))
        (else (error 'change-up! "locigal error: switch ~a asked to change while occupied." id))))

    (define (change-down!)
      (cond ((can-change-down?)
              (set! current-state (- current-state 1))
              (sendm log 'add-to-log (string-append "Switch state changed: " (number->string id))))
        (else (error 'change-up! "locigal error: switch ~a asked to change while occupied." id))))

    ; Initialisatie
    (define (init-lengths-and-curves)
      (let* ((number-of-arguments (length lengths-and-curves))
              (number-of-exits (ceiling (/ number-of-arguments 2)))
              (length-vector (make-vector number-of-exits undefined))
              (curve-vector (make-vector number-of-exits undefined)))
        (let fill-vectors ((index 0)
                            (arguments lengths-and-curves))
          (cond ((< index number-of-exits)
                  ; Vul de length-vector en de curve-vector.
                  (vector-set! length-vector index (car arguments))
                  (vector-set! curve-vector index (cadr arguments))
                  (fill-vectors (+ index 1)
                    (cddr arguments)))))
        (set! lengths-and-curves '()) ; Het originele argument is niet meer nodig.
        (set! lengths length-vector)
        (set! curves curve-vector)
        (set! B-arity number-of-exits)))

    ; #| Dispatch |#

    (define (switch-dispatch message)
      (cond

        ; Accessoren
        ; Voor ieder opvraagbaar gegeven wordt er een procedure voorzien die dat gegeven teruggeeft.
        ((eq? message 'get-id)                  (lambda () id))
        ((eq? message 'get-length)              (lambda () (vector-ref lengths current-state)))
        ((eq? message 'get-lengths)             (lambda () lengths))
        ((eq? message 'get-curves)              (lambda () curves))
        ((eq? message 'get-max-speed)           (lambda () max-speed))
        ((eq? message 'get-neighbour-A)         (lambda () neighbour-A))
        ((eq? message 'get-neighbour-B)         get-neighbour-B)
        ((eq? message 'get-arity)               (lambda () B-arity))
        ((eq? message 'get-next-section)        get-next-section)
        ((eq? message 'get-possible-B-sections) (lambda () neighbours-B))
        ((eq? message 'get-signal)              (lambda () signal))
        ((eq? message 'get-type)                (lambda () switch))
        ((eq? message 'get-part)                (lambda () part))

        ; Predicaten
        ((eq? message 'free?)                   (lambda () (eq? status free)))
        ((eq? message 'occupied? )              (lambda () (not (eq? status free))))
        ((eq? message 'reserved?)               (lambda () reservation))
        ((eq? message 'can-change-up?)          can-change-up?)
        ((eq? message 'can-change-down?)        can-change-down?)
        ; Hiermee bepalen we of een bepaalde sectie/wissel aangesloten is.
        ; Normaal zal deze procedure alleen aangeroepen worden voor de B-buren (een A-buur is altijd aangesloten).
        ((eq? message 'connected?)              (lambda (section)(or (eq? (sendm section 'get-id)
                                                                       (sendm (get-neighbour-B) 'get-id))
                                                                   (eq? (sendm section 'get-id)
                                                                     (sendm neighbour-A 'get-id)))))

        ; Mutatoren
        ; Voor iedere mutator wordt er een procedure voorzien die toelaat data aan te passen.
        ((eq? message 'set-max-speed!)          (lambda (speed)(set! max-speed speed)))
        ((eq? message 'set-neighbour-A!)        (lambda (new-A)(set! neighbour-A new-A)))
        ((eq? message 'set-neighbours-B-side!)  set-neighbours-B-side!)
        ((eq? message 'set-signal!)             (lambda (s)    (set! signal s)))
        ; Volgende lambda krijgt een trein-id als argument.
        ((eq? message 'reserve!)                (lambda (t-id)
                                                  (set! reservation t-id)
                                                  (sendm log 'add-to-log (string-append "Switch " (number->string id) " reserved for " (number->string t-id)))))
        ((eq? message 'end-reservation!)        (lambda ()
                                                  (set! reservation #f)
                                                  (sendm log 'add-to-log (string-append "Reservation ended for switch " (number->string id)))))
        ((eq? message 'occupy!)                 (lambda (train)
                                                  (set! status (sendm train 'get-id))
                                                  (set! reservation (sendm train 'get-id))
                                                  (set! signal red)
                                                  (sendm log 'add-to-log (string-append "Switch occupied: " (number->string id)))))
        ((eq? message 'release!)                (lambda ()
                                                  (set! status free)
                                                  (set! signal green)
                                                  (set! reservation #f)
                                                  (sendm log 'add-to-log (string-append "Switch free: " (number->string id)))))
        ((eq? message 'change-up!)              change-up!)
        ((eq? message 'change-down!)            change-down!)

        (else (error 'make-switch "message error: invalid message sent to switch ~a: ~a" id message))))

    (init-lengths-and-curves)

    switch-dispatch))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;             Dswitch-ADT
; ***********************************

; #| Dswitch-ADT |#

; Dit ADT stelt een "double slip switch" voor, beter bekend als een Engels wissel.
; We gaan ervan uit dat een Engels wissel symmetrisch is.

(define (make-dswitch id max-speed length-straight length-bend curve log)
  ; Een trein kan twee dingen doen: hij kan rechtdoor rijden of afbuigen.
  ; Omdat we ervan uitgaan dat de wissel symmetrisch is, hebben we maar één curve nodig als argument.
  (let ((status free)
         (neighbours-A undefined) ; We verwachten hier een vector van grootte twee.
         (neighbours-B undefined)
         ; Een trein rijdt altijd van een "A-buur" naar een "B-buur" of omgekeerd.
         (current-state-A 0)
         (current-state-B 0)
         (signal green)
         (reservation #f))

    ; #| Gespecialiseerde procedures |#

    ; Accessoren
    (define (get-neighbour-A)
      (vector-ref neighbours-A current-state-A))

    (define (get-neighbour-B)
      (vector-ref neighbours-B current-state-B))

    (define (get-next-section section)
      (cond ((or (eq? section (vector-ref neighbours-A 0))
               (eq? section (vector-ref neighbours-A 1)))
              (get-neighbour-B))
        ((or (eq? section (vector-ref neighbours-B 0))
           (eq? section (vector-ref neighbours-B 1)))
          (get-neighbour-A))
        (else (error 'get-next-section "logical error: dswitch ~a is not connected to ~a ~a." id (sendm section 'get-type) (sendm section 'get-id)))))

    (define (straight-connection?)
      (or (and (= current-state-A 0)
            (= current-state-B 1))
        (and (= current-state-A 1)
          (= current-state-B 0))))

    (define (get-length)
      (if (straight-connection?)
        length-straight
        length-bend))

    ; Mutatoren
    (define (set-neighbours-A-side! vect) ; Verwacht een vector met 2 elementen.
      (set! neighbours-A vect))

    (define (set-neighbours-B-side! vect)
      (set! neighbours-B vect))

    (define (change-A!)
      (sendm log 'add-to-log (string-append "A state of dswitch changed: " (number->string id)))
      (set! current-state-A (abs (- current-state-A 1))))

    (define (change-B!)
      (sendm log 'add-to-log (string-append "B state of dswitch changed: " (number->string id)))
      (set! current-state-B (abs (- current-state-B 1))))

    ; #| Dispatch |#

    (define (dswitch-dispatch message)
      (cond

        ; Accessoren
        ; Voor ieder opvraagbaar gegeven wordt er een procedure voorzien die dat gegeven teruggeeft.
        ((eq? message 'get-id)                  (lambda () id))
        ((eq? message 'get-length)              get-length)
        ((eq? message 'get-curve)               (lambda () curve))
        ((eq? message 'get-max-speed)           (lambda () max-speed))
        ((eq? message 'get-neighbour-A)         get-neighbour-A)
        ((eq? message 'get-neighbour-B)         get-neighbour-B)
        ((eq? message 'get-next-section)        get-next-section)
        ((eq? message 'get-possible-B-sections) (lambda () neighbours-B))
        ((eq? message 'get-possible-A-sections) (lambda () neighbours-A))
        ((eq? message 'get-signal)              (lambda () signal))
        ((eq? message 'get-type)                (lambda () dswitch))

        ; Predicaten
        ((eq? message 'free?)                  (lambda () (eq? status free)))
        ((eq? message 'occupied?)              (lambda () (not (eq? status free))))
        ((eq? message 'reserved?)              (lambda () reservation))
        ; Hiermee bepalen we of een bepaalde sectie/wissel aangesloten is.
        ; Normaal zal deze procedure alleen aangeroepen worden voor de B-buren (een A-buur is altijd aangesloten).
        ((eq? message 'connected?)             (lambda (section)(or (eq? (sendm section 'get-id)
                                                                      (sendm (get-neighbour-B) 'get-id))
                                                                  (eq? (sendm section 'get-id)
                                                                    (sendm (get-neighbour-A) 'get-id)))))

        ; Mutatoren
        ; Voor iedere mutator wordt er een procedure voorzien die toelaat data aan te passen.
        ((eq? message 'set-max-speed!)         (lambda (speed)(set! max-speed speed)))
        ((eq? message 'set-neighbours-A-side!) set-neighbours-A-side!)
        ((eq? message 'set-neighbours-B-side!) set-neighbours-B-side!)
        ((eq? message 'set-signal!)            (lambda (s)    (set! signal s)))
        ; Volgende lambda krijgt een trein-id als argument.
        ((eq? message 'reserve!)               (lambda (t-id)
                                                 (set! reservation t-id)
                                                 (sendm log 'add-to-log (string-append "Dswitch " (number->string id) " reserved for " (number->string t-id)))))
        ((eq? message 'end-reservation!)       (lambda ()
                                                 (set! reservation #f)
                                                 (sendm log 'add-to-log (string-append "Reservation ended for dswitch " (number->string id)))))
        ((eq? message 'occupy!)                (lambda (train)
                                                 (set! status (sendm train 'get-id))
                                                 (set! reservation (sendm train 'get-id))
                                                 (set! signal red)
                                                 (sendm log 'add-to-log (string-append "Dswitch occupied: " (number->string id)))))
        ((eq? message 'release!)               (lambda ()
                                                 (set! status free)
                                                 (set! signal green)
                                                 (set! reservation #f)
                                                 (sendm log 'add-to-log (string-append "Dswitch free: " (number->string id)))))
        ((eq? message 'change-A!)              change-A!)
        ((eq? message 'change-B!)              change-B!)

        (else (error 'make-dswitch "message error: invalid message sent to dswitch ~a: ~a." id message))))

    dswitch-dispatch))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;           Locomotive-ADT
; ***********************************

; #| LOCOMOTIVE-ADT |#

(define (make-locomotive id length mass max-speed acc brake)
  ; acc is het acceleratievermogen van de locomotief en brake is het remvermogen van de locomotief.
  (let ((orientation 1)) ; De oriëntatie is +1 of -1 en bepaalt de rijrichting van de locomotief.

    ; #| Gespecialiseerde procedures |#

    ; Dit ADT bevat geen gespecialiseerde procedures.

    ; #| Dispatch |#

    (define (locomotive-dispatch message)
      (cond

        ; Accessoren
        ((eq? message 'get-id)              (lambda () id))
        ((eq? message 'get-length)          (lambda () length))
        ((eq? message 'get-mass)            (lambda () mass))
        ((eq? message 'get-maximum-speed)   (lambda () max-speed))
        ((eq? message 'get-type)            (lambda () locomotive))
        ((eq? message 'get-acceleration)    (lambda () acc))
        ((eq? message 'get-deceleration)    (lambda () brake))

        ; Mutatoren
        ((eq? message 'set-maximum-speed!)  (lambda (s)(set! max-speed s)))
        ((eq? message 'set-acceleration!)   (lambda (a)(set! acc a)))
        ((eq? message 'set-deceleration!)   (lambda (d)(set! brake d)))
        ((eq? message 'change-orientation!) (lambda () (set! orientation (- orientation))))

        (else (error 'make-locomotive "message error: invalid message sent to locomotive ~a." id))))

    locomotive-dispatch))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;            Carriage-ADT
; ***********************************

; #| CARRIAGE-ADT |#

(define (make-carriage id length mass use)
  ; Use stelt het "type" wagon voor, zoals passagiersrijtuig, goederenwagon, ...

  ; #| Gespecialiseerde procedures |#

  ; Dit ADT bevat geen gespecialiseerde procedures.

  ; #| DISPATCH |#

  (define (carriage-dispatch message)
    (cond

      ; Accessoren
      ((eq? message 'get-id)     (lambda () id))
      ((eq? message 'get-length) (lambda () length))
      ((eq? message 'get-mass)   (lambda () mass))
      ((eq? message 'get-use)    (lambda () use))
      ((eq? message 'get-type)   (lambda () carriage))

      ; Mutatoren
      ((eq? message 'set-mass!)  (lambda (m)(set! mass m)))

      (else (error 'make-carriage "message error: invalid message sent to carriage ~a." id))))

  carriage-dispatch)

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;              Time-ADT
; ***********************************

; #| TIME-ADT |#

(define (make-chrono)
  (let ((previous-time (current-inexact-milliseconds)))

    ; #| Gespecialiseerde procedures |#

    (define (get-delta-time)
      (let* ((current-time (current-inexact-milliseconds))
              (delta-time (- current-time previous-time))) ; Berekening van een tijdsinterval.
        (set! previous-time current-time) ; Sla de huidige tijd op om tijdens de volgende aanroep van get-delta-time gebruikt te kunnen worden.
        (/ delta-time 1000))) ; Geef het resultaat in seconden terug.

    ; #| Dispatch |#

    ; We maken hier geen gebruik van een case omdat er toch maar één boodshap mogelijk is.
    ; Omdat we niet moeten kunnen refereren naar deze dispatchprocedure, maken we gebruik van een lambda.
    (lambda (message)
      (if (eq? message 'get-delta-time)
        get-delta-time
        (error 'make-chrono "message error: invalid message sent to chrono.")))))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;              Train-ADT
; ***********************************

; #| TRAIN-ADT |#

(define (make-train id composition position-front log)
  ; composition is een lijst van tagged lists.
  ; position is de positie van de voorkant van de trein.
  (let ((total-mass undefined)
         (total-length (vector undefined undefined undefined))
         (position-rear (vector undefined undefined undefined)) ; De positie van de achterkant van de trein wordt ook bijgehouden.
         (current-speed 0.0)
         (target-speed 0.0)
         (timer undefined)
         (max-speed undefined) ; Hangt af van de maximale snelheid van de locomotieven.
         ; We gaan ervan uit dat het acceleratievermogen en het remvermogen van de trein gelijk zijn aan het minimum van de respectievelijke vermogens van de locomotieven.
         (min-acceleration undefined)
         (min-brake undefined)
         (timetable '()))

    ; #| Gespecialiseerde procedures |#

    ; Accessoren
    (define (get-braking-distance target-speed) ; Afstand nodig om af te remmen tot een bepaalde snelheid.
      (/ (- (expt (- current-speed target-speed) 2))
        (* 2 min-brake)))

    ; Mutatoren
    (define (change-orientation!)
      (if (= current-speed 0) ; Om de oriëntatie te veranderen moet de trein stilstaan.
        (begin
          (sendm log 'add-to-log (string-append "Train orientation changed: id = " (number->string id)))
          ; Draai de oriëntatie van iedere locomotief om.
          (for-each composition (lambda (element)
                                  (if (eq? (sendm element 'get-type)
                                        locomotive)
                                    (sendm element 'change-orientation!))))
          ; De verandering van oriëntatie heeft ook effect op de positie van de trein. Deze is relatief en moet dus worden aangepast.
          (let* ((front-curr-s (curr-s position-front))
                  (front-prev-s (prev-s position-front))
                  (front-dist (dist position-front))
                  (new-rear-dist (- (sendm front-curr-s 'get-length)
                                   front-dist))
                  (rear-curr-s (curr-s position-rear))
                  (rear-prev-s (prev-s position-rear))
                  (rear-dist (dist position-rear))
                  (new-front-dist (- (sendm rear-curr-s 'get-length)
                                    rear-dist)))
            ; Front wordt rear en vice versa.
            (dist! position-front new-front-dist)
            (dist! position-rear new-rear-dist)
            (curr-s! position-front rear-curr-s)
            (curr-s! position-rear front-curr-s)
            (prev-s! position-front (sendm rear-curr-s 'get-next-section rear-prev-s))
            (prev-s! position-rear (sendm front-curr-s 'get-next-section front-prev-s))))
        (error 'change-orientation! "logical error: train ~a asked to change orientation while speed nonzero." id))
      (set! composition (reverse composition))) ; De elementen staan opgelijst van de "voorkant" van de trein naar de "achterkant" van de trein en moeten dus ook omgekeerd worden.


    (define (update-position!)
      (let ((traveled-distance (calculate-move-distance)))
        ; We maken gebruik van een hulpfunctie die de posities over een bepaalde afstand zal opschuiven.
        (if (not (= 0 traveled-distance)) ; Optimalisatie.
          (begin
            (shift-position! position-front traveled-distance 'occupy-extra)
            (shift-position! position-rear traveled-distance 'release)))))

    (define (free-sections!)
      ; Geef alle secties waarop de trein stond vrij. Wordt gebruikt bij het verwijderen van de trein.
      (shift-position! position-rear total-length 'release!))

    ; Hulpprocedures voor het berekenen van de verplaatsingsafstand
    (define (calculate-move-distance)
      (if (= current-speed target-speed) ; De trein reed het voorbije tijdsinterval aan een constante snelheid.
        (* current-speed
          (sendm timer 'get-delta-time))
        (calculate-EVRB))) ; Anders gebruiken we de formules voor de eenparig versnelde rechtlijnige beweging om te bepalen hoe ver de trein reed in het tijdsinterval.

    (define (calculate-EVRB)
      ; Deze formule gaat er van uit dat alle locomotieven in een trein hetzelfde acceleratie- en remvermogen gebruiken.
      (let* ((delta-time (sendm timer 'get-delta-time))
              (acc/brake (if (> current-speed target-speed) ; Versnelt of vertraagt de trein?
                           min-brake  ; Het remvermogen moet een negatief getal zijn!
                           min-acceleration))
              (delta-v    (* delta-time acc/brake)))
        (if (<= (abs delta-v) (abs (- target-speed current-speed))) ; Heeft de trein heel het interval versneld of geremd?
          (let ((new-speed (+ current-speed delta-v)) ; We bepalen huidige snelheid van de trein.
                 (speed current-speed)) ; We stellen de oude snelheid veilig om te kunnen gebruiken in volgende berekening.
            (set! current-speed new-speed) ; Sla de huidige snelheid op.
            (+ (* speed delta-time) ; Bereken de afgelegde weg a.d.h.v. de vorige snelheid. Δx = vt+(at²/2)
              (/ (* acc/brake
                   delta-time
                   delta-time)
                2)))
          ; De trein heeft niet heel het tijdsinterval versneld of vertraagd, maar slechts gedeeltelijk tot hij de target-speed bereikt heeft.
          ; -> We splitsen het tijdsinterval op in 2: het deel waar de trein aan een veranderende snelheid reed en het deel waar de trein aan een constante snelheid reed.
          (let* ((time-variable-speed (/ (- current-speed target-speed) acc/brake)) ; time-variable-speed < delta-time
                  (speed current-speed))
            (set! current-speed target-speed)
            (+ (+ (* speed time-variable-speed); Deel waar de trein aan een veranderende snelheid reed
                 (/ (* acc/brake
                      time-variable-speed
                      time-variable-speed)
                   2))
              (* current-speed (- delta-time time-variable-speed))))))) ; Deel waar de trein aan een constante snelheid reed. current-speed is reeds aangepast.

    ; Hulpprocedure voor het veranderen van een positie
    ; Deze procedure schuift (shifts) position traveled-distance op.
    (define (shift-position! position traveled-distance flag) ; flag geeft aan of er bepaalde nevenhandelingen gedaan moeten worden.
      (let ((length-curr-section (sendm (curr-s position) 'get-length)))
        (if (>= length-curr-section (+ traveled-distance (dist position))) ; Blijft de voor-/ achterkant op dezelfde sectie/wissel?
          (dist! position (+ traveled-distance (dist position)))
          ; In het andere geval wordt minstens één nieuwe sectie betreden.
          (let continue ((new-curr-section (sendm (curr-s position) 'get-next-section (prev-s position))) ; De sectie die we gaan betreden.
                          (prev-section (curr-s position)) ; De sectie die we verlaten.
                          (new-dist (- (+ traveled-distance (dist position)) ; De afstand afgelegd op de nieuwe sectie (next-section).
                                      length-curr-section)))
            (if (and (end-of-railway? new-curr-section) ; Dit zou nooit mogen gebeuren. Het controlesysteem zou dit moeten vermijden.
                  (> new-dist 0))
              (error 'shift-position "A dead end was encountered. new-curr-section = ~a ; prev-section = ~a." new-curr-section prev-section)
              (begin
                (cond ((eq? flag 'occupy) ; Bezet een sectie (voorkant van de trein) of geef een sectie vrij (achterkant van de trein).
                        (sendm new-curr-section 'occupy! train-dispatch))
                  ((eq? flag 'occupy-extra)
                    (sendm new-curr-section 'occupy! train-dispatch)
                    (if (not (null? timetable)) ; De trein betreedt de volgende sectie van zijn diensregeling => pas de dienstregeling aan (enkel indien er een dienstregeling is).
                      (set! timetable (cdr timetable))))
                  (else (sendm prev-section 'release!)))
                ; Is nu ook de nieuwe sectie te kort? Zijn we ook deze sectie reeds gepasseerd?
                (if (> new-dist (sendm new-curr-section 'get-length))
                  ; Als ook de nieuw betreden sectie te kort is, komen we op nog een verdere sectie terecht.
                  (continue (sendm new-curr-section 'get-next-section prev-section)
                    new-curr-section
                    (- new-dist
                      (sendm new-curr-section 'get-length)))
                  ; In het andere geval kunnen we de gegevens van de positie updaten.
                  (begin (prev-s! position prev-section)
                    (curr-s! position new-curr-section)
                    (dist! position new-dist)))))))))

    ; Initialisatie
    (define (init-train)

      ; Start met het berekenen van de totale massa en lengte van de trein.
      (let accumulate ((elements composition)
                        (mass 0)
                        (length 0)
                        (speed infinity)
                        (acc infinity)    ; We gaan naar de kleinste waarden op zoek en moeten hier dus grote waarden zetten.
                        (brake infinity))
        (if (null? elements)
          (if (or (eq? acc infinity)
                (eq? brake infinity)
                (eq? speed infinity))
            (error 'init-train "No valid train composition: train ~a has no locomotive or no valid specifications are given!" id)
            (begin (set! total-mass mass)
              (set! total-length length)
              (set! max-speed speed)
              (set! min-acceleration acc)
              (set! min-brake (- brake)))) ; Het remvermogen moet een negatief getal zijn!
          (accumulate (cdr elements)
            ; get-mass en get-length worden ondersteund door zowel de locomotieven als de wagons (polymorfisme).
            (+ mass (sendm (car elements) 'get-mass))
            (+ length (sendm (car elements) 'get-length))
            (if (eq? locomotive (sendm (car elements) 'get-type))
              (min speed (sendm (car elements) 'get-maximum-speed))
              speed)
            ; get-acceleration en get-deceleration worden enkel door de locomotieven ondersteund en vereisen typechecking.
            (if (eq? locomotive (sendm (car elements) 'get-type))
              (min acc (sendm (car elements) 'get-acceleration))
              acc)
            (if (eq? locomotive (sendm (car elements) 'get-type))
              (min brake (sendm (car elements) 'get-deceleration))
              brake))))

      ; Initialiseer nu position-rear d.m.v. onderstaande stappen:
      ; 1) Stel position-rear in op position-front, maar verander prev-s en neem "het invers" van dist (verander dus de oriëntatie).
      ;    Position-front is nu gelijk aan position-rear, maar de locatie is uitgedrukt a.d.h.v. andere "voorgaande secties" (prev-s).
      (let* ((curr-s-pf (curr-s position-front)) ; pf =  position-front
              (next-s-pf (sendm curr-s-pf 'get-next-section (prev-s position-front))) ; Dit wordt de prev-s van position-rear.
              (new-dist (- (sendm curr-s-pf 'get-length)
                          (dist position-front))))
        (curr-s! position-rear curr-s-pf)
        (prev-s! position-rear next-s-pf)
        (dist! position-rear new-dist))
      ; 2) Verplaats position-rear d.m.v. change-position over een afstand van total-length.
      (shift-position! position-rear total-length 'occupy)
      ; 3) Verander terug prev-s en dist van position-rear.
      (let* ((curr-s-pr (curr-s position-rear)) ; pr = position-rear
              (next-s-pr (sendm curr-s-pr 'get-next-section (prev-s position-rear))) ; Dit wordt de nieuwe prev-s van position-rear.
              (new-dist (- (sendm curr-s-pr 'get-length)
                          (dist position-rear))))
        ; curr-s passen we niet aan want dat blijft natuurlijk hetzelfde.
        (prev-s! position-rear next-s-pr)
        (dist! position-rear new-dist))

      ; Geef nu aan dat de begin- en eindsectie bezet zijn.
      (sendm (curr-s position-front) 'occupy! train-dispatch)
      (sendm (curr-s position-rear) 'occupy! train-dispatch)

      ; Maak ten slotte een tijd-ADT aan voor deze trein.
      (set! timer (make-chrono)))

    ;#| Dispatch |#

    (define (train-dispatch message)
      (cond

        ; Accessoren
        ((eq? message 'get-id)                (lambda () id))
        ((eq? message 'get-length)            (lambda () total-length))
        ((eq? message 'get-mass)              (lambda () total-mass))
        ((eq? message 'get-current-speed)     (lambda () current-speed))
        ((eq? message 'get-target-speed)      (lambda () target-speed))
        ((eq? message 'get-composition)       (lambda () composition))
        ((eq? message 'get-timetable)         (lambda () (update-position!) timetable))
        ((eq? message 'get-position)          (lambda ()
                                                (update-position!)
                                                (combine-positions position-front   ; We geven beide posities tezamen terug.
                                                  position-rear)))
        ((eq? message 'get-braking-distance)  get-braking-distance)

        ; Mutatoren
        ((eq? message 'set-timetable!)        (lambda (t)
                                                (set! timetable t)
                                                (sendm log 'add-to-log (string-append "Timetable added to train: " (number->string id)))))
        ((eq? message 'update-position!)      update-position!)
        ((eq? message 'set-target-speed!)     (lambda (s)
                                                (update-position!)  ; Bij het aanpassen van de doelsnelheid wordt eerst de nieuwe positie berekend.
                                                (if (> s max-speed) ; Hou rekening met de maximumsnelheid van de trein.
                                                  (set! target-speed max-speed)
                                                  (set! target-speed s))))
        ((eq? message 'change-orientation!)   change-orientation!)
        ((eq? message 'free-sections!)        free-sections!)

        (else (error 'make-train "message error: invalid message sent to train ~a: ~a" id message))))

    (init-train)
    train-dispatch))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;            Railway-data
; ***********************************

; In dit bestand worden de eigenschappen van de onderdelen van de modelspoorweg vastgelegd.

; Definieer de onderdelen van de modelspoorweg als indexen in een vector.
(define G239 0)
(define G231 1)
(define G119 2)
(define G62 3)
(define R2 4)
(define R3 5)
(define R9 6)
(define R2- 7) ; R2 7,5°
(define BWL 8)
(define BWR 9)
(define WL 10)
(define WR 11)
(define DKW 12)
(define W3 13)

; We slaan voor ieder onderdeel een lengte en een kromming op. De gegevens hier zijn voornamelijk belangrijk voor de GUI.
; Voor wissels houden we telkens een lijst bij, waarbij de lengte en kromming van één mogelijkheid op dezelfde plaats in de lijsten voorkomen.
(define lengths
  (vector 239.07  ; G239
    230.93  ; G231
    119.54  ; G119
    061.88  ; G62
    220.896 ; R2
    253.291 ; R3
    237.706 ; R9
    055.224 ; R2-
    (list 283.83 220.896) ; BWL
    (list 283.83 220.896) ; BWR
    (list 239.07 237.706) ; WL
    (list 239.07 237.706) ; WR
    (list 237.706 239.07) ; DKW
    (list 237.706 239.07 237.706) ; W3
  ))

(define curves
  ; Bij wissels is de A-zijde het deel met één uiteinde.
  (vector 0 ; G239
    0 ; G231
    0 ; G119
    0 ; G62
    -30 ; R2
    -30 ; R3
    -15 ; R9
    -7.5 ; R2-
    (list 30 30)      ; BWL
    (list -30 -30)    ; BWR
    (list 0 15)       ; WL
    (list 0 -15)      ; WR
    (list 0 -15)      ; DKW
    (list 15 0 -15))) ; W3


(define (get-part-length part)
  (vector-ref lengths part))

(define (get-part-curve part)
  (vector-ref curves part))

(define (make-simulator railway log)
  (let ((railway-elements undefined)
         (first-railway-id undefined) ; Geeft aan welke id het element op positie 0 in raiway-elements heeft.
         (trains '()) ; Gelinkte lijst van treinen.
         (train-control undefined))

    ; #| Interne procedures |#

    ; Accessor voor de onderdelen van het systeem.
    (define (fetch-railway-element id)
      ; Process-railway maakt alle elementen van de spoorweg achtereenvolgens aan zodat deze opeenvolgende id's hebben.
      ; Om een element op te zoeken moet er een indexcorrectie gedaan worden, aangezien het element op positie 0 niet noodzakelijk id 0 heeft (deze id bestaat trouwens niet).
      (vector-ref railway-elements (- id first-railway-id)))

    (define (fetch-train id)
      (let search ((first (car trains))
                    (rest (cdr trains)))
        (if (= (sendm first 'get-id) id)
          first
          (if (null? rest)
            (error 'fetch-train "The simulator could not fetch train ~a." id)
            (search (car rest)
              (cdr rest))))))

    ; Inlezen van een spoorweg
    ; -> Creatie van de bijbehorende ADT's + instellen 'connecties'

    (define (fetch-indexed-element idx) ; Zoek een element m.b.v. een index i.p.v. een id.
      (vector-ref railway-elements idx))

    (define (process-railway)
      (let* ((elements (read-railway-elements railway))
              (connections (read-railway-connections railway))
              (el-count (get-elements-count elements))
              (con-count (get-connection-count connections)))
        ; We zullen alle elementen van de simulator opslaan in een vector.
        (set! railway-elements (make-vector el-count undefined))
        ; Nu maken we van ieder element dat we inlezen een ADT en we zetten het op de juiste plaats in de vector.
        (make-ADTs el-count elements)
        ; Als alle elementen gemaakt zijn kunnen de verbindingen gemaakt worden.
        (make-connections con-count connections)
        ; Controleer of ieder element twee buren heeft. Indien niet, duid met END-OF-RAILWAY aan dat het spoor doodloopt.
        (controle el-count)
        ; Doe nog wat boekhouding.
        (set! first-railway-id (sendm (vector-ref railway-elements 0) 'get-id))
        (sendm log 'add-to-log "Simulator-ADT: railway processed.")))

    ; Hulpprocedures voor process-railway

    (define (make-ADTs el-count elements)
      (let make-ADT ((idx 0))
        (cond ((< idx el-count)
                (vector-set! railway-elements idx
                  (let* ((item (read-element elements idx))
                          (new-id (generate-id))
                          (type (rail-type item)))
                    (cond ((eq? type section)
                            (make-section new-id
                              (section-length item)
                              (section-speed item)
                              (section-components item)
                              log))
                      ((eq? type switch)
                        (make-switch new-id
                          (switch-speed item)
                          (get-lengths-and-curves (switch-type item))
                          (switch-type item)
                          log))
                      ((eq? type dswitch)
                        ; Een dswitch vertegenwoordigt altijd het onderdeel "DKW".
                        (make-dswitch new-id
                          (dswitch-speed item)
                          (car (get-part-length DKW))
                          (cadr (get-part-length DKW))
                          (cadr (get-part-curve DKW))
                          log))
                      (else 'process-railway "input error: the simulator-ADT encountered an unknown railway part type at element position ~a." idx))))
                (make-ADT (+ idx 1))))))

    ; Hulpprocedure voor make-ADTs
    ; '(length1 length2 ...) + '(curve1 curve2 ...) => '(length1 curve1 length2 curve2 ...)
    (define (get-lengths-and-curves type)
      (let ((type-lengths (get-part-length type))
             (type-curves (get-part-curve type)))
        (let make ((lengths type-lengths)
                    (curves type-curves))
          (cond ((and (null? lengths)
                   (null? curves))
                    '())
            ((or (null? lengths)
               (null? curves))
              (error 'get-lengths-and-curves "data error: the system detected and error in static data."))
            (else (cons (car lengths)
                    (cons (car curves)
                      (make (cdr lengths)
                        (cdr curves)))))))))

    (define (make-connections con-count connections) ; connections zijn de beschrijvingen van de connecties die gemaakt moeten worden.
      (let make-connection ((idx 0))
        (if (< idx con-count)
          (begin
            (let* ((description (read-description connections idx))
                    ; Eerst halen we informatie uit de connectie.
                    (part (fetch-indexed-element (connection-part description)))
                    (side (connection-side description))
                    (type (connection-type description)))
              (cond ((eq? type single)
                      (let ((part-to-be-connected (fetch-indexed-element (connection-other-part description)))
                             (dual-connection? (connection-other-part-to description))
                             ; Nu beslissen we welke procedure moet toegepast worden.
                             (proc (if (eq? side A) 'set-neighbour-A! 'set-neighbour-B!)))
                        (sendm part proc part-to-be-connected)
                        (if dual-connection?
                          (sendm part-to-be-connected
                            (if (eq? A dual-connection?) ; Ook hier moet weer de juiste procedure aangeroepen worden.
                                'set-neighbour-A!
                                'set-neighbour-B!)
                            part))))
                (else ; multi-connection
                  (let ((parts-to-be-connected (map fetch-indexed-element (map car (connection-other-part description))))
                         (dual-connections? (map cadr (connection-other-part description)))
                         (proc (if (eq? side A) 'set-neighbours-A-side! 'set-neighbours-B-side!))) ; Beslissen welke boodschap verstuurd zal worden.
                    (sendm part proc (list->vector parts-to-be-connected))
                    ; Als er ook connecties in de andere richting moeten gebeuren sporen we deze hier op en voeren deze meteen uit.
                    (let make-dual-connections ((parts parts-to-be-connected)
                                                 (connections dual-connections?))
                      (if (not (null? parts))
                        (begin
                          (if (car connections)
                            (sendm (car parts)
                              (if (eq? A (car connections))
                                  'set-neighbour-A!
                                  'set-neighbour-B!)
                              part))
                          (make-dual-connections (cdr parts)(cdr connections)))))))))
            (make-connection (+ idx 1))))))

    (define (controle el-count)
      (let check-element ((idx 0))
        (cond ((< idx el-count)
                (let* ((element (fetch-indexed-element idx))
                        (neighbour-A (sendm element 'get-neighbour-A))
                        (neighbour-B (sendm element 'get-neighbour-B)))
                  (if (eq? neighbour-A undefined)
                    (sendm element 'set-neighbour-A! END-OF-RAILWAY)) ; END-OF-RAILWAY is een constante gedefinieerd in sectie-ADT.rkt.
                  (if (eq? neighbour-B undefined)
                    (sendm element 'set-neighbour-B! END-OF-RAILWAY))
                  (check-element (+ idx 1)))))))

    ; #| Interfaceprocedures |#

    (define (add-train components position)
      ; Components bevat een linked-list van elementen: '( #(locomotive length mass max-speed acc brake)
      ;                                                    #(wagontype length weight)
      ;                                                    ...) met type = locomotive/wagonfunctie (bv. kolenwagon)
      ; Position is een tweetupel dat de positie van de voorkant van de trein voorstelt (een sectie en aan welke zijde van de sectie de trein gericht staat).
      (let* ((component-ADTs (map convert-component components))
              (train (make-train (generate-id) component-ADTs (convert-position position) log)))
        (set! trains (cons train trains))) ; Voeg de trein vooraan toe aan de linked list.
      (sendm log 'add-to-log "Simulator-ADT: train added."))

    ; Hulpprocedure voor add-train
    (define (convert-component component)
      ; Is het component een locomotief of een wagon? => Maak het gepaste ADT aan.
      (if (eq? (get_use component) locomotive)
        (make-locomotive (generate-id)
          (get_length component)
          (get_mass component)
          (get_mspd component)
          (get_acc component)
          (get_brk component))
        (make-carriage (generate-id)
          (get_length component)
          (get_mass component)
          (get_use component))))

    (define (convert-position position) ; Position is een pair: (section-id . side)
      (let* ((section (fetch-railway-element (if (number? (car position)) ; Om het oproepen makkelijker te maken laten we ook toe om een object mee te geven aan convert-position.
                                               (car position)
                                               (sendm (car position) 'get-id))))
              (side (cadr position))
              (length (sendm section 'get-length))
              (prev-section undefined))
        (if (eq? side A)
          (set! prev-section (sendm section 'get-neighbour-B))
          (set! prev-section (sendm section 'get-neighbour-A)))
        (pos section prev-section length)))

    (define (delete-train id)
      (let search-and-delete ((treinen trains)
                               (result '()))
        (cond ((null? treinen)
                (error 'delete-train "The simulator was asked to delete a nonexistent train ~a." id))
          ((eq? (sendm (car treinen) 'get-id) id) ; Gevonden!
            (sendm train-control 'unreserve-sections (car treinen)) ; Hef eventuele reservaties op.
            (sendm (car treinen) 'free-sections!) ; Geef de secties waarop de trein stond vrij.
            (set! trains `(,@(cdr treinen) ,@result))) ; De volgorde van de treinen verandert, maar dat is niet erg.
          (else (search-and-delete (cdr treinen)(cons (car treinen) result)))))
      (sendm log 'add-to-log "Simulator-ADT: train deleted."))


    ; #| Dispatch |#

    (define (simulator-interface message)
      (cond

        ; Interfaceprocedures voor de GUI en het controlesysteem.
        ((eq? message 'add-train)             add-train)                     ; Voeg een trein toe aan de simulator.
        ((eq? message 'delete-train)          delete-train)                  ; Verwijder een trein uit de simulator.
        ((eq? message 'fetch-railway-element) fetch-railway-element)         ; Haal een object op a.d.h.v. een id.
        ((eq? message 'fetch-train)           fetch-train)                   ; Vraag een trein op a.d.h.v. een id.
        ((eq? message 'get-trains)            (lambda () trains))            ; Vraag alle treinen op.
        ((eq? message 'get-first-railway-id)  (lambda () first-railway-id))  ; De laagste id die aan een spoorwegelement toegekend is.
        ((eq? message 'get-elements)          (lambda () railway-elements))  ; Vraag alle spoorwegsecties en -wissels op.
        ((eq? message 'set-train-control!)             (lambda (n)(set! train-control n)))     ; Vul de ontbrekende pointer naar de train-control in.
        (else (error 'simulator-interface "message error: wrong message provided to simulator: ~a." message))))

    (process-railway)
    simulator-interface))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;               rail-control
; ***********************************

(define (make-rail-control simulator)

  ; rail-control staat in voor de veiligheid op het spoornetwerk. Het beïnvloedt daarvoor de snelheid van de treinen.
  ; Het al dan niet toegelaten zijn van een trein op een sectie hangt af van het sein van de sectie en of de sectie gereserveerd is voor een andere trein of niet.
  ; rail-control beïnvloedt de seinen zelf niet, aangezien deze afhangen van de bezetting van een spoorsectie. De seinen worden dus automatisch aangepast.

  ; Hulpprocedure om iets voor iedere trein te doen.
  (define (train-for-each proc)
    (let ((trains (sendm simulator 'get-trains)))
      (for-each (lambda (train)(proc train))
        trains)))

  ; Bepaal de snelheid die op een sectie gereden mag worden of 0 indien de trein de sectie niet mag betreden.
  (define (get-speed-on-section section t-id)
    (let ((reservation (sendm section 'reserved?)))
      (if (not (eq? reservation t-id)) ; De sectie/wissel is niet voor deze trein gereserveerd => geen doorgang toegestaan!
        0
        (sendm section 'get-max-speed))))

  ; Bepaal of we de snelheid van de trein moeten beïnvloeden en doe zo indien nodig.
  (define (decide-train-speed train)
    (let* ((current-speed (sendm train 'get-current-speed))
            (t-id (sendm train 'get-id))
            (front-position (position-front (sendm train 'get-position)))
            (current-section-front (curr-s front-position))
            (safety-margin 150) ; Veiligheidsmarge
            (stopping-distance (sendm train 'get-braking-distance 0))
            (dist-front (dist front-position))
            (prev-section-front (prev-s front-position))
            (ttable (sendm train 'get-timetable))
            ; We voorzien twee flags die aangeven of we tijdens ons onderzoek bepaald hebben dat de trein moet afremmen of mag versnellen en tot welke snelheid.
            (braking-flag (if (> current-speed (get-speed-on-section current-section-front t-id)) ; Indien de trein te snel rijdt, rem hem af!
                            (get-speed-on-section current-section-front t-id)
                            #f))
            (acceleration-flag #f))

      ; Controleer of de trein trager rijdt dan op de huidige sectie is toegestaan.
      ; Opgelet! Een trein mag niet op voorhand versnellen (maar moet wel op voorhand afremmen). Hij mag dus pas versnellen wanneer hij de sectie met de hogere maximumsnelheid betreed!
      ; De invloed van een hogere snelheid op de remafstand zal in de volgende aanroep van decide-train-speed verwerkt worden. => Dit kan mogelijk problemen veroorzaken bij een te trage iteratie van de lus!
      ; Probeer de eventuele 'schade' te beperken door de if-test in de initiatie van de braking-flag en door extra contidies in onderstaand when-statement.
      (let ((max-speed (get-speed-on-section current-section-front t-id))
             (distance-to-next-section (- (sendm current-section-front 'get-length) dist-front)))
        (if (and (> max-speed current-speed)
              (or (>= distance-to-next-section safety-margin) ; Voorkom dat de acceleration-flag nog gezet wordt vlak vooraleer de volgende sectie betreden wordt.
                (= distance-to-next-section 0))) ; Na het toevoegen van een trein staat deze echter helemaal aan het uiteinde van een sectie. De kans is klein dat deze conditie op een ander moment ook waar wordt.
          (set! acceleration-flag max-speed)))

      ; We zullen nu sectie per sectie de toekomstige weg van de trein aflopen tot we zeker zijn dat de trein al dan niet moet vertragen.
      ; Wanneer we gevonden hebben dat een trein mag vertragen, moeten we echter niet stoppen. Het zou immers kunnen dat de bepaalde vertraging niet voldoende is.
      (let predict-route ((timetable (cdr ttable)) ; Gebruik de dienstregeling voor het bepalen van de volgende secties.
                           (next-section (sendm simulator 'fetch-railway-element (car ttable)))
                           (curr-section current-section-front)
                           (distance-to-next-section (- (sendm current-section-front 'get-length) dist-front))
                           (traveled-distance 0))
        (if (or (end-of-railway? next-section) (null? timetable)) ; Het spoor loopt dood of de trein nadert het einde van zijn dienstregeling => kan de trein nog volledig stoppen?
          (if (> (- stopping-distance traveled-distance safety-margin) distance-to-next-section)
            (error 'predict-route
              "Train ~a, located at section ~a with speed ~a and distance to next section ~a, will not be able to stop at END-OF-RAILWAY or at the end of the timetable."
              t-id (sendm curr-section 'get-id) current-speed distance-to-next-section)
            ; Controleer of de trein moet beginnen vertragen.
            (if (<= (- (+ traveled-distance distance-to-next-section)
                      stopping-distance)
                  safety-margin)
              (set! braking-flag 0))) ; De trein moet tot stilstand komen.
          (if (< traveled-distance stopping-distance)
            (let ((next-section-max-speed (get-speed-on-section next-section t-id)))
              (if (and (< next-section-max-speed current-speed) ; De volgende sectie heeft een lagere toegelaten snelheid dan de huidige sectie.
                    (<= (- (+ traveled-distance distance-to-next-section)  ; Als vertragingsafstand >= (safety-margin + afgelegde afstand) => rem af!
                          (sendm train 'get-braking-distance next-section-max-speed))
                      safety-margin))
                ; We onthouden dat de trein moet vertragen.
                (set! braking-flag (if braking-flag
                                     (min braking-flag next-section-max-speed)
                                     next-section-max-speed)))
              ; Ga verder met het verzamelen van gegevens.
              (predict-route (cdr timetable)
                (sendm simulator 'fetch-railway-element (car timetable))
                next-section
                (sendm (sendm simulator 'fetch-railway-element (car timetable)) 'get-length)
                (+ traveled-distance distance-to-next-section)))
            ; Gebaseerd op de gegevens die we net verzameld hebben, beslis wat er moet gebeuren.
            (cond (braking-flag      (sendm train 'set-target-speed! braking-flag))
              (acceleration-flag (sendm train 'set-target-speed! acceleration-flag))))))))

  (define (influence-train train)
    (if (null? (sendm train 'get-timetable))
      ; In het andere geval mag de trein niet verder rijden (als hij al reed).
      (sendm train 'set-target-speed! 0)
      ; Als een trein een dienstregeling heeft mag hij verder rijden en moeten we dus zijn snelheid aanpassen.
      (decide-train-speed train)))

  ; We moeten natuurlijk alle treinen beïnvloeden.
  (define (influence-trains)
    (train-for-each influence-train))

  ; #| DISPATCH |#

  (define (rail-control-interface message)
    (cond

      ((eq? message 'influence-trains) influence-trains)
      ((eq? message else (error 'rail-control-interface "message error: unknown message sent to rail-control: ~a." message)))))

    rail-control-interface)

  ; ***********************************
  ;          Jens Van der Plas
  ;   Programmeerproject II 2015-2016
  ;                train-control
  ; ***********************************

  (define (make-train-control simulator)

    ; De train-control staat in voor het reserveren van de rijpaden en het verzetten van de wissels, zodat treinen hun dienstregeling kunnen volgen.
    ; Opgelet! Er wordt niet gecontroleerd of twee treinen in een 'deadlock' zullen komen te staan. In dat geval zullen de treinen niet meer verder kunnen rijden.
    ; Voorbeeld deadlock: twee treinen moet op elkaars sectie terechtkomen.
    ; Een deadlock kan dan enkel opgelost worden door het veranderen van de dienstregeling van één van de treinen (of van beide treinen).

    ; Momenteel nog niet ondersteund: het veranderen van de rijrichting van een trein.

    ; Drie hulpprocedures voor complete-timetable

    (define (connect-to-switch switch-obj section-obj tid sid)
      (if (not (sendm switch-obj 'connected? section-obj)) ; Als de wissel niet goed staat, dan...
        (let change-to-bottom () ; Zet de wissel in de "laagste" stand, tenzij je onderweg de juiste stand tegenkomt.
          (if (sendm switch-obj 'can-change-down?)
            (begin
              (sendm switch-obj 'change-down!)
              (if (not (sendm switch-obj 'connected? section-obj))
                (change-to-bottom)))))
        ; Zoek nu naar de juiste stand van de wissel.
        (let search-and-connect ()
          (if (not (sendm switch-obj 'connected? section-obj))
            (cond ((sendm switch-obj 'can-change-up?)
                    (sendm switch-obj 'change-up!)
                    (search-and-connect))
              (else (error 'connect-to-switch "train-control: The timetable of train ~a requires a non-existent connection to switch ~a." tid sid)))))))

    (define (connect-to-dswitch dswitch-obj section-obj tid sid)
      (if (not (sendm dswitch-obj 'connected? section-obj))
        (let* ((possible-A (sendm dswitch-obj 'get-possible-A-sections))
                (possible-B (sendm dswitch-obj 'get-possible-B-sections))
                (mem-A (vector-member section-obj possible-A))
                (mem-B (vector-member section-obj possible-B)))
          (if (not (or mem-A mem-B))
            (error 'connect-to-dswitch "train-control: The timetable of train ~a requires a non-existent connection to dswitch ~a." tid sid))
          (if mem-A
            (sendm dswitch-obj 'change-A!)
            (sendm dswitch-obj 'change-B!)))))

    ; Controleer het type en roep indien nodig de juiste hulpprocedure op.
    (define (check-and-make-connection switch-obj section-obj tid sid)
      (cond ((eq? (sendm switch-obj 'get-type) switch)
              (connect-to-switch switch-obj section-obj tid sid))
        ((eq? (sendm switch-obj 'get-type) dswitch)
          (connect-to-dswitch switch-obj section-obj tid sid))
        (else ; Switch-obj is een reguliere spoorsectie (geen wissel). Controleer de verbinding.
          (if (not (or (eq? (sendm switch-obj 'get-neighbour-A) section-obj)
                     (eq? (sendm switch-obj 'get-neighbour-B) section-obj)))
            (error 'check-and-make-connection "train-control: The timetable of train ~a requires a non-existent connection to section ~a." tid sid)))))

    (define (complete-timetable train)
      (let ((front-position (position-front (sendm train 'get-position)))
             (ttable (sendm train 'get-timetable))
             (tid (sendm train 'get-id)))
        (if (not (null? ttable))
          (begin
          (let* ((next-in-ttable (car ttable)) ; Opgelet! De huidige sectie is niet meer opgenomen in de dienstregeling (zie shift-position! in train-ADT).
                  (next-in-ttable-obj (sendm simulator 'fetch-railway-element next-in-ttable))
                  (reserved-next (sendm next-in-ttable-obj 'reserved?))) ; Haal het desbetreffende ADT op.
            (if (not reserved-next) ; Als de sectie nog niet gereserveerd is, controleren we de connectie en reserveren indien de connectie bestaat.
              (let ((curr-f (curr-s front-position))
                     (prev-f (prev-s front-position)))
                (cond ((eq? (sendm curr-f 'get-next-section prev-f) next-in-ttable-obj) ; Het volgende element in de dienstregeling is gewoon de volgende sectie.
                        (sendm next-in-ttable-obj 'reserve! tid)
                        ; Als next-in-ttable een dswitch is, moeten we controleren of deze goed staat en indien nodig de connectie bewerkstelligen.
                        ; We weten echter niet zeker of het een dswitch is, maar dat zal worden gecontroleerd door check-and-make-connection.
                        (check-and-make-connection next-in-ttable-obj curr-f tid (sendm curr-f 'get-id)))
                  (else (error 'complete-timetable "Trying to reserve section ~a, while not connected to current section ~a." next-in-ttable (sendm curr-f 'get-id))))))

            (if (eq? reserved-next tid) ; De volgende sectie/wissel werd reeds gereserveerd voor de trein.
              ; We zullen proberen tot twee secties op voorhand te reserveren voor de trein.
              (if (not (null? (cdr ttable)))
                (let* ((second-in-ttable (cadr ttable))
                        (second-in-ttable-obj (sendm simulator 'fetch-railway-element second-in-ttable))
                        (reserved-second (sendm second-in-ttable-obj 'reserved?)))
                  (if (not reserved-second)
                    (begin
                      (sendm second-in-ttable-obj 'reserve! tid)
                      ; Controleer of next-in-ttable een wissel is en of deze wissel goed staat.
                      (check-and-make-connection next-in-ttable-obj second-in-ttable-obj tid second-in-ttable)
                      ; Als second-in-ttable een wissel is, controleer of deze goed staat.
                      (check-and-make-connection second-in-ttable-obj next-in-ttable-obj tid next-in-ttable)))))))))))

  ; Wanneer we een trein verwijderen, zullen de gereserveerde secties moeten vrijgegeven worden.
  (define (unreserve-sections train)
    (let ((ttable (sendm train 'get-timetable))
           (t-id (sendm train 'get-id)))
      ; Enkel de eerste en tweede sectie uit de dienstregeling kunnen gereserveerd zijn.
      (if (not (null? ttable))
        (let ((first-in-ttable-obj (sendm simulator 'fetch-railway-element (car ttable))))
          (if (eq? t-id (sendm first-in-ttable-obj 'reserved?))
            (begin
              (sendm first-in-ttable-obj 'end-reservation!)
              (if (not (null? (cdr ttable)))
                (let ((second-in-ttable-obj (sendm simulator 'fetch-railway-element (cadr ttable))))
                  (if (eq? t-id (sendm second-in-ttable-obj 'reserved?))
                    (sendm second-in-ttable-obj 'end-reservation!))))))))))

  (define (influence-trains)
    (let ((trains (sendm simulator 'get-trains)))
      (for-each (lambda (train)
                  (complete-timetable train))
        trains)))

  (define (train-control-interface message)
    (cond

      ((eq? message 'influence-trains)   influence-trains)
      ((eq? message 'unreserve-sections) unreserve-sections)
      (else (error 'train-control-interface "message error: unknown message sent to train-control ~a." message))))

  (sendm simulator 'set-train-control! train-control-interface) ; Geef de simulator een pointer naar dit object.
  train-control-interface)

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;      Coordinates-container-ADT
; ***********************************

; Dit ADT houdt de grafische coördinaten van alle secties en wissels bij.
; Dubbele indexering: A/B + index

(define (make-coordinates-container simulator)
  (let ((A-coordinates (make-vector (vector-length (sendm simulator 'get-elements))
                           '())) ; De indexen zullen meestal klein zijn, dus we gebruiken een lijststructuur.
         (B-coordinates (make-vector (vector-length (sendm simulator 'get-elements))
                            '()))
         (first-railway-id (sendm simulator 'get-first-railway-id))) ; Elementen van de spoorweg hebben opeenvolgend id's.

    ; Kijk of er al coördinaten op een bepaalde index van een bepaalde sectie bestaan. Vervang indien ze bestaan, voeg toe indien ze niet bestaan.
    (define (change-coordinates vect section-id idx coords)
      (let* ((c (vector-ref vect (- section-id first-railway-id)))
              (res (massoc idx c)))
        (if res
          (set-mcdr! res coords)
          (vector-set! vect (- section-id first-railway-id) (mcons (mcons idx coords) c)))))

    (define (get-coordinates vect section-id idx)
      (let ((result (massoc idx (vector-ref vect (- section-id first-railway-id)))))
        (if result
          (mcdr result)
          #f)))

    (define (container-interface message)
      (cond

        ((eq? message 'get-A-coordinates)  (lambda (section-id idx) ; Idx geeft aan welke A of B-coördinaat we nodig hebben.
                                             (get-coordinates A-coordinates (if (procedure? section-id)
                                                                              (sendm section-id 'get-id)
                                                                              section-id) idx)))
        ((eq? message 'get-B-coordinates)  (lambda (section-id idx)
                                             (get-coordinates B-coordinates (if (procedure? section-id)
                                                                              (sendm section-id 'get-id)
                                                                              section-id) idx)))
        ((eq? message 'set-A-coordinates!) (lambda (section-id idx coords)
                                             (change-coordinates A-coordinates section-id idx coords)))
        ((eq? message 'set-B-coordinates!) (lambda (section-id idx coords)
                                             (change-coordinates B-coordinates section-id idx coords)))

        (else (error 'container-interface "Unknown message sent to coordinates container: ~a." message))))

    container-interface))

; **************************************************************************************************************

(define (make-coord x y)
  (vector x y))
(define (coord-x c)
  (vector-ref c 0))
(define (coord-y c)
  (vector-ref c 1))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;            coordinates-ADT
; ***********************************

(define (make-coordinates-ADT simulator railway)
  (let* ((frail-id (sendm simulator 'get-first-railway-id))
          (container (make-coordinates-container simulator))) ; Voorzie een plaats om de coördinaten op te slaan.

    ; #|
    ;  GEBRUIK VAN DE COORDINATENVECTOREN UIT DE CONTAINER
    ;
    ;  SECTIE: G239 - G239 - R0 - G231
    ;            A                  B    => Telkens op index 0.
    ;
    ;  WISSEL:           x  => in A-vector met idx 0
    ;                   / \
    ;                  y   z => in B-vector, met idx 0,1,2,...
    ;
    ;  DSWITCH:     w  x => in A-vector met idx 0,1
    ;                \/
    ;                /\
    ;               y  z => in B-vector met idx 0,1
    ; |#

    ; #| Hulpprocedures |#

    (define (get-neighbours-A object) ; Geeft een vector terug.
      (if (eq? (sendm object 'get-type) dswitch)
        (sendm object 'get-possible-A-sections)
        (vector (sendm object 'get-neighbour-A))))

    (define (get-neighbours-B object) ; Geeft een vector terug.
      (if (eq? (sendm object 'get-type) section)
        (vector (sendm object 'get-neighbour-B))
        (sendm object 'get-possible-B-sections)))

    ; #| Wiskundige hulpprocedures |#

    (define (degsin angle)
      (sin (degrees->radians angle)))

    (define (degcos angle)
      (cos (degrees->radians angle)))

    (define (degatan angle)
      (atan (degrees->radians angle)))

    (define (calculate-new-coordinates-straight-part old-coord length angle)
      (let* ((x (coord-x old-coord))
              (y (coord-y old-coord))
              (new-x (+ x
                       (* length
                         (degcos angle))))
              (new-y (+ y
                       (* length
                         (degsin angle)))))
        (make-coord new-x new-y)))

    (define (calculate-new-coordinates-bend-part old-coord part-length part-angle starting-angle)
      (let* ((x (coord-x old-coord))
              (y (coord-y old-coord))
              (new-angle (+ part-angle starting-angle))
              (new-x (+ (- x
                          (* (/ (* 180 part-length)
                               (* part-angle pi))
                            (degsin starting-angle)))
                       (* (/ (* 180 part-length)
                            (* part-angle pi))
                         (degsin new-angle))))
              (new-y (- (+ y
                          (* (/ (* 180 part-length)
                               (* part-angle pi))
                            (degcos starting-angle)))
                       (* (/ (* 180 part-length)
                            (* part-angle pi))
                         (degcos new-angle)))))
        (make-coord new-x new-y)))

    (define (add-to-todo obj prev-obj angle queue)
      (enqueue! queue (list obj prev-obj angle)))

    ; De coördinaten voor de eerste sectie opgelijst in railway werden expliciet gegeven.
    ; Deze sectie moet wel recht zijn!
    (define (init-zero todo-queue)
      (let* ((obj (sendm simulator 'fetch-railway-element frail-id))
              (components (sendm obj 'get-components))
              ; De uiteinden van sectie 0:
              (first-A (make-coord (read-railway-Ax railway)   ; 0xA
                         (read-railway-Ay railway))) ; 0yA
              (first-B (make-coord (read-railway-Bx railway)   ; 0xB
                         (read-railway-By railway))) ; 0yB
              (angle (let ((temp-angle (degatan (abs (/ (- (coord-y first-B)
                                                          (coord-y first-A))
                                                       (- (coord-x first-B)
                                                         (coord-x first-A)))))))
                       ; Eventuele correctie voor aanpassing aan het assenstelsel.
                       (if (< (coord-x first-B)(coord-x first-A))
                         (+ temp-angle 180)
                         temp-angle))))
        ; Vul de eindpunten in.
        (sendm container 'set-A-coordinates! frail-id 0 first-A)
        (sendm container 'set-B-coordinates! frail-id 0 first-B)
        ; Voeg de buren aan de queue toe.
        (add-to-todo (sendm obj 'get-neighbour-A) obj (+ angle 180) todo-queue) ; + 180 want vertrek in andere richting.
        (add-to-todo (sendm obj 'get-neighbour-B) obj angle todo-queue)))

    ; Procedure om de coördinaten van een sectie op te slaan, gegeven een nevenliggende sectie waar de coordinaten reeds van bepaald zijn en een beginhoek (t.o.v. de horizontale).
    (define (calculate-section-coordinates section-id prev-section-id angle todo-queue)
      (let* ((section-obj (sendm simulator 'fetch-railway-element section-id))
              (prev-section-obj (sendm simulator 'fetch-railway-element prev-section-id))
              (next-section-obj (sendm section-obj 'get-next-section prev-section-obj)) ; De andere buur van section.
              ; Bepaal de connectiezijden (A/B).
              (section-side (if (vector-member prev-section-obj (get-neighbours-A section-obj)) A B))
              (prev-section-side (if (vector-member section-obj (get-neighbours-A prev-section-obj)) A B))
              ; De hoeken van een onderdeel staan altijd opgelijst gezien van de A zijde naar de B zijde.
              ; Wanneer we van de B-zijde vertrekken, is een correctie noodzakelijk.
              (angle-correction (if (eq? section-side A) 1 -1))
              (components (sendm section-obj 'get-components))
              (number-of-comp (length components))
              (common-coord (sendm container ; Gemeenschappelijk grenspunt
                              (if (eq? A prev-section-side)
                                  'get-A-coordinates
                                  'get-B-coordinates)
                              prev-section-obj
                              ; Bepaal nu enkel nog de index. Dit is niet altijd 1 omdat prev-section-obj ook een wissel kan zijn.
                              (vector-member section-obj
                                ((if (eq? A prev-section-side)
                                   get-neighbours-A
                                   get-neighbours-B)
                                  prev-section-obj)))))

        ; Vul het gemeenschappelijke grenspunt in.
        (sendm container
          (if (eq? A section-side)
              'set-A-coordinates!
              'set-B-coordinates!)
          section-id
          0
          common-coord)

        ; De componenten van de sectie staan van kant A naar kant B opgelijst en moeten dus omgedraaid worden als we van kant B naar kant A werken.
        (if (eq? section-side B)
          (set! components (reverse components)))

        ; Bereken het andere eindpunt.
        (let calculate-endpoint ((component-list components)
                                  (curr-angle angle)
                                  (curr-coord common-coord))
          (if (null? component-list)
            ; Sla de coördinaat op.
            (begin (sendm container
                     (if (eq? B section-side)
                         'set-A-coordinates!
                         'set-B-coordinates!)
                     section-id
                     0
                     curr-coord)
              ; Voeg de andere buur van de sectie toe aan de queue, om deze later te behandelen.
              (add-to-todo next-section-obj section-obj curr-angle todo-queue))
            ; Ga verder met het berekenen van de coördinaat.
            (let* ((part (car component-list)) ; G231, R9 ,...
                    (p-angle (get-part-curve part))
                    (p-length (get-part-length part)))
              (calculate-endpoint (cdr component-list)
                (+ (* angle-correction p-angle) curr-angle)
                (if (= 0 p-angle)
                  (calculate-new-coordinates-straight-part curr-coord p-length curr-angle)
                  (calculate-new-coordinates-bend-part curr-coord p-length (* angle-correction p-angle) curr-angle))))))))

    ; Hulpprocedures voor calculate-switch-coordinates

    ; Bereken en sla de coördinaten op van de B-zijde van een 1-naar-veel-wissel.
    (define (calculate-1-to-many-switch-coords switch-id A-coords start-angle container queue) ; A-coords zijn de coördinaten van de A-zijde van de wissel.
      (let* ((switch-obj (sendm simulator 'fetch-railway-element switch-id))
              (part (sendm switch-obj 'get-part))
              (neighb-B (get-neighbours-B switch-obj)))
        (let calculate-coords ((idx 0)
                                (part-lengths (get-part-length part))
                                (part-angles (get-part-curve part)))
          (if (null? part-lengths)
            (if (not (null? part-angles))
              (error 'calculate-switch-coordinates "Too few lengths specified for part ~a." part))
            (let ((first-part-angle (car part-angles))
                   (first-part-length (car part-lengths)))
              (if (null? part-angles)
                (error 'calculate-switch-coordinates "Too few curves curves specified for part ~a." part))
              (if (not (sendm container 'get-B-coordinates switch-id idx)) ; Als de waarde reeds bestaat, moeten we ze niet opnieuw berekenen.
                (if (and (= idx 0)
                      (or (eq? part BWL)
                        (eq? part BWR)))
                  ; Een BWL en BWR vereisen een speciale berekeningsmethode voor index 0: combineer een G62 met een R2.
                  (let* ((angle-correction (if (eq? part BWL) -1 1))
                          (coord-inbetween (calculate-new-coordinates-straight-part A-coords (get-part-length G62) start-angle)) ; Gemeenschappelijk punt G62 en R2.
                          (new-coords (calculate-new-coordinates-bend-part coord-inbetween (get-part-length R2) (* angle-correction (get-part-curve R2)) start-angle)))
                    (sendm container
                        'set-B-coordinates!
                      switch-id
                      idx
                      new-coords))
                  (let ((new-coords (if (= 0 first-part-angle)
                                      (calculate-new-coordinates-straight-part A-coords first-part-length start-angle)
                                      (calculate-new-coordinates-bend-part A-coords first-part-length first-part-angle start-angle))))
                    (sendm container
                        'set-B-coordinates! ; De A-coördinaat is gekend, vul dus de B-coördinaat in.
                      switch-id
                      idx
                      new-coords)))
                (add-to-todo (vector-ref neighb-B idx) switch-obj (+ first-part-angle start-angle) queue)) ; Dit werkt ook voor een BWL en een BWR.
              (calculate-coords (+ idx 1)
                (cdr part-lengths)
                (cdr part-angles)))))))

    (define (calculate-switch-coordinates switch-id prev-section-id angle todo-queue)
      (let* ((switch-obj (sendm simulator 'fetch-railway-element switch-id))
              (prev-obj (sendm simulator 'fetch-railway-element prev-section-id))
              (switch-type (sendm switch-obj 'get-type))
              ; Bepaal de connectiezijden.
              (switch-side (if (vector-member prev-obj (get-neighbours-A switch-obj)) A B))
              (prev-side (if (vector-member switch-obj (get-neighbours-A prev-obj)) A B))
              (common-coord (sendm container ; Gemeenschappelijk grenspunt
                              (if (eq? A prev-side)
                                  'get-A-coordinates
                                  'get-B-coordinates)
                              prev-obj
                              ; Bepaal de index door opvragen van de buren van prev-obj.
                              (vector-member switch-obj
                                ((if (eq? A prev-side)
                                   get-neighbours-A
                                   get-neighbours-B)
                                  prev-obj)))))
        ; Vul de gemeenschappelijke coördinaat in.
        (sendm container
          (if (eq? A switch-side)
              'set-A-coordinates!
              'set-B-coordinates!)
          switch-id
          (vector-member prev-obj (if (eq? switch-side A)
                                    (get-neighbours-A switch-obj)
                                    (get-neighbours-B switch-obj)))
          common-coord)

        ; Bereken nu de andere coördinaten en sla deze op in de container.
        (cond ((eq? switch-type dswitch)
                (let* ((neighb-A (get-neighbours-A switch-obj))
                        (neighb-B (get-neighbours-B switch-obj))
                        (idx (vector-member prev-obj (if (eq? switch-side B)
                                                       neighb-B
                                                       neighb-A)))
                        (length-straight (car (get-part-length DKW)))
                        (length-curved (cadr (get-part-length DKW)))
                        (curve (cadr (get-part-curve DKW))) ; Hier is geen angle-correction nodig aangezien een DKW symmetrisch is.
                        ; A0 ---- B0    (een DKW, schematisch)
                        ;    \  /        De verbindingen tussen A0 en B0 en tussen A1 en B1 zijn gekromd.
                        ;     \/
                        ;     /\
                        ;    /  \
                        ; A1 ---- B1
                        (opposite-coords-straight (calculate-new-coordinates-straight-part common-coord length-straight angle))
                        (opposite-coords-bend (calculate-new-coordinates-bend-part common-coord length-curved curve angle))
                        (new-angle (+ angle curve))
                        (coords-same-side (calculate-new-coordinates-straight-part opposite-coords-bend length-straight (+ new-angle 180))))
                  ; Nu we de coördinaten berekend hebben, hoeven we ze gewoon nog in te vullen.
                  (sendm container
                    (if (eq? switch-side A)
                        'set-A-coordinates!
                        'set-B-coordinates!)
                    switch-id
                    (abs (- idx 1)) ; Zelfde kant, andere index.
                    coords-same-side)
                  (sendm container
                    (if (eq? switch-side A)
                        'set-B-coordinates!
                        'set-A-coordinates!)
                    switch-id
                    idx
                    opposite-coords-bend)
                  (sendm container
                    (if (eq? switch-side A)
                        'set-B-coordinates!
                        'set-A-coordinates!)
                    switch-id
                    (abs (- idx 1)) ; Andere kant, andere index.
                    opposite-coords-straight)

                  ; Voeg nu ook de nevenliggende secties toe aan de queue.
                  (add-to-todo (vector-ref (if (eq? A switch-side) neighb-A neighb-B) (abs (- idx 1)))
                    switch-obj
                    (+ new-angle 180)
                    todo-queue)
                  (add-to-todo (vector-ref (if (eq? A switch-side) neighb-B neighb-A) (abs (- idx 1)))
                    switch-obj
                    angle
                    todo-queue)
                  (add-to-todo (vector-ref (if (eq? A switch-side) neighb-B neighb-A) idx)
                    switch-obj
                    new-angle
                    todo-queue)))
          ; 1-naar-veel-wissels
          ((eq? switch-side A)
            (calculate-1-to-many-switch-coords switch-id common-coord angle container todo-queue))
          ((eq? switch-side B) ; Bereken de A-coördinaat en gebruik deze dan om alle B-coördinaten te berekenen.
            (let* ((idx (vector-member prev-obj ((if (eq? B switch-side) ; Bepaal de index.
                                                   get-neighbours-B
                                                   get-neighbours-A)
                                                  switch-obj)))
                    (switch-type (sendm switch-obj 'get-part))
                    (part-angle (- (list-ref (get-part-curve switch-type) idx))) ; angle correction
                    (part-length (list-ref (get-part-length switch-type) idx))
                    (A-coord (if (= 0 part-angle)
                               (calculate-new-coordinates-straight-part common-coord part-length angle)
                               (calculate-new-coordinates-bend-part common-coord part-length part-angle angle)))
                    (new-angle (+ angle part-angle 180))) ; Andere richting => + 180°
              ; Sla de coördinaat op.
              (sendm container
                  'set-A-coordinates!
                switch-id
                0
                A-coord)
              ; We moeten de A-buur ook nog doen.
              (add-to-todo (vector-ref (get-neighbours-A switch-obj) 0) ; Er is maar één A-buur.
                switch-obj
                (+ part-angle angle)
                todo-queue)
              (calculate-1-to-many-switch-coords switch-id A-coord new-angle container todo-queue))))))

    (define (calculate obj prev-obj angle todo-queue)
      (if (eq? (sendm obj 'get-type) section)
        (calculate-section-coordinates (sendm obj 'get-id) (sendm prev-obj 'get-id) angle todo-queue)
        (calculate-switch-coordinates (sendm obj 'get-id) (sendm prev-obj 'get-id) angle todo-queue)))

    (define (calculate-all-coordinates)
      ; We moeten nu van iedere sectie/wissel de coördinaten berekenen.
      (define todo (make-queue)) ; Alles wat we nog moeten doen komt hierin terecht.
      (define done (mutable-set))
      (define (do-all)
        (if (not (queue-empty? todo))
          (let ((next-to-do (dequeue! todo)))
            (if (not (or (set-member? done (car next-to-do))
                       (= (sendm (car next-to-do) 'get-id) -1))) ; Verhinder het berekenen van coördinaten voor END-OF-RAILWAY!
              (begin
                (set-add! done (car next-to-do))
                (calculate (car next-to-do)                 ; obj
                  (cadr next-to-do)                ; prev-obj
                  (modulo (caddr next-to-do) 360)  ; angle
                  todo)))                          ; queue
            (do-all))))
      (init-zero todo)
      (do-all))

    (define (coordinates-ADT-interface message)
      (cond

        ((eq? message 'get-A-coordinate) (lambda (obj idx)
                                           (sendm container 'get-A-coordinates obj idx)))
        ((eq? message 'get-B-coordinate) (lambda (obj idx)
                                           (sendm container 'get-B-coordinates obj idx)))

        (else (error 'coordinates-ADT-interface "Unknown message sent to ADT: ~a." message))))

    (calculate-all-coordinates)
    coordinates-ADT-interface))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;             Drawing-ADT
; ***********************************

(define (make-drawing-ADT simulator railway log)
  (let* ((coordinate-ADT (make-coordinates-ADT simulator railway))
          (window-width 1500)
          (window-height 600)
          (window-margin 50)
          (line-thickness 5)
          (font-size 16)
          (section-colour "DimGray")
          (switch-colour "Gray")
          (train-colour "Blue")
          (green-colour "Green")
          (red-colour "Red")
          (reserved-colour "DarkOrange")
          ; Eigenschappen van de spoorweg
          (total-rail-width (vector-ref railway 2))
          (total-rail-height (vector-ref railway 3))
          ; Maken van een window en layers.
          (window (make-window window-width window-height "jevdplas - spoorwegsimulator"))
          (railway-layer (window 'make-layer))
          (railway-tile (make-tile window-width window-height))
          (id-layer (window 'make-layer))
          (id-tile (make-tile window-width window-height))
          (train-layer (window 'make-layer))
          (train-tile (make-tile window-width window-height)))

    (define (get-neighbours-A object) ; Geeft een vector terug.
      (if (eq? (sendm object 'get-type) dswitch)
        (sendm object 'get-possible-A-sections)
        (vector (sendm object 'get-neighbour-A))))

    (define (get-neighbours-B object) ; Geeft een vector terug.
      (if (eq? (sendm object 'get-type) section)
        (vector (sendm object 'get-neighbour-B))
        (sendm object 'get-possible-B-sections)))

    ; Mapping van de spoorwegcoördinaten naar coördinaten voor op het scherm.
    ; Hou rekening met het andere assenstelsel.
    (define (convert-coordinate coord)
      (let* ((x (coord-x coord))
              (y (coord-y coord))
              (available-width (- window-width (* 2 window-margin)))
              (available-height (- window-height (* 2 window-margin)))
              (horizontal-scaling (/ available-width total-rail-width))
              (vertical-scaling (/ available-height total-rail-height))
              (new-x (+ window-margin (* x horizontal-scaling)))
              (new-y (- window-height ; y-as in omgekeerde zin
                       (+ window-margin (* y vertical-scaling)))))
        (make-coord new-x new-y)))

    ; Teken een sectie of wissel op een tile.
    (define (draw-section section tile thickness colour)
      (let ((neighbours-A (get-neighbours-A section))
             (neighbours-B (get-neighbours-B section)))
        ; Verbind ieder A-uiteinde met ieder B-uiteinde.
        (let outer-loop ((idx-A 0))
          (if (< idx-A (vector-length neighbours-A))
            (begin
              (define A-coord (convert-coordinate (sendm coordinate-ADT 'get-A-coordinate (sendm section 'get-id) idx-A)))
              (let inner-loop ((idx-B 0))
                (if (< idx-B (vector-length neighbours-B))
                  (begin
                    (define B-coord (convert-coordinate (sendm coordinate-ADT 'get-B-coordinate (sendm section 'get-id) idx-B)))
                    (sendm tile
                        'draw-line
                      (coord-x A-coord)
                      (coord-y A-coord)
                      (coord-x B-coord)
                      (coord-y B-coord)
                      thickness
                      colour)
                    (inner-loop (+ idx-B 1)))))
              (outer-loop (+ idx-A 1)))))))

    ; Bereken het midden van een sectie.
    (define (calculate-middle-of-section section)
      (let* ((neighbours-A (get-neighbours-A section))
              (neighbours-B (get-neighbours-B section))
              (middle-A (- (ceiling (/ (vector-length neighbours-A) 2)) 1)) ; Bepaal de index van de 'middelste buur'.
              (middle-B (- (ceiling (/ (vector-length neighbours-B) 2)) 1))
              (coord-middle-A (sendm coordinate-ADT 'get-A-coordinate (sendm section 'get-id) middle-A))
              (coord-middle-B (sendm coordinate-ADT 'get-B-coordinate (sendm section 'get-id) middle-B))
              (middle-x (/ (+ (coord-x coord-middle-A)
                             (coord-x coord-middle-B))
                          2))
              (middle-y (/ (+ (coord-y coord-middle-A)
                             (coord-y coord-middle-B))
                          2)))
        (convert-coordinate (make-coord middle-x middle-y))))

    ; Teken de tekst ongeveer in het midden van de sectie, maar iets hoger.
    (define (draw-section-id section tile fontsize colour)
      (let ((coords (calculate-middle-of-section section)))
        (sendm tile
            'draw-text
          (number->string (sendm section 'get-id))
          fontsize
          (coord-x coords)
          (- (coord-y coords) (* 2 font-size))
          colour)))

    (define (draw-railway)
      (railway-tile 'clear)
      (define elements (sendm simulator 'get-elements))
      (let drawing-loop ((idx 0))
        (if (< idx (vector-length elements))
          (begin
            (let ((element (vector-ref elements idx)))
              (draw-section element railway-tile line-thickness (if (eq? section (sendm element 'get-type))
                                                                  section-colour
                                                                  switch-colour)))
            (drawing-loop (+ idx 1))))))

    ; Teken de id's in de kleur van het sein.
    (define (draw-railway-ids)
      (id-tile 'clear)
      (define elements (sendm simulator 'get-elements))
      (let drawing-loop ((idx 0))
        (if (< idx (vector-length elements))
          (let* ((element (vector-ref elements idx))
                  (signal (sendm element 'get-signal)))
            (draw-section-id element id-tile font-size (if (eq? signal red)
                                                         red-colour
                                                         (if (sendm element 'reserved?)
                                                           reserved-colour
                                                           green-colour)))
            (drawing-loop (+ idx 1))))))

    (define (draw-trains)
      (define (draw-train train)
        (let* ((position (sendm train 'get-position))
                (front (position-front position))
                (rear (position-rear position))
                (curr-f (curr-s front)))
          ; Kleur de bezette secties helemaal, van achter naar voor.
          (let draw ((curr-r (curr-s rear))
                      (prev-r (prev-s rear)))
            (if (not (eq? curr-f prev-r))
              (begin
              (draw-section curr-r train-tile line-thickness train-colour)
              (draw (sendm curr-r 'get-next-section prev-r)
                curr-r))))
        ; Teken de id van de trein samen met zijn snelheid.
        (let* ((coords (calculate-middle-of-section curr-f))
                (id (sendm train 'get-id))
                (speed (sendm train 'get-current-speed))
                (text (string-append (number->string id) ": " (number->string speed))))
          (sendm train-tile 'draw-text text font-size (coord-x coords) (coord-y coords) train-colour))))
      (train-tile 'clear)
      (let ((trains (sendm simulator 'get-trains)))
        (for-each (lambda (train)(draw-train train)) trains)))

  (define (draw-init)
    (sendm window 'set-background! "white")
    (sendm railway-tile 'set-x! 0)
    (sendm railway-tile 'set-y! 0)
    (sendm railway-layer 'add-drawable railway-tile)
    (sendm train-tile 'set-x! 0)
    (sendm train-tile 'set-y! 0)
    (sendm train-layer 'add-drawable train-tile)
    (sendm id-tile 'set-x! 0)
    (sendm id-tile 'set-y! 0)
    (sendm id-layer 'add-drawable id-tile)
    (sendm log 'add-to-log "Graphical user interface initialised.")
    (draw-railway))

  ; Het slimme venster staat ook in voor de spellus en voor de input van het keyboard.
  ; => Laat toe callbackprocedures in te stellen.
  (define (install-update-callback fun)
    (sendm window 'set-update-callback! fun))

  (define (install-key-callback fun)
    (sendm window 'set-key-callback! fun))

  (define (GUI-interface message)
    (cond

      ((eq? message 'draw-trains)             draw-trains)
      ((eq? message 'draw-railway-ids)        draw-railway-ids)
      ((eq? message 'install-update-callback) install-update-callback)
      ((eq? message 'install-key-callback)    install-key-callback)
      (else (error 'GUI-interface "Unknown message sent to GUI: ~a." message))))

  (draw-init)
  GUI-interface))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;              Input-ADT
; ***********************************

; Dit ADT staat in voor het verkrijgen en verwerken van gebruikersinvoer.

(define (make-input-adt simulator log)

  ; Vraag wat de gebruiker wil doen.
  (define (handle-input)
    (sendm log 'add-to-log "User request.")
    (bell)
    (let ((input (get-choices-from-user "Spoorwegsimulator: actiemenu"               ; titel
                   "Selecteer de gewenste actie en druk op ok." ; message
                     '("Trein toevoegen"                          ; opties
                        "Trein verwijderen"
                        "Dienstregeling instellen"
                        "Maximumsnelheid instellen"
                        "Toon logboek"
                        "Toon credits"))))
      (cond
        ((= input 0) (add-train))
        ((= input 1) (remove-train))
        ((= input 2) (add-timetable))
        ((= input 3) (change-maximum-speed))
        ((= input 4) (show-log-content))
        ((= input 5) (show-credits)))))

  ; Vraag twee gegevens op van de gebruiker en roep een functie op met deze argumenten.
  (define (get-args-and-apply title message1 message2 function)
    (let ((first-input-string (get-text-from-user title message1)))
      (if first-input-string
        (let ((second-input-string (get-text-from-user title message2)))
          (if second-input-string
            (function first-input-string second-input-string))))))

  (define (add-train)
    (get-args-and-apply "Trein toevoegen"
      "Geef de bouw op als (list component...)\nwaarbij iedere component één van volgende indelingen heeft:\n (new_locomotive length mass max-speed acceleration brake)\n (new_carriage length mass 'use)"
      "Geef de beginpositie van de trein als volgt: '(section-id side) met side A of B."
      (lambda (components position)
        (sendm simulator
            'add-train
          (eval (read (open-input-string components)))
          (eval (read (open-input-string position)))))))

  (define (remove-train)
    (let* ((id-string (get-text-from-user "Trein verwijderen"
                        "Geef de id in van de trein die u uit de simulator wil verwijderen."))
            (id (if id-string
                  (string->number id-string)
                  #f)))
      (if id
        (sendm simulator 'delete-train id))))

  (define (add-timetable)
    (get-args-and-apply "Dienstregeling instellen"
      "Geef de id in van de trein waarvoor u een dienstregeling wil toevoegen."
      "Voer de dienstregeling in in volgende vorm: '(section-id...)"
      (lambda (id-string regeling-string)
        (sendm (sendm simulator 'fetch-train (string->number id-string))
            'set-timetable!
          (eval (read (open-input-string regeling-string)))))))

  (define (show-credits)
    (message-box "Credits"
      "Deze modelspoorsimulator werd gebouwd voor het vak \"Programmeerproject II\", gedoceerd aan de Vrije Universiteit Brussel.\nAcademiejaar 2015-2016\nMeer informatie: jevdplas@vub.ac.be"))

  (define (show-log-content)
    (let ((content (sendm log 'read-out))
           (string ""))
      (let create-string ((idx (- (vector-length content) 1))) ; Contstrueer één grote string gebaseerd op de inhoud van het logboek.
        (if (>= idx 0)
          (begin
            (if (not (eq? 'void (vector-ref content idx)))
              (set! string (string-append (vector-ref content idx) "\n" string)))
            (create-string (- idx 1)))))
      (message-box "Logboek" ; Toon de inhoud van het logboek.
        string)))

  (define (change-maximum-speed)
    (get-args-and-apply "Maximumsnelheid aanpassen"
      "Geef de id in van de sectie/wissel waarvoor u de maximumsnelheid wilt instellen."
      "Voer de nieuwe maximumsnelheid in."
      (lambda (id-string speed-string)
        (let ((speed (string->number speed-string)))
          (if (>= speed 0)
            (sendm (sendm simulator 'fetch-railway-element (string->number id-string))
                'set-max-speed! speed)
            (message-box "Fout"
              "U gaf een ongeldige snelheid in. De opgegeven snelheid mag niet negatief zijn!"))))))


  (define (input-dispatch message)
    (cond

      ((eq? message 'handle-input) handle-input)
      (else (error 'input-dispatch "Unknown message encountered: ~a." message))))

  input-dispatch)

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;               Log-ADT
; ***********************************

; Maak een logboek om size strings in op te slaan.
(define (make-log size)
  (let ((first-empty 0)
         (data (make-vector size 'void))) ; (void) == #<void>

    (define (add-to-log s)
      (if (string? s)
        (begin (vector-set! data first-empty (string-append (number->string (round (current-inexact-milliseconds))) ": " s)) ; Voeg een timestamp toe, maar verwijder de microseconden.
          (set! first-empty (modulo (+ first-empty 1) size)))
        (error 'add-to-log "String expected, but given ~a instead." s)))

    ; Geef alles terug in een vector van grootte size.
    (define (read-out)
      (let ((result (make-vector size 'void)))
        (let filling-loop ((data-idx first-empty) ; oudste data-element
                            (result-idx 0))
          (cond ((eq? 'void (vector-ref data data-idx)) ; Lege elementen slaan we niet op.
                  (filling-loop (modulo (+ data-idx 1) size) ; Ga verder, want op volgende adressen kan nog wel data staan!
                    result-idx))
            ((= data-idx (modulo (- first-empty 1) size)) ; Dit is het laatste data-element.
              (vector-set! result result-idx (vector-ref data data-idx)))
            (else
              (vector-set! result result-idx (vector-ref data data-idx))
              (filling-loop (modulo (+ data-idx 1) size)
                (+ result-idx 1)))))
        result))

    (define (log-interface message)
      (cond

        ((eq? message 'add-to-log) add-to-log)
        ((eq? message 'read-out)   read-out)
        (else (error 'log-interface "Unknown message given: ~a." message))))

    log-interface))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;               Railway
; ***********************************

(define RAILWAY
  (make-railway
    ; Secties & wissels
    (describe-elements
      ; #(type length max-speed components) -> Componenten opgelijst van A naar B-zijde.
      ; #(type max-speed switch-type)
      ; Het systeem zal automatisch interne id's aan de onderdelen toekennen!
      ;   type       length speed   elements                                temporary id
      (rail-section 1154.65  100 (list G231 G231 G231 G231 G231))              ; 00
      (rail-section 1285.58  100 (list G231 G231 G231 G231 G231 G231))         ; 01
      (rail-section 0759.87  75  (list R3 R3 R3))                              ; 02
      (rail-section 1104.48  75  (list R2 R2 R2 R2 R2))                        ; 03
      (rail-section 0220.896 75  (list R2))                                    ; 04
      (rail-section 1877.941 250 (list R3 G231 G231 G231 G231 G231 G231 G239)) ; 05
      (rail-section 1437.426 100 (list R2 G62 G231 G231 G231 G231 G231))       ; 06
      (rail-section 1154.65  100 (list G231 G231 G231 G231 G231))              ; 07
      (rail-section 0717.21  50  (list G239 G239 G239))                        ; 08
      (rail-section 0253.291 75  (list R3))                                    ; 09
      (rail-section 0531.706 50  (list G231 G62 R2))                           ; 10
      (rail-section 0220.896 50  (list R2))                                    ; 11
      (rail-section 1335.41  75  (list R2 R2 R2 R2 R2 G231))                   ; 12
      (rail-section 0759.873 75  (list R3 R3 R3))                              ; 13
      (rail-section 0803.238 50  (list G231 G231 R2- R2- G231))                ; 14
      (rail-section 0700.93  50  (list G231 G231 G239))                        ; 15
      (rail-section 0464.86  50  (list G231 G231))                             ; 16
      (rail-section 0475.776 50  (list R9 G239))                               ; 17
      (rail-section 0230.93  75  (list G231))                                  ; 18
      (rail-section 0692.79  75  (list G231 G231 G231))                        ; 19
      (rail-section 0358.61  50  (list G119 G239))                             ; 20
      (rail-section 0589.54  50  (list G231 G239 G119))                        ; 21
      (rail-section 0478.14  50  (list G239 G239))                             ; 22
      (rail-switch  50  WR)                                                    ; 23
      (rail-dswitch 50)                                                        ; 24
      (rail-switch  50  WR)                                                    ; 25
      (rail-switch  50  WL)                                                    ; 26
      (rail-switch  50  WL)                                                    ; 27
      (rail-switch  50  W3)                                                    ; 28
      (rail-switch  50  WL)                                                    ; 29
      (rail-switch  50  WR)                                                    ; 30
      (rail-switch  50  BWR)                                                   ; 31
      (rail-switch  50  BWR)                                                   ; 32
      (rail-switch  50  BWL)                                                   ; 33
      (rail-switch  50  WR)                                                    ; 34
      (rail-switch  50  WL)                                                    ; 35
      (rail-switch  50  WL)                                                    ; 36
      (rail-switch  50  WR)                                                    ; 37
      (rail-switch  50  BWR)                                                   ; 38
      (rail-switch  50  BWL)                                                   ; 39
      (rail-switch  50  BWL)                                                   ; 40
    )
    ; Verbindingen
    ; (connection part-idx part-side part-to-connect-with side-from-part-to-connect-with) -> laatste enkel mogelijk indien geen wissel of één-naar-veelwissel!
    (describe-connections
      (connection        0 B  3 A)
      (connection        1 B  2 A)
      (connection       23 A 13 B)
      (connection       20 B 26 A)
      (connection       18 A 27 A)
      (connection       18 B 28 A)
      (connection        2 B 31 A)
      (connection        7 B 33 A)
      (connection        5 B  9 A)
      (connection       34 A  6 B)
      (connection        8 A 36 A)
      (connection       37 A 38 A)
      (connection       13 A 39 A)
      (multi-connection 40 B '(9  B) '(10 B))
      (multi-connection 39 B '(40 A) '(11 B))
      (multi-connection 38 B '(11 A) '(12 A))
      (multi-connection 37 B '(35 A) '(34 #f))
      (multi-connection 35 B '(7  A) '(36 #f))
      (multi-connection 36 B '(22 A) '(35 #f))
      (multi-connection 33 B '(4  B) '(3  B))
      (multi-connection 30 B '(17 A) '(16 A))
      (multi-connection 29 B '(15 A) '(30 A))
      (multi-connection 28 B '(29 A) '(14 B) '(25 #f)) ; Driewegwissel
      (multi-connection 26 B '(19 B) '(27 #f))
      (multi-connection 27 B '(21 A) '(26 #f))
      (multi-connection 24 B '(12 B) '(23 #f))
      (multi-connection 24 A '(25 A) '(0  A))
      (multi-connection 23 B '(1  A) '(24 #f))
      (multi-connection 25 B '(19 A) '(28 #f))
      (multi-connection 31 B '(32 A) '(4  A))
      (multi-connection 32 B '(5  A) '(6  A))
      (multi-connection 34 B '(10 A) '(37 #f))
    )
    2620 ; Totale breedte
    1048 ; Totale hoogt
    ; Veronderstel een orthonormaal assenstelsel:
    ; Coördinaten van de A-zijde van sectie 0 (x en y) => Voor het tekenen op het scherm.
    1638.40 0061.9
    ; Coördinaten van de B-zijde van sectie 0.
    0483.65 0061.9
  ))

; ***********************************
;          Jens Van der Plas
;   Programmeerproject II 2015-2016
;                MAIN
; ***********************************

(define (main railway)
  (let* ((log-size 100)
          (log (make-log log-size))
          (simulator (make-simulator railway log))
          (rail-control (make-rail-control simulator))
          (train-control (make-train-control simulator))
          (gui (make-drawing-ADT simulator railway log))
          (inputhandler (make-input-adt simulator log)))

    ; Callbackfunctie
    (define (simulator-callback delta-time)
      (sendm gui 'draw-railway-ids)
      (sendm gui 'draw-trains)
      (sendm train-control 'influence-trains)
      (sendm rail-control 'influence-trains))

    ; Stel de callbackfuncties in.
    (sendm gui 'install-update-callback simulator-callback)
    (sendm gui 'install-key-callback (lambda (token)
                                       (if (eq? token #\space) ; Het duwen op de spatiebalk heeft het aanroepen van de inputhandler tot gevolg.
                                         (sendm inputhandler 'handle-input))))))

(main RAILWAY)