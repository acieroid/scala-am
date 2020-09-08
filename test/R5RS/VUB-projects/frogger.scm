;; Copyright 2015 Jens Van der Plas
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

;; Notice: this file contains a modified version of the original Frogger program written by Jens Van der Plas.

;; Dummy functions for static analyser.
(define default-random-source 'source)
(define (random-source-randomize! source) (display "randomised ") (display source) (newline))
(define random-integer random)
(define (random-real) (/ (random 100) 100))

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

(define (pass-message object message . args)
  (if (null? args)
    ((object message))
    (apply (object message) args)))

; ################# FROGGER ###################
; Jens Van der Plas - 1BA Computerwetenschappen
;                PROCS LIBRARY
; ################# 2014/15 ###################

; Volgende macro dient om berichten naar objecten te sturen.
;(define-syntax pass-message
;  (syntax-rules ()
;    ((pass-message object message argument ...)
;      ((object message) argument ...))
;    ((pass-message object message)
;      ((object message)))))

; Abstractie voor het toevoegen van en verwijderen van objecten aan de elementen van een lijst.
(define (make-tagged-list tag nr obj)
  (list tag nr obj))
(define (get-tag tagged-list)
  (car tagged-list))
(define (get-nr tagged-list)
  (cadr tagged-list))
(define (get-content tagged-list)
  (caddr tagged-list))

; De volgende procedure kijkt na of Frogger niets opgegeten heeft.
; Dat gebeurt door op de laan waar Frogger staat na te gaan of Frogger gemeenschappelijke coördinaten heeft met eetbare elementen.
(define (has-eaten? frogger road grafisch-ADT manager)
  (let* ((frogger-y (pass-message frogger 'y))
          (flx (pass-message frogger 'left-x))
          (frx (pass-message frogger 'right-x))
          (elements-on-frogger-lane (pass-message road 'lane-elements frogger-y)))

    (define (at-collision element)
      (pass-message road 'setlane-remove-element frogger-y element)
      (if (eq? (get-tag element) 'pill) ; We moeten het type van het element bepalen om te weten welke argumenten de procedure 'eaten' nodig heeft.
        (pass-message (get-content element) 'eaten grafisch-ADT frogger)
        (pass-message (get-content element) 'eaten (get-nr element) manager grafisch-ADT)))

    (define (collision-with-element? element)
      (let ((elx (pass-message (get-content element) 'left-x (get-nr element)))
             (erx (pass-message (get-content element) 'right-x (get-nr element))))
        (if (or (and (<= elx flx) (<= flx erx)) ;(<= elx flx erx)
              (and (<= elx frx) (<= frx erx)) ;(<= elx frx erx)
              (and (<= flx elx)
                (<= erx frx)))
          #t    ; -> In al deze gevallen is er een botsing,
          #f))) ;    in de andere situaties niet.

    (define (check-eaten-loop elements)
      (cond ((null? elements)) ; EINDE
        ((eq? (get-tag (car elements)) 'bush) ; Een struik is oneetbaar.
          (check-eaten-loop (cdr elements)))
        (else (if (collision-with-element? (car elements))
                (at-collision (car elements)))
          (check-eaten-loop (cdr elements)))))

    (check-eaten-loop elements-on-frogger-lane)))

; De volgende procedure kijkt na of Frogger niet met een auto gebotst heeft.
; Indien zo, herstarten we het level.
(define (check-for-collision car-left-x car-right-x lane-ref frogger level-manager)
  (if (and (pass-message frogger 'can-die?) ; Eerst wordt gecontroleerd of de kikker niet onsterfelijk is.
        (= lane-ref (pass-message frogger 'y))
        (or (and (<= car-left-x (pass-message frogger 'left-x)) (<= (pass-message frogger 'left-x) car-right-x))
          (and (<= car-left-x (pass-message frogger 'right-x)) (<= (pass-message frogger 'right-x) car-right-x))))
    (begin (pass-message level-manager 'restart-level))))

; De volgende procedure kijkt na of Frogger na verplaatsing niet met een struik zou botsen.
; Het resultaat van deze procedure is een boolean.
; newleft, newright en newy zijn de coördinaten die Frogger na verplaatsing zou hebben.
(define (would-run-into-bush? newleft newright newy road)
  (if (not (eq? (pass-message road 'lane-function newy) 'middenberm))
    #f ; Frogger komt niet op een berm terecht en struiken staan natuurlijk enkel op bermen.
    (let ((bushes (pass-message road 'lane-elements newy)))

      (define (collision? left-x right-x y) ; Deze procedure krijgt de gegevens van een struik mee en vergelijkt deze met de gegevens van Frogger.
        (and (= y newy)
          (or (<= left-x newleft right-x)
            (<= left-x newright right-x)
            (and (<= newleft left-x)
              (<= right-x newright))
            (and (<= left-x newleft)
              (<= newright right-x)))))

      ; We lopen over de gegevens van alle struiken. Vanaf het moment waarop we een botsing vaststellen geven we #t terug. Stellen we geen botsing vast geven we #f terug.
      (define (checking-loop lst)
        (cond ((null? lst) #f)
          ((collision? (pass-message (get-content (car lst)) 'left-x (get-nr (car lst)))
             (pass-message (get-content (car lst)) 'right-x (get-nr (car lst)))
             (pass-message (get-content (car lst)) 'y (get-nr (car lst))))
            #t)
          (else (checking-loop (cdr lst)))))

      (checking-loop bushes))))

; ################# FROGGER ###################
; Jens Van der Plas - 1BA Computerwetenschappen
;                 ADT LIBRARY
; ################# 2014/15 ###################

;#|
;In dit bestand staat de implementatie van alle ADT's die tot de spellogica behoren,
;met uitzondering van het manager-ADT, het level-manager-ADT en het H/B-ADT.
;ADT's bevatten:
;    * een constructor,
;    * één of meerdere accessoren,
;    * eventueel mutatoren,
;    * een tekenfunctionaliteit
;    * een dispatchprocedure.
;Het resultaat van een oproep naar een constructor is de dispatchprocedure, m.a.w. een 'object'.
;|#

;#| ############################## FROGGER ############################## |#

;#| KIKKER ADT |#

(define (make-frogger liv road im-time)
  (let ((l-x 'unspecified) ; De coördinaten zullen later aangevuld worden bij het begin of herstarten van een level.
         (r-x 'unspecified)
         (y 'unspecified)
         (status #f))
    ; Als parameters krijgt deze procedure het aantal levens voor Frogger mee, de snelweg en im-time, het aantal milliseconden dat Frogger onsterfelijk moet blijven.
    ; De status is een boolean die weergeeft of Frogger een pil opgegeten heeft. #f betekent dat de kikker geen pil opgegeten heeft.

    ; ACCESSOREN
    (define (frog-left-x) l-x)
    (define (frog-right-x) r-x)
    (define (frog-y) y)
    (define (frog-status) status)
    (define (frog-lives) liv)
    (define (can-die?) (not (frog-status))) ; De kikker kan sterven als de status #f is.

    ; MUTATOREN
    (define (setfrog-left-x newleft)(set! l-x newleft))
    (define (setfrog-right-x newright)(set! r-x newright))
    (define (setfrog-y newlane-ref)(set! y newlane-ref))
    (define (setfrog-status newstatus)(set! status newstatus))
    (define (setfrog-lives new-number-of-lives grafisch-ADT)
      (set! liv new-number-of-lives)
      (pass-message grafisch-ADT 'draw-data))

    ; ONSCHENDBAARHEID
    ; Er wordt gebruikgemaakt van een timer, die aangeeft hoe lang kikker nog onsterfelijk is.
    (define immortality-time 0)

    (define (has-eaten-pill grafisch-ADT)
      ; We zetten de status van de kikker op #t en maken de kikker blauw. De timer wordt opgehoogd.
      (setfrog-status #t)
      (pass-message grafisch-ADT 'make-frogger-blue)
      (set! immortality-time im-time))
    (define (decrease-immortality-time dt grafisch-ADT)
      (if (frog-status) ; Het verlagen van de immortality-time kan enkel als de kikker onsterfelijk is.
        (begin (set! immortality-time (if (>= 0 (- immortality-time dt))
                                        (begin (setfrog-status #f) ; Als de resterende tijd negatief is, zetten we de kikkerstatus terug op #f.
                                          (pass-message grafisch-ADT 'make-frogger-green) ; De kikker wordt dan ook weer groen en de waarde 0 wordt teruggegeven.
                                          0)
                                        (- immortality-time dt)))))) ; Anders verlagen we de resterende immortality-time.

    ; MOVE MUTATOREN
    ; Worden gebruikt om de kikker te verplaatsen.
    ; De procedure would-run-into-bush? (gedefinieerd in de PROCS-LIBRARY) controleert of de kikker na verplaatsen op een struik terecht zou komen.

    (define standard-horizontal-move-rate 25) ; De horizontale verplaatsingsafstand is de breedte van het venster gedeeld door de move rate.
    (define immortal-horizontal-move-rate 50)

    (define (move-frog-left width) ; We moeten de breedte van het scherm meegeven om te bepalen hoeveel we de kikker moeten opschuiven.
      (let* ((move-distance (if (can-die?) ; Als Frogger onsterfelijk is, beweegt hij trager.
                              (quotient width standard-horizontal-move-rate)
                              (quotient width immortal-horizontal-move-rate)))
              (new-left (- l-x move-distance))
              (new-right (- r-x move-distance)))
        (if (not (would-run-into-bush? new-left new-right y road))
          (if (>= new-left 0) ; We moeten eerst controleren of de kikker nog wel zo veel naar links kan.
            (begin (setfrog-left-x new-left)     ; Ja? Dan verplaatsen we de kikker.
              (setfrog-right-x new-right))
            (begin (setfrog-right-x (- r-x l-x)) ; Nee? We zetten de kikker tegen de linkerkant van het spelvenster.
              (setfrog-left-x 0))))))

    (define (move-frog-right width)
      (let* ((move-distance (if (can-die?)
                              (quotient width standard-horizontal-move-rate)
                              (quotient width immortal-horizontal-move-rate)))
              (new-left (+ l-x move-distance))
              (new-right (+ r-x move-distance)))
        (if (not (would-run-into-bush? new-left new-right y road))
          (if (<= new-right width) ; Eerst wordt er gecontroleerd of de kikker nog wel zo ver naar rechts kan. Indien niet wordt de kikker tegen de rechterkant van het spelvenster geplaatst.
            (begin (setfrog-left-x new-left)
              (setfrog-right-x new-right))
            (begin (setfrog-left-x (+ l-x (- width r-x)))
              (setfrog-right-x width))))))

    (define (move-frog-forward road coins level-manager)
      (if (not (= y 1)) ; Staan we op de aankomstberm?
        (if (not (would-run-into-bush? l-x r-x (- y 1) road))
          (setfrog-y (- y 1)))
        ; Als de kikker op de eindberm staat en nog eens naar voor wil verplaatsen, wordt er gecontroleerd of alle muntjes opgegeten zijn.
        ; De speler moet dus nog eens op de 'up' toets drukken als hij op de eindberm staat.
        ; Op deze manier 'verspringt' het spel niet plots naar een nieuw level wanneer de kikker naar de eindberm toespringt.
        (if (pass-message coins 'has-eaten-all-coins?)
          (pass-message level-manager 'next-level)
          (pass-message level-manager 'restart-level))))

    ; TEKENFUNCTIONALITEIT
    (define (draw-frog grafisch-ADT)
      (pass-message grafisch-ADT 'draw-frog))

    ; DISPATCH
    (define (frog-disp message)
      (cond ((eq? message 'left-x) frog-left-x)
        ((eq? message 'right-x) frog-right-x)
        ((eq? message 'y) frog-y)
        ((eq? message 'can-die?) can-die?)
        ((eq? message 'decrease-immortality-time) decrease-immortality-time)

        ((eq? message 'move-frog-left) move-frog-left)
        ((eq? message 'move-frog-right) move-frog-right)
        ((eq? message 'move-frog-forward) move-frog-forward)

        ((eq? message 'frog-status) frog-status)
        ((eq? message 'frog-lives) frog-lives)

        ((eq? message 'setfrog-left-x) setfrog-left-x)
        ((eq? message 'setfrog-right-x) setfrog-right-x)
        ((eq? message 'setfrog-y) setfrog-y)

        ((eq? message 'has-eaten-pill) has-eaten-pill)
        ((eq? message 'setfrog-status) setfrog-status)
        ((eq? message 'setfrog-lives) setfrog-lives)

        ((eq? message 'draw-frog) draw-frog)

        (else (error "frog-disp" "Unidentified procedure" message))))
    frog-disp))

;#| ############################## ROAD & RANDOM ############################## |#

;#| ROAD ADT |#

(define (make-road height random-fun-generator)
  ; Height is de hoogte van het spelvenster.
  ; random-fun-generator genereert functies (m.a.w. naar-links of naar-rechts) voor de rijstroken.
  (let ((total-number-of-lanes 'unknown) ; Dit zal later bepaald worden door de procedure new-number-of-lanes op te roepen.
         (data 'not-yet-initialised) ; We slaan alle gegevens over de snelweg op in een vector.
         (laanhoogte 'unknown)
         (car-lanes '())     ; Car-lanes zal de referentienummers van de rijstroken bevatten.
         (other-lanes '()))  ; Other-lanes zal de referentienummers van de middenberm(en) bevatten.

    ; Iedere rijstrook bevat een "upper-y", een "lower-y", een functie, een lijst met alle elementen die op de rijstrook staan en een lijst met alle auto's die op de rijstrook rijden.
    ; Frogger wordt niet opgeslagen in zo'n lijst.
    ; Om de posities (!) van de gegevens in de vector makkelijker te vinden, voorzien we enkele abstracties.
    ; Volgende procedures geven dus, gegeven een laannummer, een INTEGER terug die de plaats van een gegeven in de vector weergeeft.
    (define (u-y: lane-ref)(* (- lane-ref 1) 6))
    (define (l-y: lane-ref)(+ 1 (* (- lane-ref 1) 6)))
    (define (function: lane-ref)(+ 2 (* (- lane-ref 1) 6)))
    (define (elements: lane-ref)(+ 3 (* (- lane-ref 1) 6))) ; Dit zijn de eetbare elementen, zoals pillen, muntjes en insecten. Ook de struiken komen hier terecht.
    (define (cars: lane-ref)(+ 4 (* (- lane-ref 1) 6)))     ; Er wordt een aparte ruimte voorzien om de auto's op de rijstrook op te slaan.
    (define (tile: lane-ref) (+ 5 (* (- lane-ref 1) 6)))

    ; ACCESSOREN
    ; z is telkens het referentienummer van de laan waar gegevens van opgevraagd worden.
    (define (number-of-lanes) total-number-of-lanes)
    (define (give-car-lanes) car-lanes)
    (define (give-other-lanes) other-lanes)
    (define (lane-upper-y z)(vector-ref data (u-y: z)))
    (define (lane-lower-y z)(vector-ref data (l-y: z)))
    (define (lane-function z)(vector-ref data (function: z)))
    (define (lane-elements z)(vector-ref data (elements: z)))
    (define (lane-cars z)(vector-ref data (cars: z)))
    (define (lane-height) laanhoogte)
    (define (lane? z) (or (eq? (lane-function z) 'naar-links)
                        (eq? (lane-function z) 'naar-rechts)))
    (define (lane-tile z)(vector-ref data (tile: z)))
    (define (has-tile? z)(not (eq? #f (lane-tile z))))

    ; MUTATOREN
    ; new-number-of-lanes zorgt ervoor dat het aantal lanen bepaald wordt en de datavector geïnitialiseerd wordt.
    (define (new-number-of-lanes n grafisch-ADT)
      (set! data (make-vector (* n 6))) ; Maken van de datavector.
      (set! total-number-of-lanes n) ; Registreren van het aantal lanen.
      (set! laanhoogte (/ height n)) ; De laanhoogte bepalen.
      (initiate-road)) ; De snelweg wordt daarna terug geïnitialiseerd, m.a.w. de datavector wordt ingevuld.
    ; !! Er wordt verwacht dat de snelweg opnieuw getekend wordt!

    (define (setlane-upper-y z y)
      (vector-set! data (u-y: z) y))
    (define (setlane-lower-y z y)
      (vector-set! data (l-y: z) y))

    ; Hulpprocedure om een element uit een lijst te verwijderen. Remp uit standaard library "lists (6)".
    (define (delete element lst) ; Geeft de lijst zonder het verwijderde element.
      (let loop ((l lst))
        (if (null? l)
             l
          (if (equal? (car l) element)
            (delete element (cdr lst))
            (cons (car l) (delete element (cdr lst)))))))
     ; (remp (lambda (x)(equal? element x)) lst)) ; We gebruiken equal? omdat we lijsten moeten vergelijken.

    (define (setlane-add-element lane-reference-number e)
      (vector-set! data (elements: lane-reference-number) (cons e (lane-elements lane-reference-number))))
    (define (setlane-remove-element lane-reference-number e)
      (if (member e (lane-elements lane-reference-number))
        (vector-set! data (elements: lane-reference-number)(delete e (lane-elements lane-reference-number)))
        (error "setlane-remove-element" "Unknown element" e)))

    (define (setlane-add-car lane-ref c) ;  We ordenen de auto's op een laan per x-positie. Zo kunnen we ze makkelijker verplaatsen.
      ; z is het referentienummer de laan waarop de auto komt te staan, c is de tagged-list van de auto.
      (define (insert-at-place lst e)
        (cond ((null? lst) (cons e '()))
          ((< (pass-message (get-content e) 'left-x (get-nr e))
             (pass-message (get-content (car lst)) 'left-x (get-nr (car lst))))
            (cons e lst))
          (else (cons (car lst)
                  (insert-at-place (cdr lst) e)))))
      (vector-set! data (cars: lane-ref) (insert-at-place (lane-cars lane-ref) c)))

    (define (setlane-remove-car lane-ref c)
      (if (member c (lane-elements lane-ref))
        (vector-set! data (cars: lane-ref)(delete c (lane-cars lane-ref)))
        (error "setlane-remove-car" "Unknown car" c)))

    (define (move-most-left-element-to-right lane-ref) ; Wordt opgeroepen wanneer een auto van links naar rechts gezet wordt.
      (vector-set! data (cars: lane-ref)               ; We willen  de autolijst van een laan immers ten allen tijde gesorteerd houden.
        (let loop ((all-but-first (cdr (lane-cars lane-ref))))
          (if (null? all-but-first)
            (cons (car (lane-cars lane-ref)) '())
            (cons (car all-but-first) (loop (cdr all-but-first)))))))

    (define (move-most-right-element-to-left lane-ref) ; Wordt opgeroepen wanneer aan auto van rechts naar links gezet wordt.
      (let ((cars-but-last '())
             (last-car '()))
        (define (except-last lst res)
          (cond ((null? (cdr lst)) (set! last-car (car lst))
                  (set! cars-but-last (reverse res)))
            (else (except-last (cdr lst)(cons (car lst) res)))))
        (except-last (lane-cars lane-ref) '())
        (vector-set! data (cars: lane-ref)(cons last-car cars-but-last))))

    (define (setlane-tile lane-ref t)
      (vector-set! data (tile: lane-ref) t))

    (define (setlane-function lane-ref f)
      (vector-set! data (function: lane-ref) f))

    ; INITIALISATIE
    ; Volgende prodecure zorgt voor een (her)invulling van de data. Dient opgeroepen te worden aan het begin van een nieuw level.
    (define (initiate-road)
      ; z is telkens het referentienummer van de laan.
      (define (fill-vector z)
        (if (<= z total-number-of-lanes)
          (begin (setlane-upper-y z (* laanhoogte (- z 1))) ; upper-y berekenen en opslaan
            (setlane-lower-y z (* laanhoogte z)) ; lower-y berekenen en opslaan
            ; Aangezien P(0,0) de linkerbovenhoek is, zal u-y < l-y.
            ; We genereren een willekeurige functie voor de rijstroken.
            (setlane-function z (pass-message random-fun-generator 'give-lane-function))
            (vector-set! data (elements: z) '()) ; Er staan nog geen elementen en auto's op de rijstroken.
            (vector-set! data (cars: z) '())
            (setlane-tile z #f)
            (fill-vector (+ z 1))) ; Recursieve oproep.
          ; We maken van deze clause gebruik om enkele correcties uit te voeren voor de begin- en eindberm en voegen één of twee  middenbermen toe.
          ; Er wordt 1 middenberm toegevoegd wanneer het totaal aantal lanen oneven is. In het andere geval worden 2 middenbermen toegevoegd.
          (begin (vector-set! data (function: 1) 'eindberm)
            (if (odd? total-number-of-lanes)
              (vector-set! data (function: (inexact->exact (ceiling (/ total-number-of-lanes 2)))) 'middenberm)
              (begin (vector-set! data (function: (inexact->exact (ceiling (/ total-number-of-lanes 3)))) 'middenberm)
                (vector-set! data (function: (inexact->exact (ceiling (* 2 (/ total-number-of-lanes 3))))) 'middenberm)))
            (vector-set! data (function: total-number-of-lanes) 'startberm))))

      (define (fill-lane-lists)
        ; Deze procedure vult de lijsten car-lanes en other-lanes.
        (let loop ((z 2)
                    (lanes '())
                    (bermen '()))
          (cond  ((> z (- total-number-of-lanes 1))
                   (begin (set! car-lanes lanes)
                     (set! other-lanes bermen)))
            ((eq? (lane-function z) 'middenberm)
              (loop (+ z 1)
                lanes
                (cons z bermen)))
            (else (loop (+ z 1)
                    (cons z lanes)
                    bermen)))))

      (set! car-lanes '())
      (set! other-lanes '())
      (fill-vector 1)
      (fill-lane-lists))

    ; TEKENFUNCTIONALITEIT
    (define (draw-road grafisch-ADT)
      (pass-message grafisch-ADT 'draw-road))
    (define (delete-road grafisch-ADT)
      (pass-message grafisch-ADT 'delete-road))

    ; DISPATCH
    (define (road-disp message)
      (cond ((eq? message 'number-of-lanes) number-of-lanes)
        ((eq? message 'lane-height) lane-height)
        ((eq? message 'lane-upper-y) lane-upper-y)
        ((eq? message 'lane-lower-y) lane-lower-y)
        ((eq? message 'lane-function) lane-function)
        ((eq? message 'lane-elements) lane-elements)
        ((eq? message 'lane-cars) lane-cars)
        ((eq? message 'lane-tile) lane-tile)
        ((eq? message 'lane?) lane?)
        ((eq? message 'has-tile?) has-tile?)

        ((eq? message 'move-ltor) move-most-left-element-to-right)
        ((eq? message 'move-rtol) move-most-right-element-to-left)

        ((eq? message 'car-lanes) give-car-lanes)
        ((eq? message 'other-lanes) give-other-lanes)

        ((eq? message 'initiate) initiate-road)
        ((eq? message 'new-number-of-lanes) new-number-of-lanes)
        ((eq? message 'setlane-upper-y) setlane-upper-y)
        ((eq? message 'setlane-lower-y) setlane-lower-y)
        ((eq? message 'setlane-add-element) setlane-add-element)
        ((eq? message 'setlane-remove-element) setlane-remove-element)
        ((eq? message 'setlane-add-car) setlane-add-car)
        ((eq? message 'setlane-remove-car) setlane-remove-car)
        ((eq? message 'setlane-tile) setlane-tile)

        ((eq? message 'draw-road) draw-road)
        ((eq? message 'delete-road) delete-road)

        (else (error "road-disp" "Unidentified procedure" message))))
    road-disp))

;#| RANDOM ADT |#

; Dit ADT wordt gebruikt voor het genereren van willekeurige functies (in de zin van rollen) voor de rijstroken.
; Bij de initialisatie van een snelweg dient iedere rijstrook een willekeurige functie toebedeeld te worden.

(define (Rand)
  (let ((functions (vector 'naar-rechts 'naar-links))
         (car-types (vector 'A 'B 'C 'D)))
    (random-source-randomize! default-random-source)

    (define (give-lane-function)
      (let ((r (random-integer 2)))
        (vector-ref functions r)))
    (define (give-random-integer n)
      (+ 1 (random-integer n))) ; We krijgen een integer in het interval [1..n].
    (define (give-car-type)
      (let ((r (random-integer 4)))
        (vector-ref car-types r)))

    (define (dispatch m)
      (cond ((eq? m 'give-lane-function) give-lane-function)
        ((eq? m 'give-random-integer) give-random-integer)
        ((eq? m 'give-car-type) give-car-type)

        (error "rand-disp" "Unidentified procedure" m)))
    dispatch))

;#| ############################## EATABLE OBJECTS: COINS, INSECTS & PILLS ############################## |#

;#| COINS ADT |#

(define (make-coins n road manager h/b)
  (random-source-randomize! default-random-source)
  ; n is het aantal muntjes en value is de waarde van zo'n muntje.
  ; We zullen onze muntjes namelijk willekeurig verspreiden en willen dat deze enkel
  ; getekend worden indien ze ook daadwerkelijk op een rijstrook belanden.
  ; Er zijn dus eigenlijk maximaal n muntjes.
  ; We dienen ook de manager mee te geven om de hoogte en breedte van het scherm te weten.
  (let ((data (make-vector (* n 5)))
         (value 20)
         (breedte (pass-message manager 'get-width))
         (hoogte (pass-message manager 'get-height)))
    ; Ieder muntje bevat  2 x-coördinaten, een y-coördinaat, de status (#t betekent dat het muntje nog niet opgegeten is) en een plaats om de tile op te slaan.
    ; De waarde van alle muntjes is hetzelfde.

    ; We maken eerst enkele abstracties.
    ; Deze abstracties geven, gegeven een muntnummer z, de posities van data in de vector weer (m.a.w. de procedures hebben als returnvalue een INTEGER).
    ; Om verwarring te voorkomen eindigt hun naam met een dubbelpunt.
    (define (l-x: z)(+ 1 (* (- z 1) 5)))
    (define (r-x: z)(+ 2 (* (- z 1) 5)))
    (define (y: z)(+ 3 (* (- z 1) 5)))
    (define (status: z)(+ 4 (* (- z 1) 5)))
    (define (tile: z) (+ 0 (* (- z 1) 5)))

    ; ACCESSOREN
    (define (coin-left-x coin-ref)(vector-ref data (l-x: coin-ref)))
    (define (coin-right-x coin-ref)(vector-ref data (r-x: coin-ref)))
    (define (coin-y coin-ref)(vector-ref data (y: coin-ref)))
    (define (coin-status coin-ref)(vector-ref data (status: coin-ref)))
    (define (coin-value) value)
    (define (coin-tile coin-ref)(vector-ref data (tile: coin-ref)))
    (define (has-tile? coin-ref)(not (eq? (coin-tile coin-ref) #f)))
    (define (number-of-coins) n)
    (define (has-eaten-all-coins?)
      (let loop ((q 1))
        (cond ((> q n) #t)
          ((eq? (coin-status q) #f)(loop (+ q 1)))
          (else #f))))

    ; MUTATOREN
    (define (setcoin-left-x coin-ref newleft)(vector-set! data (l-x: coin-ref) newleft))
    (define (setcoin-right-x coin-ref newright)(vector-set! data (r-x: coin-ref) newright))
    (define (setcoin-y coin-ref newy)(vector-set! data (y: coin-ref) newy))
    (define (setcoin-status coin-ref newstatus)
      (vector-set! data (status: coin-ref) newstatus))
    (define (setcoin-tile coin-ref newtile)
      (vector-set! data (tile: coin-ref) newtile))
    (define (eaten coin-ref manager grafisch-ADT)
      (pass-message manager 'setscore value)
      (delete-coin coin-ref grafisch-ADT)
      (setcoin-status coin-ref #f))

    ; INITIATIE
    (define (initiate-coins road)
      (define (fill-vector coin-ref)
        (if (<= coin-ref n)
          (begin
            (setcoin-left-x coin-ref  (+ (random-integer (inexact->exact (ceiling (- breedte (pass-message road 'lane-height))))) 1))
            (setcoin-right-x coin-ref (+ (coin-left-x coin-ref) (* (pass-message h/b 'coin-hb)(pass-message road 'lane-height))))
            (setcoin-y coin-ref (+ (random-integer (- (pass-message road 'number-of-lanes) 2)) 2))
            (cond ((pass-message road 'lane? (coin-y coin-ref)) ; Is de laan waar het muntje opkomt een berm of een rijstrook?
                    (pass-message road 'setlane-add-element (coin-y coin-ref) (make-tagged-list 'coin coin-ref coin-disp))
                    (setcoin-status coin-ref #t))
              (else
                (setcoin-status coin-ref #f))) ; Als het muntje op een berm staat, zetten we zijn status op #f.
            (setcoin-tile coin-ref #f)
            (fill-vector (+ coin-ref 1)))))
      (fill-vector 1))

    ; TEKENFUNCTIONALITEIT
    (define (draw-coins grafisch-ADT)
      (pass-message grafisch-ADT 'draw-coins))

    (define (delete-coin z grafisch-ADT)
      (pass-message grafisch-ADT 'delete-coin z))

    (define (delete-coins grafisch-ADT)
      (let loop ((z 1))
        (if (<= z n)
          (begin (delete-coin z grafisch-ADT)
            (loop (+ z 1))))))

    ; DISPATCH
    (define (coin-disp message)
      (cond ((eq? message 'left-x) coin-left-x)
        ((eq? message 'right-x) coin-right-x)
        ((eq? message 'y) coin-y)
        ((eq? message 'coin-status) coin-status)
        ((eq? message 'coin-value) coin-value)
        ((eq? message 'number-of-coins) number-of-coins)
        ((eq? message 'has-eaten-all-coins?) has-eaten-all-coins?)
        ((eq? message 'has-tile?) has-tile?)
        ((eq? message 'coin-tile) coin-tile)

        ((eq? message 'setcoin-left-x) setcoin-left-x)
        ((eq? message 'setcoin-right-x) setcoin-right-x)
        ((eq? message 'setcoin-y) setcoin-y)
        ((eq? message 'setcoin-status) setcoin-status)
        ((eq? message 'setcoin-tile) setcoin-tile)
        ((eq? message 'initiate) initiate-coins)
        ((eq? message 'eaten) eaten)

        ((eq? message 'draw-coins) draw-coins)
        ((eq? message 'delete-coin) delete-coin)
        ((eq? message 'delete-coins) delete-coins)

        (else "coin-disp" "Unidentified procedure" message)))
    coin-disp))

;#| INSECTS ADT |#

(define (make-insects n road manager h/b random-generator)
  ; n is het aantal insecten.
  ; We zullen  de insecten willekeurig verspreiden en willen dat deze enkel
  ; getekend worden indien ze ook daadwerkelijk op een rijstrook belanden. Daarom heeft deze procedure ook de snelweg als argument.
  ; We dienen ook de manager mee te geven om de hoogte en breedte van het scherm te weten.
  (let ((data (make-vector (* n 6)))
         (insect-values (vector 15 20 30 40)) ; Voor ieder type wordt er een waarde bepaald.
         (breedte (pass-message manager 'get-width))
         (hoogte (pass-message manager 'get-height)))

    ; We maken eerst enkele abstracties.
    ; Deze abstracties geven de posities van data voor een bepaald insect, met referentienummer z, in de datavector weer
    ; De procedures hebben dus als returnvalue een INTEGER.
    ; Om verwarring te voorkomen eindigt hun naam met een dubbelpunt.
    (define (l-x: z)(+ 0 (* (- z 1) 6)))
    (define (r-x: z)(+ 1 (* (- z 1) 6)))
    (define (y: z)(+ 2 (* (- z 1) 6)))
    (define (type: z)(+ 3 (* (- z 1) 6))) ; Er zijn 4 types insecten. Ieder type heeft een vaste waarde. Een type wordt opgeslagen als een integer ∈ [1..4].
    (define (status: z)(+ 4 (* (- z 1) 6)))
    (define (tile: z) (+ 5 (* (- z 1) 6)))

    ; ACCESSOREN
    (define (insect-left-x insect-ref)(vector-ref data (l-x: insect-ref)))
    (define (insect-right-x insect-ref)(vector-ref data (r-x: insect-ref)))
    (define (insect-y insect-ref)(vector-ref data (y: insect-ref)))
    (define (insect-type insect-ref)(vector-ref data (type: insect-ref)))
    (define (insect-status insect-ref)(vector-ref data (status: insect-ref)))
    (define (insect-tile insect-ref)(vector-ref data (tile: insect-ref)))
    (define (has-tile? insect-ref)(not (eq? (insect-tile insect-ref) #f)))
    (define (number-of-insects) n)
    (define (insect-value type)(vector-ref insect-values (- type 1)))

    ; MUTATOREN
    (define (setinsect-left-x insect-ref newleft)(vector-set! data (l-x: insect-ref) newleft))
    (define (setinsect-right-x insect-ref newright)(vector-set! data (r-x: insect-ref) newright))
    (define (setinsect-y insect-ref newy)(vector-set! data (y: insect-ref) newy))
    (define (setinsect-type insect-ref newtype)(vector-set! data (type: insect-ref) newtype))
    (define (setinsect-status insect-ref newstatus)
      (vector-set! data (status: insect-ref) newstatus))
    (define (setinsect-tile insect-ref newtile)
      (vector-set! data (tile: insect-ref) newtile))
    (define (eaten insect-ref manager grafisch-ADT)
      (pass-message manager 'setscore (insect-value (insect-type insect-ref)))
      (delete-insect insect-ref grafisch-ADT)
      (setinsect-status insect-ref #f))

    ; INITIATIE
    (define (initiate-insects road)
      (define (fill-vector insect-ref)
        (if (<= insect-ref n)
          (begin
            (setinsect-left-x insect-ref  (+ (random-integer (inexact->exact (ceiling (- breedte (pass-message road 'lane-height))))) 1))
            (setinsect-type insect-ref (pass-message random-generator 'give-random-integer 4))
            ; De breedte van het insect wordt bepaald door het formaat van de afbeelding.
            (setinsect-right-x insect-ref (+ (insect-left-x insect-ref)
                                            (cond ((eq? (insect-type insect-ref) 1)(* (pass-message h/b 'insect1-hb) (pass-message road 'lane-height)))     ; Type 1
                                              ((eq? (insect-type insect-ref) 2)(* (pass-message h/b 'insect2-hb) (pass-message road 'lane-height)))     ; Type 2
                                              ((eq? (insect-type insect-ref) 3)(* (pass-message h/b 'insect3-hb) (pass-message road 'lane-height)))     ; Type 3
                                              ((eq? (insect-type insect-ref) 4)(* (pass-message h/b 'insect4-hb) (pass-message road 'lane-height))))))  ; Type 4
            (setinsect-y insect-ref (+ (random-integer (- (pass-message road 'number-of-lanes) 2)) 2))
            (cond ((pass-message road 'lane? (insect-y insect-ref)) ; Insecten komen enkel op bermen terecht.
                    (pass-message road 'setlane-add-element (insect-y insect-ref) (make-tagged-list 'insect insect-ref insect-disp))
                    (setinsect-status insect-ref #t))
              (else
                (setinsect-status insect-ref #f))) ; Als het insect op een berm staat, zetten we zijn status op #f.
            (setinsect-tile insect-ref #f)
            (fill-vector (+ insect-ref 1)))))
      (fill-vector 1))

    ; TEKENFUNCTIONALITEIT
    (define (draw-insects grafisch-ADT)
      (pass-message grafisch-ADT 'draw-insects insect-disp))
    (define (delete-insect z grafisch-ADT)
      (pass-message grafisch-ADT 'delete-insect z insect-disp))
    (define (delete-insects grafisch-ADT)
      (pass-message grafisch-ADT 'delete-insects insect-disp))

    ; DISPATCH
    (define (insect-disp message)
      (cond ((eq? message 'left-x) insect-left-x)
        ((eq? message 'right-x) insect-right-x)
        ((eq? message 'y) insect-y)
        ((eq? message 'insect-status) insect-status)
        ((eq? message 'insect-type) insect-type)
        ((eq? message 'insect-value) insect-value)
        ((eq? message 'number-of-insects) number-of-insects)
        ((eq? message 'has-tile?) has-tile?)
        ((eq? message 'insect-tile) insect-tile)

        ((eq? message 'setinsect-left-x) setinsect-left-x)
        ((eq? message 'setinsect-right-x) setinsect-right-x)
        ((eq? message 'setinsect-y) setinsect-y)
        ((eq? message 'setinsect-status) setinsect-status)
        ((eq? message 'setinsect-tile) setinsect-tile)
        ((eq? message 'initiate) initiate-insects)
        ((eq? message 'eaten) eaten)

        ((eq? message 'draw-insects) draw-insects)
        ((eq? message 'delete-insect) delete-insect)
        ((eq? message 'delete-insects) delete-insects)

        (else (error "insect-disp" "Unidentified procedure" message))))
    insect-disp))

;#| PILL ADT |#
(define (make-pill manager road h/b)
  (let ((screenwidth (pass-message manager 'get-width))
         (l-x 'unknown) ; Deze gegevens worden tijdens de initialisatie aangevuld.
         (r-x 'unknown)
         (y 'unknown)
         (stat 'unknown)) ; De status geeft weer of een pil al dan niet opgegeten is

    ; ACCESSOREN
    (define (left-x . z) l-x)   ; Het vrije argument zorgt ervoor dat de procedure 'has-eaten?' uit de PROCS-LIBRARY
    (define (right-x . z) r-x)  ; geen problemen ondervindt wanneer hij hier data opvraagt. Voor andere objecten, zoals munten en insecten,
    (define (y-waarde . z) y)   ; geeft deze procedure immers een referentienummer mee.
    (define (status) stat)

    ; MUTATOREN
    (define (setpill-left-x newleft) (set! l-x newleft))
    (define (setpill-right-x newright) (set! r-x newright))
    (define (setpill-y newy) (set! y newy))
    (define (setpill-status newstatus) (set! stat newstatus))

    (define (eaten grafisch-ADT frogger)
      (delete-pill grafisch-ADT)
      (setpill-status #f)
      (pass-message frogger 'has-eaten-pill grafisch-ADT))

    ; INITIATIE
    (define (initiate-pill)
      ; Ieder level bevat steeds één pil. Deze pil ligt niet op een berm.
      (let* ((driving-lanes (pass-message road 'car-lanes))
              (number-of-driving-lanes (length driving-lanes))
              (lane-height (pass-message road 'lane-height))
              (pill-width (* lane-height (pass-message h/b 'pill-hb))))
        (setpill-left-x (random-integer (- screenwidth (inexact->exact (round pill-width)))))
        (setpill-right-x (+ l-x pill-width))
        (setpill-y (list-ref driving-lanes (random-integer number-of-driving-lanes)))
        (pass-message road 'setlane-add-element y (make-tagged-list 'pill 'no-reference-number pill-disp)) ; Een pil heeft geen referentienummer.
        (setpill-status #t)))

    ; TEKENFUNCTIONALITEIT
    (define (draw-pill grafisch-ADT)
      (pass-message grafisch-ADT 'draw-pill pill-disp))
    (define (delete-pill grafisch-ADT)
      (if stat ; We moeten een pil enkel verwijderen als deze nog getekend is.
        (pass-message grafisch-ADT 'delete-pill)))

    ; DISPATCH
    (define (pill-disp message)
      (cond ((eq? message 'left-x) left-x)
        ((eq? message 'right-x) right-x)
        ((eq? message 'y) y-waarde)
        ((eq? message 'initiate) initiate-pill)
        ((eq? message 'draw-pill) draw-pill)
        ((eq? message 'delete-pill) delete-pill)
        ((eq? message 'eaten) eaten)

        (else (error "pill-disp" "Unknown procedure" message))))
    pill-disp))

;#| ############################## NON-EATABLE OBJECTS: BUSHES & CARS ############################## |#

;#| BUSH ADT |#
(define (make-bushes manager road h/b)
  (let* ((breedte (pass-message manager 'get-width))
          (number-of-bshs 'unspecified)
          (data 'not-yet-initialised))

    ; We maken enkele abstracies om in de gegevensvector makkelijk informatie terug te vinden.
    ; Het resultaat van volgende procedures, gegeven een struik met referentienummer z, is de plaats in de vector waar we de desbetreffende informatie kunnen terugvinden.
    (define (l-x: z)(+ 0 (* (- z 1) 4)))
    (define (r-x: z)(+ 1 (* (- z 1) 4)))
    (define (y: z)(+ 2 (* (- z 1) 4)))
    (define (tile: z)(+ 3 (* (- z 1) 4)))

    ; ACCESSOREN
    (define (bush-left-x bush-ref)(vector-ref data (l-x: bush-ref)))
    (define (bush-right-x bush-ref)(vector-ref data (r-x: bush-ref)))
    (define (bush-y bush-ref)(vector-ref data (y: bush-ref)))
    (define (bush-tile bush-ref)(vector-ref data (tile: bush-ref)))
    (define (has-tile? bush-ref)(not (eq? (bush-tile bush-ref) #f)))
    (define (number-of-bushes) number-of-bshs)

    ; MUTATOREN
    (define (setbush-left-x bush-ref newleft)(vector-set! data (l-x: bush-ref) newleft))
    (define (setbush-right-x bush-ref newright)(vector-set! data (r-x: bush-ref) newright))
    (define (setbush-y bush-ref newy)(vector-set! data (y: bush-ref) newy))
    (define (setbush-tile bush-ref newtile)(vector-set! data (tile: bush-ref) newtile))

    ; INITIALISATIE
    (define (initiate-bushes)
      (let* ((bush-lanes (pass-message road 'other-lanes)) ;Struiken komen natuurlijk enkel op bermen terecht.
              (number-of-bush-lanes (length bush-lanes))
              (bush-width (* (pass-message road 'lane-height)(pass-message h/b 'bush-hb)))
              (number-of-bushes (min (inexact->exact (round (/ (pass-message manager 'get-width)(* 2 bush-width)))) ; We beperken het maximaal aantal struiken.
                                  (* number-of-bush-lanes (- (pass-message manager 'get-level) 1)))))
        (define (filling-loop bush-ref)
          (if (<= bush-ref number-of-bushes)
            (begin (setbush-left-x bush-ref (random-integer breedte))
              (setbush-right-x bush-ref (+ (bush-left-x bush-ref) bush-width))
              (setbush-y bush-ref (list-ref bush-lanes (random-integer number-of-bush-lanes)))
              (pass-message road 'setlane-add-element (bush-y bush-ref)(make-tagged-list 'bush bush-ref bush-disp))
              (setbush-tile bush-ref #f)
              (filling-loop (+ bush-ref 1)))))
        (set! number-of-bshs number-of-bushes)
        (set! data (make-vector (* 4 number-of-bushes)))
        (filling-loop 1)))

    ; TEKENEN
    (define (draw-bushes grafisch-ADT)
      (pass-message grafisch-ADT 'draw-bushes bush-disp))
    (define (delete-bushes grafisch-ADT)
      (pass-message grafisch-ADT 'delete-bushes bush-disp))

    (define (bush-disp message)
      (cond ((eq? message 'left-x) bush-left-x)
        ((eq? message 'right-x) bush-right-x)
        ((eq? message 'y) bush-y)
        ((eq? message 'bush-tile) bush-tile)
        ((eq? message 'has-tile?) has-tile?)

        ((eq? message 'setbush-left-x) setbush-left-x)
        ((eq? message 'setbush-right-x) setbush-right-x)
        ((eq? message 'setbush-y) setbush-y)
        ((eq? message 'setbush-tile) setbush-tile)

        ((eq? message 'initiate) initiate-bushes)
        ((eq? message 'number-of-bushes) number-of-bushes)
        ((eq? message 'draw-bushes) draw-bushes)
        ((eq? message 'delete-bushes) delete-bushes)

        (else (error "bush-disp" "Unknown procedure" message))))
    bush-disp))

;#| CARS ADT |#

(define (make-cars road manager frogger h/b R) ; Met n het aantal auto's per rijstrook, speed een basissnelheid voor de auto's en R het Random-ADT.
  (let ((data 'not-initialised)       ; Deze gegevens worden bij initialisatie ingevuld.
         (speed 'not-yet-initialised)
         (number-of-cars 'unspecified)
         (level-manager 'null))        ; Wordt aan het begin van het spel ingevuld.
    (random-source-randomize! default-random-source)

    ; AUTOTYPES
    ; Type A: Deze soort versnelt wanneer Frogger op dezelfde laan terecht komt. Dit zijn de gele auto's.
    ; Type B: Deze soort remt af wanneer Frogger op dezelfde laan terecht komt. Dit zijn de grijze auto's.
    ; Type C: Deze auto versnelt/vertraagt op willekeurige basis.  Dit zijn de witte auto's.
    ; Type D: Deze auto rijdt aan een constante snelheid. Dit zijn de rode auto's.

    ; We maken eerst enkele abstracties.
    ; Deze abstracties geven de posities van data, gegeven een autoreferentienummer z, in de gegevensvector weer.
    ; Deze procedures hebben dus als resultaat een INTEGER. Om verwarring te voorkomen eindigt hun naam met een dubbelpunt.
    (define (type: z)(* (- z 1) 5))
    (define (l-x: z)(+ 1 (* (- z 1) 5)))
    (define (r-x: z)(+ 2 (* (- z 1) 5)))
    (define (y: z)(+ 3 (* (- z 1) 5)))
    (define (tile: z) (+ 4 (* (- z 1) 5)))

    ; ACCESSOREN
    (define (car-type car-ref)(vector-ref data (type: car-ref)))
    (define (car-left-x car-ref)(vector-ref data (l-x: car-ref)))
    (define (car-right-x car-ref)(vector-ref data (r-x: car-ref)))
    (define (car-y car-ref)(vector-ref data (y: car-ref)))
    (define (car-tile car-ref)(vector-ref data (tile: car-ref)))
    (define (has-tile? car-ref)(not (eq? (car-tile car-ref) #f)))
    (define (give-number-of-cars) number-of-cars)

    ; MUTATOREN
    (define (setcar-type car-ref newtype)(vector-set! data (type: car-ref) newtype))
    (define (setcar-left-x car-ref newleft)(vector-set! data (l-x: car-ref) newleft))
    (define (setcar-right-x car-ref newright)(vector-set! data (r-x: car-ref) newright))
    (define (setcar-y car-ref newy)(vector-set! data (y: car-ref) newy))
    (define (setcar-tile car-ref newtile)(vector-set! data (tile: car-ref) newtile))
    (define (setcar-speed newspeed) (set! speed newspeed))
    (define (set-level-manager lm)(set! level-manager lm))

    ; INITIALISATIE
    (define (initiate-cars)
      (let* ((car-lanes (pass-message road 'car-lanes))
              (window-width (pass-message manager 'get-width))
              (car-width (* (pass-message h/b 'car-hb)(pass-message road 'lane-height))) ; Hier wordt de breedte van een auto berekend. Deze hangt af van de gebruikte afbeelding.
              (frogger-width (- (pass-message frogger 'right-x)(pass-message frogger 'left-x))) ; We bepalen de breedte van de kikker.
              (max-number-of-cars (min (inexact->exact (round (ceiling (/ (pass-message manager 'get-level) 2)))) ; We beperken het maximaal aantal auto's per laan (!), om er zeker van te zijn dat
                                    (inexact->exact (round (/ window-width (+ car-width frogger-width 100)))))); Frogger steeds zal kunnen passeren.
              ; Alle auto's worden bij initialisatie verdeeld over de breedte van het spelvenster. Als er bv. 2 auto's op één rijstrook staan, dan zullen deze auto's elk op een andere
              ; helft van het venster terechtkomen.
              (car-position-range (/ window-width max-number-of-cars))
              (car-move-range (- car-position-range car-width))
              (filling-counter 1)) ; De filling counter wordt gebruikt omdat alle auto's een apart referentienummer nodig hebben. De counter wordt dan gebruikt om bij te houden
        ; aan welk nummer we al zitten.

        (define (fill-vector-for-a-lane z lane-ref) ; z geeft weer hoeveel auto's we op deze rijstrook al geïnitialiseerd hebben.
          (if (<= z max-number-of-cars)
            (begin
              (setcar-type filling-counter (pass-message R 'give-car-type))
              (setcar-left-x filling-counter (+ (* (- z 1) car-position-range) (random-integer (inexact->exact (round car-move-range))))) ; We verdelen de auto's over de rijstrook.
              (setcar-right-x filling-counter (+ (car-left-x filling-counter) car-width))
              (setcar-y filling-counter lane-ref)
              (pass-message road 'setlane-add-car (car-y filling-counter)(make-tagged-list 'car filling-counter car-disp))
              (setcar-tile filling-counter #f)
              (set! filling-counter (+ filling-counter 1))
              (fill-vector-for-a-lane (+ z 1) lane-ref))
            (set! number-of-cars (- filling-counter 1))))
        (set! data (make-vector (* 5 max-number-of-cars (length car-lanes))))
        (for-each (lambda (x)(fill-vector-for-a-lane 1 x)) ; We passen fill-vector-for-a-lane toe voor iedere rijstrook.
          car-lanes)))

    (define (move-cars dt grafisch-ADT)

      (define car-width (- (car-right-x 1)(car-left-x 1))) ; We bepalen de breedte van de auto's a.d.h.v. gegevens van de eerste auto.

      (define (calculate-move-distance type car-y move-direction) ; We berekenen hier het aantal pixels waarover de auto verplaatst dient te worden.
        (* (if (equal? move-direction 'naar-rechts)
             1
             -1)
          (cond ((equal? type 'A)
                  (if (equal? car-y (pass-message frogger 'y))
                    (* dt (* speed 1.5))     ; Als deze auto op dezelfde laan dan Frogger is, versnelt de auto tot 1,5 keer de standaardsnelheid.
                    (* dt (* speed 0.8))))   ; Zoniet rijdt de auto iets trager dan de standaardsnelheid.
            ((equal? type 'B)
              (if (equal? car-y (pass-message frogger 'y))
                (* dt (/ speed 2))       ; Als deze auto op dezelfde laan dan Frogger is, vertraagt de auto tot de helft van de standaardsnelheid.
                (* dt (* speed 1.2))))   ; Zoniet rijdt de auto iets sneller dan de standaardsnelheid.
            ((equal? type 'C) ; Type C rijdt met een variabele snelheid.
              (* dt (+ (/ speed 2.5) (* 1.5 speed (random-real)))))
            (else (* speed dt))))) ; Type D rijdt aan een constante snelheid.

      (define (move-cars-on-lane lane-ref) ; Deze procedure verplaatst de auto's op één rijstrook.
        (let loop ((autos (if (eq? (pass-message road 'lane-function lane-ref) 'naar-links)
                            (cons 'first (pass-message road 'lane-cars lane-ref))
                            (cons 'first (reverse (pass-message road 'lane-cars lane-ref))))))
          ; De auto's worden één voor één verplaatst.
          (cond ((null? (cdr autos))) ; stopconditie
            ((equal? (car autos) 'first) ; We kunnen niet botsen met een voorgaande auto. Deze auto rijdt vooraan.
              (let* ((car-nr (get-nr (cadr autos)))
                      (move-distance (calculate-move-distance (car-type car-nr) lane-ref (pass-message road 'lane-function lane-ref)))
                      (new-left (+ (car-left-x car-nr) move-distance))
                      (new-right (+ new-left car-width)))
                (if (>= 0 new-right) ; Als de auto na verplaasting links buiten beeld zou zijn, wordt hij naar rechts verplaatst.
                  (begin (setcar-left-x car-nr (pass-message manager 'get-width))
                    (setcar-right-x car-nr (+ (pass-message manager 'get-width) car-width))
                    (pass-message road 'move-ltor lane-ref)
                    (draw-car car-nr grafisch-ADT)
                    ; Botst de huidige auto met de kikker?
                    (check-for-collision (car-left-x car-nr)(car-right-x car-nr) lane-ref frogger level-manager)
                    ; De volgende auto moet nu gezien worden als de eerste auto op deze laan.
                    (loop (cons 'first (cddr autos))))
                  (if (>= new-left (pass-message manager 'get-width)) ; Als de auto voor verplaatsing rechts buiten beeld zou zijn, wordt hij naar links verplaatst.
                    (begin (setcar-left-x car-nr (- car-width))
                      (setcar-right-x car-nr 0)
                      (pass-message road 'move-rtol lane-ref)
                      (draw-car car-nr grafisch-ADT)
                      ; Botst de huidige auto met de kikker?
                      (check-for-collision (car-left-x car-nr)(car-right-x car-nr) lane-ref frogger level-manager)
                      (draw-car car-nr grafisch-ADT)
                      ; De volgende auto moet nu gezien worden als de eerste auto op deze laan.
                      (loop (cons 'first (cddr autos))))
                    (begin (setcar-left-x car-nr new-left)
                      (setcar-right-x car-nr new-right)
                      (draw-car car-nr grafisch-ADT)
                      ; Botst de huidige auto met de kikker?
                      (check-for-collision (car-left-x car-nr)(car-right-x car-nr) lane-ref frogger level-manager)
                      (loop (cdr autos)))))))
            (else (let* ((car-nr (get-nr (cadr autos)))
                          (move-distance (calculate-move-distance (car-type car-nr) lane-ref (pass-message road 'lane-function lane-ref)))
                          (new-left (+ (car-left-x car-nr) move-distance))
                          (new-right (+ new-left car-width))
                          (left-followed-car (car-left-x (get-nr (car autos)))) ; We moeten ook de gegevens hebben van de voorgaande auto.
                          (right-followed-car (car-right-x (get-nr (car autos)))))
                    ; Controleren of we niet botsen met de voorgaande auto.
                    (if (or (and (<= move-distance 0)(<= left-followed-car new-left right-followed-car))   ; We rijden naar links.
                          (and (>= move-distance 0)(>= right-followed-car new-right left-followed-car))) ; We rijden naar rechts.
                      (if (>= move-distance 0) ; We moeten de richting weten om te bepalen welke coördinaat we eerst moeten aanpassen.
                        (begin (setcar-right-x car-nr left-followed-car)
                          (setcar-left-x car-nr (- left-followed-car car-width)))
                        (begin (setcar-left-x car-nr right-followed-car)
                          (setcar-right-x car-nr (+ right-followed-car car-width))))
                      (begin (setcar-left-x car-nr new-left) ; We botsen niet met de voorgaande auto.
                        (setcar-right-x car-nr new-right)))
                    (draw-car car-nr grafisch-ADT)
                    ; Botst de huidige auto met de kikker?
                    (check-for-collision (car-left-x car-nr)(car-right-x car-nr) lane-ref frogger level-manager))
              (loop (cdr autos))))))
      (for-each (lambda (x)(move-cars-on-lane x))
        (pass-message road 'car-lanes)))

    ; TEKENFUNCTIONALITEIT
    (define (draw-cars grafisch-ADT)
      (pass-message grafisch-ADT 'draw-cars))
    (define (draw-car z grafisch-ADT)
      (pass-message grafisch-ADT 'draw-car z))
    (define (delete-cars grafisch-ADT)
      (pass-message grafisch-ADT 'delete-cars))

    ; DISPATCH
    (define (car-disp message)
      (cond ((eq? message 'draw-cars) draw-cars)
        ((eq? message 'delete-cars) delete-cars)

        ((eq? message 'move-cars) move-cars)

        ((eq? message 'car-type) car-type)
        ((eq? message 'left-x) car-left-x)
        ((eq? message 'right-x) car-right-x)
        ((eq? message 'y) car-y)
        ((eq? message 'car-tile) car-tile)
        ((eq? message 'has-tile?) has-tile?)
        ((eq? message 'number-of-cars) give-number-of-cars)

        ((eq? message 'setcar-type) setcar-type)
        ((eq? message 'setcar-left-x) setcar-left-x)
        ((eq? message 'setcar-right-x) setcar-right-x)
        ((eq? message 'setcar-y) setcar-y)
        ((eq? message 'setcar-tile) setcar-tile)
        ((eq? message 'initiate) initiate-cars)
        ((eq? message 'setcar-speed) setcar-speed)
        ((eq? message 'set-level-manager) set-level-manager)

        (else (error "car-disp" "Unknown procedure" message))))
    car-disp))

; ################# FROGGER ###################
; Jens Van der Plas - 1BA Computerwetenschappen
;                 GRAPH LIBRARY
; ################# 2014/15 ###################

;#| H/B ADT |#
; De afbeeldingen die voor de GUI gebruikt worden hebben een bepaalde hoogte-breedteverhouding.
; Deze worden hier opgeslagen om zo hun weerspiegeling in het spel te kunnen hebben.
; De tiles dienen immers correct geschaald te worden en ook de breedte van de elementen in het spel zal hiervan afhangen.
; Aangezien de afbeeldingen vast zijn, bevat dit ADT dus allemaal vaste waarden.
; In het ADT wordt de waarde van breedte/hoogte bewaard.

(define (make-hb)

  ; We slaan deze eigenschappen op als procedure, omdat zo pass-message gebruikt kan worden.
  (define (frogger-hb) 1.25) ; De breedte is 1.25 keer de hoogte.
  (define (coin-hb) 1) ; Een coin is daadwerkelijk vierkant.
  (define (insect1-hb) 0.8)
  (define (insect2-hb) 0.7)
  (define (insect3-hb) 1)
  (define (insect4-hb) 0.8)
  (define (bush-hb) 1.2)
  (define (auto-hb) 2) ; Alle auto's hebben dezelfde hoogte-breedteverhouding.
  (define (pill-hb) 1.2)

  (define (hb-disp message)
    (cond ((eq? message 'frogger-hb) frogger-hb)
      ((eq? message 'coin-hb) coin-hb)
      ((eq? message 'insect1-hb) insect1-hb)
      ((eq? message 'insect2-hb) insect2-hb)
      ((eq? message 'insect3-hb) insect3-hb)
      ((eq? message 'insect4-hb) insect4-hb)
      ((eq? message 'bush-hb) bush-hb)
      ((eq? message 'car-hb) auto-hb)
      ((eq? message 'pill-hb) pill-hb)
      (else (error "hb-disp" "Unknown procedure" message))))
  hb-disp)

;#| #################################################### SCHERM ADT #################################################### |#

; Dit ADT bevat alle eigenlijke functionaliteiten om te kunnen tekenen op het scherm. Het behoort als enige tot de GUI en staat als enige in verband met de grafische library.
; In het ADT worden eerst het venster en de layers aangemaakt. Daarna worden de tiles voor enkelvoudige elementen (zoals kikker en pil) aangemaakt.
; Nadien worden alle tekenfunctionaliteiten geïmplementeerd. Ook de gebruikersinvoer en spellus worden hier onderhouden.

(define (maak-venster hoogte breedte frogger road manager level-manager coins autos)

  ; INITIALISATIE ####################################################

  ; Maken van een venster + toevoegen van de layers.
  (define gamewindow (make-window breedte hoogte "Jens Van der Plas -- FROGGER"))
  (define snelweg-layer (gamewindow 'make-layer))
  (define object-layer (gamewindow 'make-layer))
  (define car-layer (gamewindow 'make-layer))
  (define tekst-layer (gamewindow 'make-layer))

  ; Maken van de tiles voor de kikker en de tekst.
  (define frogger-tile (make-tile 300 240 "Frogger.png" "Frogger_mask.png"))
  (define blue-frogger-tile (make-tile 300 240 "Frogger-blauw.png" "Frogger_mask.png"))
  (define (scale-frogger-tiles)
    (pass-message frogger-tile 'set-scale! (/ (pass-message road 'lane-height) 240))
    (pass-message blue-frogger-tile 'set-scale! (/ (pass-message road 'lane-height) 240)))
  (define tekst-tile (make-tile hoogte breedte)) ; De teksttile overspant het hele spelvenster.

  ; TEKENEN ##########################################################

  ; KIKKER
  (define (draw-frog)
    ; We zetten de coördinaten van de frogger-tiles op de juiste plaats.
    (pass-message frogger-tile 'set-x! (pass-message frogger 'left-x))
    (pass-message frogger-tile 'set-y!
      (pass-message road 'lane-upper-y (pass-message frogger 'y)))
    (pass-message blue-frogger-tile 'set-x! (pass-message frogger 'left-x))
    (pass-message blue-frogger-tile 'set-y!
      (pass-message road 'lane-upper-y (pass-message frogger 'y))))

  (define (make-frogger-blue)
    (pass-message object-layer 'remove-drawable frogger-tile)
    (pass-message object-layer 'add-drawable blue-frogger-tile))

  (define (make-frogger-green)
    (pass-message object-layer 'remove-drawable blue-frogger-tile)
    (pass-message object-layer 'add-drawable frogger-tile))

  ; MUNTJES
  (define (draw-coins)
    (let ((number-of-coins (pass-message coins 'number-of-coins)))
      (define (draw-coin coin-ref)
        (cond ((and (pass-message coins 'coin-status coin-ref) ; Het muntje is nog niet opgegeten
                 (pass-message coins 'has-tile? coin-ref))  ; en heeft al een tile.
                ; -> We zetten de coördinaten van de tile juist.
                (pass-message (pass-message coins 'coin-tile coin-ref)
                    'set-x! (pass-message coins 'left-x coin-ref))
                (pass-message (pass-message coins 'coin-tile coin-ref)
                    'set-y! (pass-message road 'lane-upper-y
                              (pass-message coins 'y coin-ref))))
          ((pass-message coins 'coin-status coin-ref) ; Het muntje is nog niet opgegeten, maar heeft nog geen tile.
            ; -> We maken een tile voor het muntje en zetten zijn coördinaten op de juiste plaats.
            (let ((coin-tile (make-tile 128 128 "coin.png" "coin-mask.png")))
              (pass-message coin-tile 'set-scale! (/ (pass-message road 'lane-height) 128))
              (pass-message coins 'setcoin-tile coin-ref coin-tile)
              (pass-message coin-tile 'set-x! (pass-message coins 'left-x coin-ref))
              (pass-message coin-tile 'set-y! (pass-message road 'lane-upper-y (pass-message coins 'y coin-ref)))
              (pass-message object-layer 'add-drawable coin-tile)))))

      (define (draw-coins-loop coin-ref)
        (if (<= coin-ref number-of-coins)
          (begin (draw-coin coin-ref)
            (draw-coins-loop (+ coin-ref 1)))))
      (draw-coins-loop 1)))

  (define (delete-coin coin-ref)
    (if (pass-message coins 'coin-tile coin-ref) ; Eerst moeten we controleren of de coin wel getekend is.
      (begin (pass-message object-layer 'remove-drawable (pass-message coins 'coin-tile coin-ref))
        (pass-message coins 'setcoin-tile coin-ref #f))))

  ; INSECTEN
  ; Onderstaande procedure maakt tiles voor insecten en schaalt ze.
  (define (make-tile-for-insect width height picture mask lane-height)
    (let ((tile (make-tile width height picture mask)))
      (pass-message tile 'set-scale! (/ lane-height height))
      tile))

  (define (draw-insects insects)
    (let ((number-of-insects (pass-message insects 'number-of-insects)))
      (define (draw-insect insect-ref)
        (cond ((and
                 (pass-message insects 'insect-status insect-ref) ; Het insect is nog niet opgegeten
                 (pass-message insects 'has-tile? insect-ref))    ; en heeft al een tile.
                ; -> We zetten de coördinaten van de tile juist.
                (pass-message (pass-message insects 'insect-tile insect-ref)
                    'set-x! (pass-message insects 'left-x insect-ref))
                (pass-message (pass-message insects 'insect-tile insect-ref)
                    'set-y! (pass-message road 'lane-upper-y
                              (pass-message insects 'y insect-ref))))
          ((pass-message insects 'insect-status insect-ref) ; Het insect is nog niet opgegeten, maar heeft nog geen tile.
            ; -> We maken een tile voor het insect en zetten zijn coördinaten op de juiste plaats. De afbeelding hangt af van het type insect.
            (let* ((insect-type (pass-message insects 'insect-type insect-ref))
                    (insect-tile (cond ((= 1 insect-type)
                                         (make-tile-for-insect 125 100 "Insect1.jpg" "Insect1-mask.png" (pass-message road 'lane-height)))
                                   ((= 2 insect-type)
                                     (make-tile-for-insect 115 165 "Insect2.png" "Insect2-mask.png" (pass-message road 'lane-height)))
                                   ((= 3 insect-type)
                                     (make-tile-for-insect 180 180 "Insect3.jpg" "Insect3-mask.png" (pass-message road 'lane-height)))
                                   ((= 4 insect-type)
                                     (make-tile-for-insect 85 105 "Insect4.jpg" "Insect4-mask.png" (pass-message road 'lane-height)))
                                   (else (assertion-violation "draw-insect" "Unknown insect type" insect-type insect-ref)))))
              (pass-message insects 'setinsect-tile insect-ref insect-tile)
              (pass-message insect-tile 'set-x! (pass-message insects 'left-x insect-ref))
              (pass-message insect-tile 'set-y! (pass-message road 'lane-upper-y
                                                  (pass-message insects 'y insect-ref)))
              (pass-message object-layer 'add-drawable insect-tile)))))

      (define (draw-insects-loop insect-ref)
        (if (<= insect-ref number-of-insects)
          (begin (draw-insect insect-ref)
            (draw-insects-loop (+ insect-ref 1)))))
      (draw-insects-loop 1)))

  (define (delete-insect insect-ref insects)
    (if (pass-message insects 'insect-tile insect-ref) ; Eerst moeten we controleren of het insect wel getekend is.
      (begin (pass-message object-layer 'remove-drawable (pass-message insects 'insect-tile insect-ref))
        (pass-message insects 'setinsect-tile insect-ref #f))))

  (define (delete-insects insects)
    (let loop ((number-of-insects (pass-message insects 'number-of-insects))
                (insect-ref 1))
      (if (<= insect-ref number-of-insects)
        (begin (delete-insect insect-ref insects)
          (loop number-of-insects (+ insect-ref 1))))))

  ; STRUIKEN
  (define (draw-bushes bushes)
    (let ((number-of-bushes (pass-message bushes 'number-of-bushes)))
      (define (draw-bush bush-ref)
        ; Voor iedere struik maken we een tile die we schalen en op de juiste plaats zetten.
        (let ((tile (make-tile 165 140 "struik.png" "struik-mask.png")))
          (pass-message tile 'set-scale! (/ (pass-message road 'lane-height) 140))
          (pass-message bushes 'setbush-tile bush-ref tile)
          (pass-message tile 'set-x! (pass-message bushes 'left-x bush-ref))
          (pass-message tile 'set-y! (pass-message road 'lane-upper-y (pass-message bushes 'y bush-ref)))
          (pass-message object-layer 'add-drawable tile)))

      (define (draw-loop bush-ref)
        (if (<= bush-ref number-of-bushes)
          (begin (draw-bush bush-ref)
            (draw-loop (+ bush-ref 1)))))
      (draw-loop 1)))

  (define (delete-bushes bushes)
    (let ((number-of-bushes (pass-message bushes 'number-of-bushes)))
      (let loop ((bush-ref 1))
        (cond ((> bush-ref number-of-bushes)) ; Stopconditie
          ((pass-message bushes 'has-tile? bush-ref)
            (pass-message object-layer 'remove-drawable (pass-message bushes 'bush-tile bush-ref))
            (pass-message bushes 'setbush-tile bush-ref #f)
            (loop (+ bush-ref 1)))
          (else (error "delete-bushes" "Missing tile" bush-ref))))))

  ; PILL
  (define pill-tile (make-tile 300 250 "Pill.png" "Pill-mask.png"))
  (define (scale-pill-tile)(pass-message pill-tile 'set-scale! (/ (pass-message road 'lane-height) 250)))
  (define (draw-pill pill)
    (pass-message pill-tile 'set-x! (pass-message pill 'left-x))
    (pass-message pill-tile 'set-y! (pass-message road 'lane-upper-y (pass-message pill 'y)))
    (pass-message object-layer 'add-drawable pill-tile))
  (define (delete-pill)
    (pass-message object-layer 'remove-drawable pill-tile))

  ; AUTO'S
  (define (draw-car car-ref)
    (if (not (pass-message autos 'has-tile? car-ref)) ; Als de auto nog geen tile heeft maken we er een aan.
      (let* ((direction (pass-message road 'lane-function (pass-message autos 'y car-ref))) ; In welke richting moet de auto rijden?
              (type (pass-message autos 'car-type car-ref)) ; We moeten ook het type van de auto weten om te bepalen welke afbeelding we gaan gebruiken.
              (car-x (pass-message autos 'left-x car-ref))                                ; x-coördinaat voor de tile
              (car-y (pass-message road 'lane-upper-y (pass-message autos 'y car-ref)))   ; y-coördinaat voor de tile
              ; De tile hangt af van het type auto en van de rijrichting.
              ; We slaan de hoogte van de afbeelding mee op om te kunnen schalen achteraf.
              (car-tile-and-height (cond ((equal? type 'A)
                                           (if (equal? direction 'naar-links)
                                             (cons 135 (make-tile 265 135 "2-naar-links.png" "2-naar-links-mask.png"))
                                             (cons 135 (make-tile 265 135 "2-naar-rechts.png" "2-naar-rechts-mask.png"))))
                                     ((equal? type 'B)
                                       (if (equal? direction 'naar-links)
                                         (cons 100 (make-tile 205 100 "3-naar-links.png" "3-naar-links-mask.png"))
                                         (cons 100 (make-tile 205 100 "3-naar-rechts.png" "3-naar-rechts-mask.png"))))
                                     ((equal? type 'C)
                                       (if (equal? direction 'naar-links)
                                         (cons 140 (make-tile 280 140 "4-naar-links.png" "4-naar-links-mask.png"))
                                         (cons 140 (make-tile 280 140 "4-naar-rechts.png" "4-naar-rechts-mask.png"))))
                                     (else (if (equal? direction 'naar-links)
                                             (cons 125 (make-tile 245 125 "1-naar-links.png" "1-naar-links-mask.png"))
                                             (cons 125 (make-tile 245 125 "1-naar-rechts.png" "1-naar-rechts-mask.png")))))))
        (pass-message (cdr car-tile-and-height) 'set-scale! (/ (pass-message road 'lane-height) (car car-tile-and-height))) ; We moeten de tile natuurlijk ook schalen
        (pass-message autos 'setcar-tile car-ref (cdr car-tile-and-height))                ; en aan de auto geven, waarna we de coördinaten goed zetten.
        (pass-message (cdr car-tile-and-height) 'set-x! car-x)
        (pass-message (cdr car-tile-and-height) 'set-y! car-y)
        (pass-message car-layer 'add-drawable (cdr car-tile-and-height)))        ; Tot slot wordt de tile toegevoegd aan de carlayer.
      (begin (pass-message (pass-message autos 'car-tile car-ref) 'set-x! (pass-message autos 'left-x car-ref)) ; Als de auto al een tile heeft, moeten we gewoon de coördinaten juist zetten.
        (pass-message (pass-message autos 'car-tile car-ref) 'set-y! (pass-message road 'lane-upper-y (pass-message autos 'y car-ref))))))

  (define (draw-cars)
    (let ((number-of-cars (pass-message autos 'number-of-cars)))
      (define (draw-car-loop z)
        (if (<= z number-of-cars)
          (begin (draw-car z)
            (draw-car-loop (+ z 1)))))
      (draw-car-loop 1)))

  (define (delete-cars)
    (let ((number-of-cars (pass-message autos 'number-of-cars)))
      (let loop ((z 1))
        (if (<= z number-of-cars)
          (begin (pass-message car-layer 'remove-drawable (pass-message autos 'car-tile z))
            (pass-message autos 'setcar-tile z #f)
            (loop (+ z 1)))))))

  ; SNELWEG
  (define (draw-road)
    (let ((car-lanes (pass-message road 'car-lanes)))
      (define (draw-lane lane-ref)
        (cond ((pass-message road 'has-tile? lane-ref) ; De rijstrook heeft al een tile.
                ; -> We zetten de y-coördinaat van de tile juist. De x-coördinaat is steeds 0 en moet dus niet veranderd worden.
                (pass-message (pass-message road 'lane-tile lane-ref)
                    'set-y! (pass-message road 'lane-upper-y lane-ref)))

          (else ; De laan is een rijstrook maar heeft nog geen tile.
            ; -> We maken een tile voor de rijstrook en zetten de coördinaten op de juiste plaats.
            (let ((lane-tile (make-tile 2910 112 "weg.png" "weg-mask.png")))
              (pass-message lane-tile 'set-scale! (/ (pass-message road 'lane-height) 113))
              (pass-message road 'setlane-tile lane-ref lane-tile)
              (pass-message lane-tile 'set-x! 0)
              (pass-message lane-tile 'set-y! (pass-message road 'lane-upper-y lane-ref))
              (pass-message snelweg-layer 'add-drawable lane-tile)))))
      (for-each draw-lane car-lanes)))

  (define (delete-lane lane-ref)
    (if (pass-message road 'lane-tile lane-ref) ; Eerst moeten we controleren of de laan wel een tile heeft.
      (begin (pass-message snelweg-layer 'remove-drawable (pass-message road 'lane-tile lane-ref))
        (pass-message road 'setlane-tile lane-ref #f))))

  (define (delete-road)
    (let ((lanes (pass-message road 'number-of-lanes)))
      (define (delete-road-loop lane-ref)
        (if (<= lane-ref lanes)
          (begin (delete-lane lane-ref)
            (delete-road-loop (+ lane-ref 1)))))
      (delete-road-loop 1)))

  ; TEKST EN GEBRUIKERSBOODSCHAPPEN ##################################

  ;Tekst
  (define (draw-data)
    (tekst-tile 'clear)
    (pass-message tekst-tile 'draw-text (string-append "Score: " (number->string (pass-message manager 'get-score))) 12 5 5 "White")
    (pass-message tekst-tile 'draw-text (string-append "Highscore: " (number->string (pass-message manager 'get-highscore))) 12 100 5 "White")
    (pass-message tekst-tile 'draw-text (string-append "Lives: " (number->string (pass-message frogger 'frog-lives))) 12 5 20 "White")
    (pass-message tekst-tile 'draw-text (string-append "Level: " (number->string (pass-message manager 'get-level))) 12 100 20 "White")
    (pass-message tekst-tile 'draw-text "Move Frogger with the arrow keys. When you've reached the top lane, press 'up'." 10 5 (- hoogte 30) "Dark Slate Gray")
    (pass-message tekst-tile 'draw-text "The home key will restart the game, the end key will end the game and the enter key wil restart the level."   10 5 (- hoogte 15) "Dark Slate Gray"))

  ; Aanmaken van de tiles voor de boodschappen
  (define tile-for-next-level-announcement (make-tile 400 300 "next-level.png" "mask-for-announcement.png"))
  (define tile-for-restart-announcement (make-tile 400 300 "restarting-level.png" "mask-for-announcement.png"))
  (define tile-for-immortal-restart-announcement (make-tile 400 300 "restarting-level-immortal.png" "mask-for-announcement.png"))
  (define tile-for-lost-announcement (make-tile 400 300 "game-over.png" "mask-for-announcement.png"))
  (define tile-for-instructions (make-tile 600 800 "game-instructions.png" "game-instructions-mask.png"))

  ; Aanmaken van een schaalfucntie voor de announcementtiles
  (define (scale-and-position-announcement-tile tile)
    (let* ((scale-factor (/ (/ hoogte 2) 400))
            (new-tile-width (* 400 scale-factor))
            (new-tile-height (* 300 scale-factor)))
      (pass-message tile 'set-scale! scale-factor)
      (pass-message tile 'set-x! (- (/ breedte 2)(/ new-tile-width 2))) ; We centreren de tile op het scherm.
      (pass-message tile 'set-y! (- (/ hoogte 2)(/ new-tile-height 2)))))

  ; De instructions-tile wordt op een aparte manier geschaald.
  (define (scale-and-position-instructions)
    (let* ((scale-factor (min (/ breedte (* 1.2 600))
                           (/ hoogte (* 1.2 800))))
            (new-tile-width (* 600 scale-factor))
            (new-tile-height (* 800 scale-factor)))
      (pass-message tile-for-instructions 'set-scale! scale-factor)
      (pass-message tile-for-instructions 'set-x! (- (/ breedte 2) (/ new-tile-width 2)))
      (pass-message tile-for-instructions 'set-y! (- (/ hoogte 2) (/ new-tile-height 2)))))

  ; Tekenen en verwijderen van de tiles
  (define (draw-announcement announcement)
    (cond ((eq? announcement 'next)(pass-message tekst-layer 'add-drawable tile-for-next-level-announcement))
      ((eq? announcement 'restart)(pass-message tekst-layer 'add-drawable tile-for-restart-announcement))
      ((eq? announcement 'immortal-restart)(pass-message tekst-layer 'add-drawable tile-for-immortal-restart-announcement))
      ((eq? announcement 'lost)(pass-message tekst-layer 'add-drawable tile-for-lost-announcement))
      ((eq? announcement 'instructions) (pass-message tekst-layer 'add-drawable tile-for-instructions))
      (else (error "draw-announcement" "Unknown announcement" announcement))))

  (define (delete-announcements)
    (pass-message tekst-layer 'remove-drawable tile-for-next-level-announcement)
    (pass-message tekst-layer 'remove-drawable tile-for-restart-announcement)
    (pass-message tekst-layer 'remove-drawable tile-for-immortal-restart-announcement)
    (pass-message tekst-layer 'remove-drawable tile-for-lost-announcement)
    (pass-message tekst-layer 'remove-drawable tile-for-instructions))

  ; INPUT EN SPELLUS #################################################

  (define (on-key key)
    (cond ((eq? key 'left)
            (pass-message frogger 'move-frog-left breedte)
            (has-eaten? frogger road graph-disp manager)
            (draw-frog))
      ((eq? key 'right)
        (pass-message frogger 'move-frog-right breedte)
        (has-eaten? frogger road graph-disp manager)
        (draw-frog))
      ((eq? key 'up)
        (pass-message frogger 'move-frog-forward road coins level-manager)
        (has-eaten? frogger road graph-disp manager)
        (draw-frog))
      ((eq? key 'home) ; Herstarten van het spel.
        (pass-message level-manager 'restart-game))
      ((eq? key 'end) ; Het beëindigen van het spel.
        (pass-message level-manager 'end-game))
      ((eq? key #\newline) ; Het herstarten van het level. ; Orignally #\return
        (pass-message level-manager 'restart-level))
      ((eq? key #\space) ; Het verwijderen van schermboodschappen
        (delete-announcements))))

  (define delta-move-time 300) ; Iedere 300 ms worden de auto's verplaatst.
  (define current-move-time 0)
  (define (move-cars dt)
    (if (>= (+ dt current-move-time) delta-move-time)
      (begin (pass-message autos 'move-cars delta-move-time graph-disp)
        (set! current-move-time 0))
      (set! current-move-time (+ current-move-time dt))))

  (define (gameloop dt)
    (pass-message frogger 'decrease-immortality-time dt graph-disp)
    (move-cars dt))

  (define (start-game)
    (pass-message gamewindow 'set-update-callback! gameloop)
    (pass-message gamewindow 'set-key-callback! on-key)
    (gamewindow 'start))

  ; DISPATCH #########################################################

  (define (graph-disp message)
    (cond ((eq? message 'draw-frog) draw-frog)
      ((eq? message 'scale-frogger) scale-frogger-tiles)
      ((eq? message 'make-frogger-blue) make-frogger-blue)
      ((eq? message 'make-frogger-green) make-frogger-green)

      ((eq? message 'draw-coins) draw-coins)
      ((eq? message 'delete-coin) delete-coin)

      ((eq? message 'draw-insects) draw-insects)
      ((eq? message 'delete-insect) delete-insect)
      ((eq? message 'delete-insects) delete-insects)

      ((eq? message 'draw-bushes) draw-bushes)
      ((eq? message 'delete-bushes) delete-bushes)

      ((eq? message 'draw-cars) draw-cars)
      ((eq? message 'draw-car) draw-car)
      ((eq? message 'delete-cars) delete-cars)

      ((eq? message 'draw-road) draw-road)
      ((eq? message 'delete-road) delete-road)

      ((eq? message 'draw-pill) draw-pill)
      ((eq? message 'delete-pill) delete-pill)
      ((eq? message 'scale-pill) scale-pill-tile)

      ((eq? message 'draw-data) draw-data)
      ((eq? message 'start) (start-game))

      ((eq? message 'draw-announcement) draw-announcement)
      ((eq? message 'delete-announcements) delete-announcements)
      (else (error "graph-disp" " Unidentified procedure" message))))

  ; VERVOLG INITIALISATIE ############################################

  (pass-message gamewindow 'set-background! "Lawngreen")
  (scale-and-position-announcement-tile tile-for-next-level-announcement)
  (scale-and-position-announcement-tile tile-for-restart-announcement)
  (scale-and-position-announcement-tile tile-for-immortal-restart-announcement)
  (scale-and-position-announcement-tile tile-for-lost-announcement)
  (scale-and-position-instructions)
  (pass-message object-layer 'add-drawable frogger-tile)
  (pass-message tekst-layer 'add-drawable tekst-tile)
 ; (start-game) ; We starten de spellus op.
  graph-disp)

; ################# FROGGER ###################
; Jens Van der Plas - 1BA Computerwetenschappen
;               MANAGERS LIBRARY
; ################# 2014/15 ###################

;#| LEVEL MANAGER ADT |#

(define (make-level-manager manager frogger road coins insects cars bushes pill h/b)
  (let ((breedte (pass-message manager 'get-width))
         (hoogte (pass-message manager 'get-height))
         (random (Rand))
         (grafisch-ADT 'null))

    (define (set-grafisch-ADT! r)
      (set! grafisch-ADT r))

    (define (init-frogger)
      ; De kikker wordt terug op zijn beginpositie gezet.
      (let* ((h (pass-message road 'lane-height))
              (w (* h (pass-message h/b 'frogger-hb)))) ; We berekenen de breedte van de kikker.
        (pass-message frogger 'setfrog-y (pass-message road 'number-of-lanes))
        (pass-message frogger 'setfrog-left-x (- (/ breedte 2)(/ w 2)))
        (pass-message frogger 'setfrog-right-x (+ (pass-message frogger 'left-x)  w))))

    (define (draw-game-elements)
      (pass-message road 'draw-road grafisch-ADT)
      (pass-message frogger 'draw-frog grafisch-ADT)
      (pass-message coins 'draw-coins grafisch-ADT)
      (pass-message insects 'draw-insects grafisch-ADT)
      (pass-message bushes 'draw-bushes grafisch-ADT)
      (pass-message pill 'draw-pill grafisch-ADT)
      (pass-message cars 'draw-cars grafisch-ADT)
      (pass-message grafisch-ADT 'draw-data))

    (define (delete-game-elements)
      ; Het verwijderen van de weg, struiken, auto's en de insecten van het scherm.
      ; Deze procedure verwijdert de muntjes niet!
      (pass-message road 'delete-road grafisch-ADT)
      (pass-message pill 'delete-pill grafisch-ADT)
      (pass-message bushes 'delete-bushes grafisch-ADT)
      (pass-message cars 'delete-cars grafisch-ADT)
      (pass-message insects 'delete-insects grafisch-ADT))

    (define (initiate-level)
      (pass-message road 'new-number-of-lanes (+ 6 (pass-message random 'give-random-integer 4)) grafisch-ADT) ; We krijgen 6 à 10 lanen.
      (init-frogger)
      (pass-message coins 'initiate road)
      (pass-message insects 'initiate road)
      (pass-message bushes 'initiate)
      (pass-message pill 'initiate)
      (pass-message cars 'initiate)
      (pass-message grafisch-ADT 'scale-frogger) ; We moeten de froggertile en de pilltile opnieuw schalen. We hebben immers een nieuw aantal lanen.
      (pass-message grafisch-ADT 'scale-pill)
      (draw-game-elements)
      (pass-message cars 'setcar-speed (* 0.01 (pass-message manager 'get-level)))) ; De snelheid van de auto's is afhankelijk van het level.

    (define (restart-level)
      (let ((y (pass-message frogger 'y)))
        (delete-announcements) ; We verwijderen alle boodschappen van het scherm.
        (if (pass-message frogger 'can-die?) ; Als de kikker niet onsterfelijk is, verliest hij een leven.
          (begin (pass-message frogger 'setfrog-lives (- (pass-message frogger 'frog-lives) 1) grafisch-ADT)
            (announcement 'restart))
          (announcement 'immortal-restart))
        (if (not (<= (pass-message frogger 'frog-lives) 0)) ; Heeft Frogger nog levens over?
          (begin (init-frogger)
            (pass-message frogger 'draw-frog grafisch-ADT))
          (end-game))))

    (define (end-game) ; Het spel wordt beëindigd.
      (delete-announcements)
      (pass-message frogger 'setfrog-y 1) ; We zetten frogger op de eindberm.
      (pass-message frogger 'draw-frog grafisch-ADT)
      (pass-message manager 'sethighscore (pass-message manager 'get-score))
      (pass-message manager 'setscore-reset)
      (announcement 'lost))

    (define (start-game)
      (announcement 'instructions)
      (initiate-level))

    (define (restart-game)
      (pass-message manager 'setscore-reset)
      (pass-message coins 'delete-coins grafisch-ADT) ; We maken het speelveld leeg.
      (delete-game-elements)
      (delete-announcements) ; We verwijderen alle boodschappen van het scherm.
      (pass-message frogger 'setfrog-lives 3 grafisch-ADT)
      (pass-message manager 'setscore 0)
      (pass-message manager 'setlevel-reset)
      (start-game)) ; En we starten het spel terug opnieuw.

    (define (next-level)
      (delete-announcements) ; We verwijderen alle boodschappen van het scherm.
      (announcement 'next)
      (pass-message manager 'setlevel-up) ; We verhogen het level.
      (pass-message manager 'sethighscore (pass-message manager 'get-score)) ; De highscore wordt na ieder level aangepast.
      ; Nu verwijderen we alle bestaande elementen van het speelveld. De muntjes zijn allemaal opgegeten en hoeven dus niet meer verwijderd te worden.
      (delete-game-elements)
      (initiate-level))

    ; TEKENEN VAN GEBRUIKERSBOODSCHAPPEN
    (define (announcement announcement)
      (pass-message grafisch-ADT 'draw-announcement announcement))
    (define (delete-announcements)
      (pass-message grafisch-ADT 'delete-announcements))

    (define (level-disp message)
      (cond ((eq? message 'initiate-level) initiate-level)
        ((eq? message 'restart-level) restart-level)
        ((eq? message 'next-level) next-level)
        ((eq? message 'end-game) end-game)
        ((eq? message 'restart-game) restart-game)

        ((eq? message 'set-grafisch-ADT) set-grafisch-ADT!)
        ((eq? message 'draw-game-elements) draw-game-elements)

        ((eq? message 'announcement) announcement)
        ((eq? message 'delete-announcement) delete-announcements)

        ((eq? message 'start-game) start-game)

        (else (error "level-disp" "Unidentified procedure" message))))
    level-disp))

;#| MANAGER ADT |#

(define (make-manager height width)
  (let ((level 1)             ; Beginlevel
         (score 0)             ; Beginscore
         (highscore 0)         ; Eerste highscore
         (hoogte height)       ; Hoogte van het scherm
         (breedte width)       ; Breedte van het scherm
         (grafisch-ADT 'null)) ; Wordt later ingevuld.

    (define (set-grafisch-ADT ADT)
      (set! grafisch-ADT ADT))

    ; ACCESSOREN
    (define (get-level) level)
    (define (get-score) score)
    (define (get-highscore) highscore)
    (define (get-height) hoogte)
    (define (get-width) breedte)

    ; MUTATOREN
    (define (setlevel-up)
      (set! level (+ level 1))
      (pass-message grafisch-ADT 'draw-data))
    (define (setlevel-reset) (set! level 1))
    (define (setscore punten)
      (set! score (+ score punten))
      (pass-message grafisch-ADT 'draw-data))
    (define (setscore-reset) (set! score 0))
    (define (sethighscore punten)
      (if (> punten highscore)
        (set! highscore punten))
      (pass-message grafisch-ADT 'draw-data))

    ; DISPATCH
    (define (manager-disp message)
      (cond ((eq? message 'get-level) get-level)
        ((eq? message 'get-score) get-score)
        ((eq? message 'get-highscore) get-highscore)
        ((eq? message 'get-height) get-height)
        ((eq? message 'get-width) get-width)

        ((eq? message 'setlevel-up) setlevel-up)
        ((eq? message 'setlevel-reset) setlevel-reset)
        ((eq? message 'setscore) setscore)
        ((eq? message 'setscore-reset) setscore-reset)
        ((eq? message 'sethighscore) sethighscore)
        ((eq? message 'set-grafisch-ADT) set-grafisch-ADT)

        (else (error "manager-disp" "Unidentified procedure" message))))
    manager-disp))

; ################# FROGGER ###################
; Jens Van der Plas - 1BA Computerwetenschappen
;                  MAIN FILE
; ################# 2014/15 ###################

; INITIALISATIE

; Eerst maken we een manager. Aan de manager geven de de hoogte en breedte van het scherm mee. Hieruit wordt de rest bepaald.
(define Management (make-manager 800 800))
; We voorzien ook onze afbeeldingsgegevens.
(define Width-height (make-hb))
; We maken ook een random-object aan.
(define Random-generator (Rand))
; Dan maken we onze snelweg. We stellen het aantal lanen in op 9. Dit kan makkelijk aangepast worden door het argument van make-road te veranderen.
(define Road (make-road (pass-message Management 'get-height) Random-generator))
; Nu maken we onze kikker. We zetten de kikker in het midden op de beginberm.
(define Frogger (make-frogger 3
                  Road
                  7500))
; Maken van de auto's en de struiken.
(define Cars (make-cars Road Management Frogger Width-height Random-generator))
(define Bushes (make-bushes Management Road Width-height))
; Toevoegen van de muntjes en de insecten. Er worden 5 muntjes gemaakt met waarde 20.
(define Coins (make-coins 5 Road Management Width-height))
(define Insects (make-insects 5 Road Management Width-height Random-generator))

; Nu de pil.
(define Pill (make-pill Management Road Width-height))

; Levelmanagement initialiseren
(define Level-management (make-level-manager Management Frogger Road Coins Insects Cars Bushes Pill Width-height))

; Maken van het venster.
(define Window (maak-venster (pass-message Management 'get-height)
                 (pass-message Management 'get-width)
                 Frogger
                 Road
                 Management
                 Level-management
                 Coins
                 Cars))

; Initialisatie
(pass-message Level-management 'set-grafisch-ADT Window)
(pass-message Management 'set-grafisch-ADT Window)
(pass-message Cars 'set-level-manager Level-management)
(pass-message Level-management 'start-game)

; Voor het uitvoeren van het spel: klik "Run".
(Window 'start)