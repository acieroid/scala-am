(define down-key 0)
(define left-key 1)
(define right-key 2)

(define pi 3.1415)

(define MAX_LOOP_ITERATIONS 10)

(define global-min #f)
(define global-max #f)
(define global-draw-adt #f)
(define global-current 0)

(define (remq x l)
  (if (pair? l)
      (if (eq? x (car l))
          (remq x (cdr l))
          (cons x (remq x (cdr l))))
      l))

(define (enter-keyboard-input)
  (random 3))
  ;0)

;;;;---------------------------------------------------------------------
;;;; Note: this code needs to be cleaned up since we mainly did all the dirty
;;;;       graphics work to achieve a decent drawing efficiency and to 
;;;;       make sure students don't waste time on this. 
;;;;       this is NOT a reference on how to code cleanly.
;;;;---------------------------------------------------------------------

;;;;################################ WINDOW #######################################
;;;;---------------------------------------------------------------------
;;;; make-window creates a window that accepts tiles and tile-sequences.
;;;; changing the x-value of a tile will update the canvas.
;;;;---------------------------------------------------------------------
(define (make-window w h title)
  ;; #############################################################
  ;; ###### Initalization of our intelligent game-window #########
  ;; #############################################################  
  (let* ((show-fps #t)
         (fps 0)
         (delta-time 0)
         (previous-time 0)
         ;; Define our dummy keyboard-callback
         (keyboard-callback (lambda (ev) 0))
         ;; Define our dummy update-callback
         (update-callback (lambda () 0))
         (layers '())
         (closed #f))
    
    ;; Define the paint-callback which is called each frame
    (define (paint-callback)
      ;; before we do anything, the game-loop is executed.
      (update-callback)
      ;; calculate frames per second. 
      (set! fps (calculate-fps delta-time)))
    
    ;; Calculate FPS from the time (ms) since last frame
    (define (calculate-fps delta-time)
      (if (not (= delta-time 0))
          (/ 1000 delta-time) 
          fps))
    
    ;; #############################################################    
    ;; ###### public methods for the window ADT ####################
    ;; #############################################################
    ;;Create and add layers to the window
    (define (add-layer)
      (define layer (make-layer w h))
      (set! layers (append layers (cons layer '())))
      layer)
    
    ;; Set the backgroudn color of the window
    (define (set-background! colorstring)
      0)
        
    ;; The heart of the self-sustaning loop.
    (define (game-loop)
      (keyboard-callback (enter-keyboard-input))
      ;; We wait for min-delta-time, which is typically the min-wait-per-frame
      (paint-callback))
    
    ;; dispatch
    (define (window-dispatch msg)
      (cond ((eq? msg 'make-layer) (add-layer))
            ((eq? msg 'set-background!) (lambda (mw-sb) (set-background! mw-sb)))
            ((eq? msg 'set-key-callback!) (lambda (eh) (set! keyboard-callback eh)))
            ((eq? msg 'set-update-callback!) (lambda (gl) (set! update-callback gl)))
            ((eq? msg 'game-loop) (lambda () (game-loop)))
            (else (#f msg))))
    
    window-dispatch))

;;;;################################ GET SPRITES FROM DISK #######################################
;;;;---------------------------------------------------------------------
;;;; make-bitmap creates a bitmap given a path to an image file
;;;; String -> get-bitmap
;;;;---------------------------------------------------------------------
(define (get-bitmap file)
  0)

;;;;---------------------------------------------------------------------
;;;; make-bitmap creates a bitmap given a path to an image file
;;;; String -> get-bitmap-section
;;;;---------------------------------------------------------------------
(define (get-bitmap-section tilebitmap x y width height)
  0)

;;;;---------------------------------------------------------------------
;;;; generate-mask generates a mask and saves it to disk.
;;;;---------------------------------------------------------------------
(define (generate-mask bitmappath background-color)
  0)

;;;;################################ TILES #######################################
;;;;---------------------------------------------------------------------
;;;; make-bitmap-tile creates a tile from a bitmap with optionally a mask.
;;;; [] mean it is optional.
;;;; String [String] -> Tile
;;;;---------------------------------------------------------------------
(define (make-bitmap-tile bitmappath mask)
  (let ((x 0)
        (y 0))
    (define (bitmap-tile-dispatch msg)
      (cond ((eq? msg 'set-x!) (lambda (new-x) (set! x new-x)))
            ((eq? msg 'set-y!) (lambda (new-y) (set! y new-y)))
            ((eq? msg 'get-x) (lambda () x))
            ((eq? msg 'get-y) (lambda () y))
            (else (#f msg))))
    bitmap-tile-dispatch))

;;;;---------------------------------------------------------------------
;;;; make-tile creates a tile from a width and height with optionally
;;;; a bitmap and a mask. 
;;;; Integer Integer [String] [String] -> Tile
;;;;---------------------------------------------------------------------     
(define (make-tile w h bitmap mask)
  (if (string? bitmap) (set! bitmap (get-bitmap bitmap)))
  (if (string? mask) (set! mask (get-bitmap mask)))
  (let* ((x 0) 
         (y 0) 
         (update-callback  (lambda () #t))
         (rotation 0))
    
    ;; ##### Drawing methods to draw on the tile yourself.
    ;; Clear removed your own drawings.
    (define (clear)
      (update-callback))
    
    ;; Drawing a rectangle
    (define (draw-rectangle x y w h color )
      (if (string? color) (set! color ""))
      (update-callback))
    
    ;; Drawing an Ellipse
    (define (draw-ellipse x y w h color)
      (if (string? color) "")
      (update-callback))
    
    ;; Drawing a Line
    (define (draw-line x y w h color)
      (if (string? color) "")
      (update-callback))
    
    ;; Drawing Text
    (define (draw-text text fontsize x y color)
      (if (string? color) (set! color ""))
      (update-callback))
    
    ;; Rotation of 90 degrees clockwise.
    (define (rotate-clockwise)
      (set! rotation (modulo (+ rotation 90) 360))
      (rotate rotation))
    
    ;; Rotation of 90 degrees counterclockwise.
    (define (rotate-counterclockwise)
      (set! rotation (modulo (- rotation 90) 360))
      (if (< rotation 0)
          (set! rotation 270))
      (rotate rotation))
    
    ;; Internal Rotation Function with a hack to solve
    ;; the rather bizar way of rotating in the graphical DrRacket library.
    (define (rotate tempr)
      (set! rotation tempr)
      (if (not (or (eq? 90 tempr) (eq? 0 tempr) (eq? 180 tempr)  (eq? 270 tempr)))
        (begin (display "ERROR ::: illegal rotation given, only 0,90,180,270 is allowed: ")
               (newline)))
      ; (define r (/ (* tempr pi) 180))
      (update-callback))
    
    ;; Set the X position on the screen
    (define (set-x! new_x)
      (set! x new_x)
      (update-callback))
    
    ;; Set the Y position on the screen
    (define (set-y! new_y)
      (set! y new_y)
      (update-callback))
    
    ;; Drawing procedure called by the layer 
    ;; on which the tile is drawn. Not to be used
    ;; by students manually!
    (define (draw dc)
      0)
    
    ;; A procedure to set a callback. This callback
    ;; will notify the parent (layers) that the tile
    ;; has changed and allows us to automatically
    ;; redraw the tiles.
    (define (set-on-update! new_callback)
      (set! update-callback new_callback))
    
    ;; Dispatch
    (define (tile-dispatch msg)
      (cond 
        ;; Not to be called manually
        ((eq? msg 'draw) (lambda (x-graphics-1) (draw x-graphics-1)))
        ((eq? msg 'set-on-update!) (lambda (x-graphics-2) (set-on-update! x-graphics-2)))
        
        ;; Getters and setters
        ((eq? msg 'set-x!) (lambda (x-graphics-3) (set-x! x-graphics-3)))
        ((eq? msg 'set-y!) (lambda (x-graphics-4) (set-y! x-graphics-4)))
        ((eq? msg 'get-x) (lambda () x))
        ((eq? msg 'get-y) (lambda () y))
        ((eq? msg 'get-w) (lambda () w))
        ((eq? msg 'get-h) (lambda () h))
        ;; Drawing Functions
        ((eq? msg 'rotate-clockwise) (lambda () (rotate-clockwise)))
        ((eq? msg 'rotate-counterclockwise) (lambda () (rotate-counterclockwise)))
        ((eq? msg 'clear) (lambda () (clear)))
        ((eq? msg 'draw-rectangle) (lambda (x-5 x-6 x-7 x-8 x-9) (draw-rectangle x-5 x-6 x-7 x-8 x-9)))
        ((eq? msg 'draw-line) (lambda (x-10 x-11 x-12 x-13 x-14) (draw-line x-10 x-11 x-12 x-13 x-14)))
        ((eq? msg 'draw-ellipse) (lambda (x-15 x-16 x-17 x-18 x-19) (draw-ellipse x-15 x-16 x-17 x-18 x-19)))
        ((eq? msg 'draw-text) (lambda (x-20 x-21 x-22 x-23 x-24) (draw-text x-20 x-21 x-22 x-23 x-24)))
        (else (#f msg))))
    tile-dispatch))

;;;;---------------------------------------------------------------------
;;;; tile-sequence is a sequence of tiles, it is created by passing a list
;;;; of tiles to the tile-sequence. A tile-sequence is meant to animate tiles.
;;;; When it is created, the current tile (index) is set on the first tile that
;;;; was added. Calling next will cycle through the tile-sequence and select the 
;;;; next tile. 
;;;; List<Tile> -> Tile-Sequence
;;;;---------------------------------------------------------------------  
(define (make-tile-sequence tiles)
  ;; Initialize the current index and its callback.
  (let ((index 0) 
        (update-callback (lambda () #t)))
    
    ;; Change its coordiantes on the window
    (define (set-x! new_x)
      (for-each (lambda (tile) ((tile 'set-x!) new_x)) tiles)
      (update-callback))
    (define (set-y! new_y)
      (for-each (lambda (tile) ((tile 'set-y!) new_y)) tiles)
      (update-callback))
    
    ;; choose which tile in the sequence is currently active
    ;; by providing an index.
    (define (set-current! new_index)
      (if (or (>= new_index (length tiles))
              (< new_index 0))
          (begin (display "ERROR ::: illegal index given for tile-sequence: ")
                 (display new_index)
                 (newline))
          (begin (set! index new_index)
                 (update-callback))))
    ;; Set the previous tile as active tile. 
    (define (set-previous!)
      (set! index (modulo  (- index 1) (length tiles)))
      (if (< index 0) (set! index (- (length tiles) 1)))
      (update-callback))
    
    ;; Set the next tile as active tile.
    (define (set-next!)
      (set! index (modulo (+ 1 index) (length tiles)))
      (update-callback))
    
    ;; Drawing functions, each of them will forward the 
    ;; drawing instruction to the underlying tiles.
    (define (rotate-clockwise)
      (for-each (lambda (tile) (tile 'rotate-clockwise) ) tiles)
      (update-callback))
    
    (define (rotate-counterclockwise)
      (for-each (lambda (tile) (tile 'rotate-counterclockwise) ) tiles)
      (update-callback))
    
    (define (draw-rectangle x y w h color)
      (for-each (lambda (tile) ((tile 'draw-rectangle) x y w h color )) tiles)
      (update-callback))
    
    (define (draw-ellipse x y w h color)
      (for-each (lambda (tile) ((tile 'draw-ellipse) x y w h color )) tiles)
      (update-callback))
    
    (define (draw-text text fontsize x y color)
      (for-each (lambda (tile) ((tile 'draw-text) text fontsize x y color )) tiles)
      (update-callback))
    
    (define (draw-line x y w h width color )
      (for-each (lambda (tile) ((tile 'draw-line)x y w h width color  )) tiles)
      (update-callback))
    
    ;; Clears everything that is drawn by the user, 
    ;; if there were bitmaps, the bitmaps are restored.
    (define (clear)
      (for-each (lambda (tile) (tile 'clear)) tiles)
      (update-callback))
    
    
    ;; redraw itself on the provided drawing context
    (define (draw dc)     
      (((current) 'draw) dc))
    
    ;; set update callback which is called every-time a sequence changes
    (define (set-on-update! new_callback)
      (set! update-callback new_callback))
    
    ;; Interal function to retrieve current (private).
    (define (current)
      (list-ref tiles index))
    
    ;; Dispatch
    (define (tile-sequence-dispatch msg )    
      (cond 
        ;; Not to be called manually
        ((eq? msg 'draw)  draw)
        ((eq? msg 'set-on-update!) set-on-update!)
        
        ;; Moving and dimension and position getters.
        ((eq? msg 'set-x!) set-x!)
        ((eq? msg 'set-y!) set-y!)
        ((eq? msg 'get-x)  ((current) 'get-x))
        ((eq? msg 'get-y)  ((current) 'get-y))
        ((eq? msg 'get-w)  ((current) 'get-w))
        ((eq? msg 'get-h)  ((current) 'get-h))
        
        ;; Animations to switch between tiles
        ((eq? msg 'set-current!) set-current!)
        ((eq? msg 'set-next!) (set-next!))
        ((eq? msg 'set-previous!) (set-previous!))    
        
        ;; Rotation manipulations
        ((eq? msg 'rotate-clockwise) (rotate-clockwise)) 
        ((eq? msg 'rotate-counterclockwise) (rotate-counterclockwise))
        
        ;; Clear all manual drawings
        ((eq? msg 'clear) (clear))
        
        ;; Create manual drawings
        ((eq? msg 'draw-rectangle) draw-rectangle)
        ((eq? msg 'draw-ellipse) draw-ellipse)
        ((eq? msg 'draw-line) draw-line)
        ((eq? msg 'draw-text) draw-text) 
        (else (#f msg))))
    tile-sequence-dispatch))

;;;;################################ LAYER #######################################
;;;;---------------------------------------------------------------------
;;;; layers in a window, each layer has a temporary bitmap. 
;;;; Integer Integer -> Layer
;;;;---------------------------------------------------------------------  
(define (make-layer w h)
  
  (let* ((drawables '())                             ;; all drawables on this layer.
         (needs-update #t))                           ;; even faster drawing thanks to dirty bit.
    
    ;; # redraw on temporary bitmap layer.
    (define (redraw)
      0)
    
    ;; # draw itself on given drawing context.
    (define (draw dc)
      (if needs-update 
          (begin (redraw) 
                 (set! needs-update #f))
          #f))
    
    ;; # methods
    ;; Adds a drawable to the layer which is a tile a tile-sequence or 
    ;; a drawable created by the student which suports 'draw' and 'set-on-update!'
    (define (add-drawable drawable)
      ;((drawable 'set-on-update!) (lambda () (set! needs-update #t)))
      (set! drawables (cons drawable drawables))
      (redraw))
    
    ;; Remove a drawable to the layer which is a tile a tile-sequence or 
    ;; a drawable created by the student which suports 'draw' and 'set-on-update!'
    (define (remove-drawable drawable)
      ((drawable 'set-on-update!) (lambda () #t))
      (set! drawables (remq drawable drawables))
      (redraw))
    
    ;; # dispatch
    (define (layer-dispatch msg)
      (cond ((eq? msg 'add-drawable)  add-drawable)
            ((eq? msg 'remove-drawable) remove-drawable)
            ((eq? msg 'draw) draw)
            (else (#f msg))))
    layer-dispatch))




;;; Student's code

(define (add1 add1-x)
  (+ add1-x 1))
(define (sub1 sub1-x)
  (- sub1-x 1))

(define (fill-vector! lambda-fun vector)
  (define (fill-vector-iter! i vector)
    (cond ((= i (vector-length vector)) vector)
          (else (vector-set! vector i (lambda-fun i))
                (fill-vector-iter! (+ i 1) vector))))
  (fill-vector-iter! 0 vector))

(define (make-matrix x y l)
  (let ((matrix-data (make-vector x (make-vector y #f))))
    (define (set-matrix! x y value)
      (vector-set! (vector-ref matrix-data x) y value))
    (define (get-matrix x y)
      (vector-ref (vector-ref matrix-data x) y))
    (define (matrix-dispatch msg)
      (cond ((eq? msg 'set!) set-matrix!)
            ((eq? msg 'get) get-matrix)
            ((eq? msg 'show) (display matrix-data))))
    (fill-vector! (lambda (i) (make-vector y l)) matrix-data)
    matrix-dispatch))

(define (arrow min max draw-adt)
  (set! global-min min)
  (set! global-max max)
  (set! global-draw-adt draw-adt)
  (set! global-current min))

(define (arrow-draw arrow-draw-arrow)
  ((global-draw-adt 'draw-pointer) #f))
(define (arrow-min arrow-min-arrow)
  global-min)
(define (arrow-max arrow-max-arrow)
  global-max)
(define (arrow-current arrow-current-arrow)
  global-current)
(define (arrow-left arrow-left-arrow)
  (if (> (arrow-current arrow-left-arrow) (arrow-min arrow-left-arrow))
      (set! global-current (sub1 (arrow-current arrow-left-arrow)))))
(define (arrow-right arrow-right-arrow)
  (if (< (arrow-current arrow-right-arrow) (arrow-max arrow-right-arrow))
      (set! global-current (add1 (arrow-current arrow-right-arrow)))
      (display "current coordinate above max")))

;(define (arrow min max draw-adt)
;  (let ((current min))
;    (define (draw)
;      ((draw-adt 'draw-pointer) dispatch))
;    (define (dispatch msg)
;      (cond ((eq? msg 'current) current)
;            ((eq? msg 'left)
;             (if (> current min) 
;                 (set! current (sub1 current)))
;             (draw))
;            ((eq? msg 'right) 
;             (if (< current max) 
;                 (set! current (add1 current))
;                 (error "current coordinate above max"))
;             (draw))))
;    dispatch))

(define (make-coin x y bitmap draw-adt)
  (define (draw)
    ((draw-adt 'draw-coin) coin-dispatch))
  (define (coin-dispatch msg)
      (cond ((eq? msg 'bitmap) bitmap)
            ((eq? msg 'draw) (draw))
            ((eq? msg 'get-x) x)
            ((eq? msg 'get-y) y)))
  coin-dispatch)

(define (make-graphics window width height rows cols)
  (let ((background-layer (window 'make-layer))
        (layer (window 'make-layer))
        (coin-tile-list '())
        (pointer-tile (make-bitmap-tile "arrow.png" #f)))
    
    (define (c2p-w x) (* x (/ width cols)))
    
    (define (draw-pointer pointer)
      ((pointer-tile 'set-x!) (c2p-w (arrow-current pointer))))
    
    (define (update-coin! coin tile)
      ((tile 'set-x!) (c2p-w (coin 'get-x)))
      ((tile 'set-y!) (c2p-w (add1 (coin 'get-y)))))
    
    (define (new-coin-tile! coin)
      (let ((coin-tile (make-bitmap-tile (coin 'bitmap) #f)))
        (set! coin-tile-list (cons (list coin coin-tile) coin-tile-list))
        (update-coin! coin coin-tile)
        ((layer 'add-drawable) coin-tile)))
    
    (define (draw-coin coin)
      (let ((asc (assq coin coin-tile-list)))
        (if asc (update-coin! coin (cadr asc))
            (new-coin-tile! coin))))
      
    (define (graphics-dispatch msg)
      (cond ((eq? msg 'draw-pointer) draw-pointer)
            ((eq? msg 'draw-coin) draw-coin)))
    
    (define (make-background-row x y)
      (if (< y (add1 rows))
        (let ((empty-tile (make-bitmap-tile "empty.png" #f)))
          ((background-layer 'add-drawable) empty-tile)
          ((empty-tile 'set-x!) (c2p-w x))
          ((empty-tile 'set-y!) (c2p-w y))
          (make-background-row x (add1 y)))))
    
    (define (make-background x y)
      (if (< x cols) 
        (begin (make-background-row x 1)
               (make-background (add1 x) y))))
    
    ((layer 'add-drawable) pointer-tile)
    (make-background 0 0)
    
    graphics-dispatch))

(define (four-in-a-row width height rows cols title)
  (let* ((window (make-window width height title))
         (game-board (make-matrix rows cols 0))
         (current-coin "blue-tile.png")
         (draw (make-graphics window width height rows cols))
         (pointer (arrow 0 (sub1 cols) draw)))
    
    (define (switch)
      (if (eq? current-coin "blue-tile.png")
          (set! current-coin "red-tile.png")
          (set! current-coin "blue-tile.png")))
   
    (define (find-first col)
      (define (find-first-iter col r)
        (let ((tile ((game-board 'get) r col)))
          (cond 
            ((not (eq? tile 0)) (sub1 r))
            ((= r (sub1 rows)) r)
            (else (find-first-iter col (add1 r))))))
      (find-first-iter col 0))
    
    (define (drop-coin)
      (if (and (not (eq? ((game-board 'get) 0 3) 0)) (not (eq? ((game-board 'get) 1 3) 0)))
          (error "arbitrary error"))
      (let ((row (find-first (arrow-current pointer))))
        (if (>= row 0)
            (let ((new-coin (make-coin (arrow-current pointer) row current-coin draw)))
              (switch)
              (new-coin 'draw)
              ((game-board 'set!) row (arrow-current pointer)  new-coin)))))
      
    (define (on-key rt)
      (cond ((= rt down-key) (drop-coin))
            ((= rt left-key) (arrow-left pointer))
            ((= rt right-key) (arrow-right pointer))
            (else (#f rt))))
    
    (define (start)
      (define (main-loop main-loop-n)
        (if (> main-loop-n 0)
            (begin ((window 'game-loop))
                   (main-loop (- main-loop-n 1)))))
      ((window 'set-key-callback!) on-key)
      ((window 'set-background!) "white")
      (main-loop MAX_LOOP_ITERATIONS))
    
    (define (dispatch msg)
      (cond ((eq? msg 'start!) (start))))
    
    dispatch))

(define game (four-in-a-row 700 700 6 7 "four in a row"))
(game 'start!)
