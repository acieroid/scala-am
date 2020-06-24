;; define planetary masses, initial positions & velocity

(define *pi* 3.141592653589793) ;; define locally to enable inlining
(define *days-per-year* 365.24)

(define *solar-mass* (* 4 *pi* *pi*))

(define *dt* 0.01)

(define (make-body x y z vx vy vz mass) (list (t/ref x) (t/ref y) (t/ref z)
                                              (t/ref vx) (t/ref vy) (t/ref vz)
                                              (t/ref mass)))
(define (body-x b) (t/deref (car b)))
(define (set-body-x! b v) (t/ref-set (car b) v))
(define (body-y b) (t/deref (cadr b)))
(define (set-body-y! b v) (t/ref-set (cadr b) v))
(define (body-z b) (t/deref (caddr b)))
(define (set-body-z! b v) (t/ref-set (caddr b) v))
(define (body-vx b) (t/deref (cadr (cddr b))))
(define (set-body-vx! b v) (t/ref-set (cadddr b) v))
(define (body-vy b) (t/deref (cadr (cdddr b))))
(define (set-body-vy! b v) (t/ref-set (cadr (cdddr b)) v))
(define (body-vz b) (t/deref (caddr (cdddr b))))
(define (set-body-vz! b v) (t/ref-set (caddr (cdddr b)) v))
(define (body-mass b) (t/deref (cadddr (cdddr b))))
(define (set-body-mass! b v) (t/ref-set (cadddr (cdddr b)) v))

(define *sun*
  (make-body 0.0 0.0 0.0 0.0 0.0 0.0 *solar-mass*))

(define *jupiter*
  (make-body 4.84143144246472090
             -1.16032004402742839
             -0.103622044471123109
             (* 0.00166007664274403694 *days-per-year*)
             (* 0.00769901118419740425 *days-per-year*)
             (* -0.0000690460016972063023 *days-per-year*)
             (* 0.000954791938424326609 *solar-mass*)))

(define *saturn*
  (make-body 8.34336671824457987
             4.12479856412430479
             -0.403523417114321381
             (* -0.00276742510726862411 *days-per-year*)
             (* 0.00499852801234917238 *days-per-year*)
             (* 0.0000230417297573763929 *days-per-year*)
             (* 0.000285885980666130812 *solar-mass*)))

(define *uranus*
  (make-body 12.8943695621391310
             -15.1111514016986312
             -0.223307578892655734
             (* 0.00296460137564761618 *days-per-year*)
             (* 0.00237847173959480950 *days-per-year*)
             (* -0.0000296589568540237556 *days-per-year*)
             (*  0.0000436624404335156298 *solar-mass*)))

(define *neptune*
  (make-body 15.3796971148509165
             -25.9193146099879641
             0.179258772950371181
             (* 0.00268067772490389322 *days-per-year*)
             (* 0.00162824170038242295 *days-per-year*)
             (* -0.0000951592254519715870 *days-per-year*)
             (* 0.0000515138902046611451 *solar-mass*)))

(define *system* (vector *sun* *jupiter* *saturn* *uranus* *neptune*))
(define *system-size* 5)
;; -------------------------------
(define (offset-momentum)
  (let loop-i ((i 0) (px 0.0) (py 0.0) (pz 0.0))
    (if (= i *system-size*)
      (begin
        (set-body-vx! (vector-ref *system* 0) (/ (- 0.0 px) *solar-mass*))
        (set-body-vy! (vector-ref *system* 0) (/ (- 0.0 py) *solar-mass*))
        (set-body-vz! (vector-ref *system* 0) (/ (- 0.0 pz) *solar-mass*)))
      (let ((i1 (vector-ref *system* i)))
        (loop-i (+ i 1)
                (+ px (* (body-vx i1) (body-mass i1)))
                (+ py (* (body-vy i1) (body-mass i1)))
                (+ pz (* (body-vz i1) (body-mass i1))))))))

;; -------------------------------
(define (energy)
  (let loop-o ((o 0) (e 0.0))
    (if (= o *system-size*)
      e
      (let* ((o1 (vector-ref *system* o))
             (e (+ e (* (* 0.5 (body-mass o1))
                        (+ (+ (* (body-vx o1) (body-vx o1))
                              (* (body-vy o1) (body-vy o1)))
                           (* (body-vz o1) (body-vz o1)))))))
        (let loop-i ((i (+ o 1)) (e e))
          (if (= i *system-size*)
            (loop-o (+ o 1) e)
            (let* ((i1   (vector-ref *system* i))
                   (dx   (- (body-x o1) (body-x i1)))
                   (dy   (- (body-y o1) (body-y i1)))
                   (dz   (- (body-z o1) (body-z i1)))
                   (dist (sqrt (+ (+ (* dx dx) (* dy dy)) (* dz dz))))
                   (e    (- e (/ (* (body-mass o1) (body-mass i1)) dist))))
              (loop-i (+ i 1) e))))))))

;; -------------------------------

(define (range n)
  (letrec ((loop (lambda (i acc)
                   (if (< i 0)
                       acc
                       (loop (- i 1) (cons i acc))))))
    (loop n '())))
(define (for-each-parallel f l)
  (let ((ts (map (lambda (x) (fork (f x))) l)))
    (map (lambda (t) (join t)) ts)))

(define (advance)
  (for-each-parallel
   (lambda (o)
     (let* ((o1 (vector-ref *system* o)))
       (let loop-i ((i  (+ o 1))
                    (vx (body-vx o1))
                    (vy (body-vy o1))
                    (vz (body-vz o1)))
         (if (< i *system-size*)
             (let* ((i1    (vector-ref *system* i))
                    (dx    (- (body-x o1) (body-x i1)))
                    (dy    (- (body-y o1) (body-y i1)))
                    (dz    (- (body-z o1) (body-z i1)))
                    (dist2 (+ (+ (* dx dx) (* dy dy)) (* dz dz)))
                    (mag   (/ *dt* (* dist2 (sqrt dist2))))
                    (dxmag (* dx mag))
                    (dymag (* dy mag))
                    (dzmag (* dz mag))
                    (om (body-mass o1))
                    (im (body-mass i1)))
               (set-body-vx! i1 (+ (body-vx i1) (* dxmag om)))
               (set-body-vy! i1 (+ (body-vy i1) (* dymag om)))
               (set-body-vz! i1 (+ (body-vz i1) (* dzmag om)))
               (loop-i (+ i 1)
                       (- vx (* dxmag im))
                       (- vy (* dymag im))
                       (- vz (* dzmag im))))
             (begin (set-body-vx! o1 vx)
                    (set-body-vy! o1 vy)
                    (set-body-vz! o1 vz)
                    (set-body-x! o1 (+ (body-x o1) (* *dt* vx)))
                    (set-body-y! o1 (+ (body-y o1) (* *dt* vy)))
                    (set-body-z! o1 (+ (body-z o1) (* *dt* vz))))))))
   (range (- *system-size* 1))))

;; -------------------------------

(define N (random 42))
(offset-momentum)
(display (energy)) (newline)
(map (lambda (i) (advance)) (range N))
(display (energy)) (newline)
