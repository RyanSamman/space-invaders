;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 4)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT "Midnight Blue"))
(define BLANK (rectangle WIDTH HEIGHT "outline" "black"))

(define INVADER (bitmap "Java.png"))
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))
(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))

(define TANK (bitmap "Cannon.png"))
(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))

(define MISSILE (bitmap "BMissile.png"))
#;
(define MISSILE (ellipse 5 15 "solid" "red"))
(define MISSILE-WIDTH/2 (/ (image-width MISSILE) 2))
(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1
              
(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left
(define TR (make-tank TANK-WIDTH/2 1))  ; Maximum Length
(define TL (make-tank (- WIDTH TANK-WIDTH/2) 1)) ; Maximum Left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define IBL (make-invader INVADER-WIDTH/2 INVADER-HEIGHT/2 0))
(define IREACHED (make-invader 150 0 1))
(define I1 (make-invader 150 400 1))           ;not landed, moving right
(define I2 (make-invader 150 INVADER-HEIGHT/2 -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ INVADER-HEIGHT/2 10) 1)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (invader-y I1)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1) (image-height MISSILE))))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define GE (make-game (list ) empty T0))


;; =================
;; Functions:

;; Number Number Number -> Number
(check-expect (clamp 1 10 5) 5)
(check-expect (clamp 1 10 11) 10)
(check-expect (clamp 1 10 0) 1)

#;
(define (clamp min max i) 0)

(define (clamp min max i)
  (cond [(>= i max) max]
        [(<= i min) min]
        [else i]))

;; Game -> Game
;; start the world with a tank
;; 
(define (main g)
  (big-bang g                 ; Game
    (on-tick   tock)          ; Game -> Game
    (stop-when stop?)         ; Game -> Boolean
    (to-draw   render)        ; Game -> Image
    (on-key    handle-key)))  ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game instance

(define (tock g)
  (make-game (tick-loinvader (game-invaders g) (game-missiles g))
             (tick-lomissiles (game-missiles g) (game-invaders g))
             (tick-tank (game-tank g))))

(define (tick-tank t)
  (make-tank
   (clamp TANK-WIDTH/2 (- WIDTH TANK-WIDTH/2) (+ (tank-x t) (* TANK-SPEED (tank-dir t))))
   (tank-dir t)))

(define (tick-loinvader loi lom)
  (cond
    [(empty? loi) (generate-invader 6)] ; End of List
    [(i-intersects-lom (first loi) lom) (tick-loinvader (rest loi) lom)] ; Remove if intersects a missile
    [(<= (invader-x (first loi)) 0)
     (cons
      (make-invader INVADER-WIDTH/2 (- (invader-y (first loi)) INVADER-Y-SPEED) +1)
      (tick-loinvader (rest loi) lom))]
    [(>= (invader-x (first loi)) WIDTH) (cons
      (make-invader (- WIDTH INVADER-WIDTH/2) (- (invader-y (first loi)) INVADER-Y-SPEED) -1)
      (tick-loinvader (rest loi) lom))]
    [else (cons
           (make-invader
            (+ (invader-x (first loi)) (* INVADER-X-SPEED (invader-dx (first loi))))
            (- (invader-y (first loi)) INVADER-Y-SPEED)        
            (invader-dx (first loi)))
           (tick-loinvader (rest loi) lom))]))

(define (generate-invader s) 
  (cond [(> (random 100) 96) (cons
            (make-invader 
              (+ (random (- WIDTH (image-width INVADER))) (image-width INVADER) )
              (- HEIGHT INVADER-HEIGHT/2)
              (if (= (random 2) 0) -1 1)) empty)] ; != 0   
        [else empty]))

;; -1 1

(define (i-intersects-lom i lom)
  (cond [(empty? lom) false] ;; No missile intersected
        [(m-hit-i (first lom) i) true] ;; Intersected one missile
        [else (i-intersects-lom i (rest lom))]))

(define (tick-lomissiles lom loi)
  (cond
    [(empty? lom) empty] ; End of List
    [(> (missile-y (first lom))(- HEIGHT MISSILE-HEIGHT/2)) (tick-lomissiles (rest lom) loi)] ; Remove node if exits bounds
    [(m-intersects-loi (first lom) loi) (tick-lomissiles (rest lom) loi)] ; Remove if intersects an invader
    [else (cons
           (make-missile
            (missile-x (first lom))
            (+ (missile-y (first lom)) MISSILE-SPEED))
           (tick-lomissiles (rest lom) loi))]))

(define (m-intersects-loi m loi)
  (cond [(empty? loi) false] ;; No invader intersected
        [(m-hit-i m (first loi)) true] ;; Intersected one invader
        [else (m-intersects-loi m (rest loi))]))

(check-expect (m-hit-i (make-missile 600 400) (make-invader 300 200 0)) false)
(check-expect (m-hit-i (make-missile 300 200) (make-invader 300 200 0)) true)
(check-expect (m-hit-i (make-missile 320 200) (make-invader 315 200 0)) true)
(check-expect (m-hit-i (make-missile 300 220) (make-invader 300 215 0)) true)

;; Missile Invader -> Boolean
(define (m-hit-i m i)
  (and [< (abs (- (missile-x m) (invader-x i))) INVADER-WIDTH/2]
       [< (abs (- (missile-y m) (invader-y i))) INVADER-HEIGHT/2]))

#;
(define (fn-for-game s)
  (make-game
   (fn-for-loinvader (game-invaders s))
   (fn-for-lom (game-missiles s))
   (fn-for-tank (game-tank s))))

;; Game -> Image
;; renders the game struct into an image

#;
(define (render g) BACKGROUND)

(define (render s)
  (overlay
   (render-invaders (game-invaders s))
   (render-missiles (game-missiles s))
   (render-tank (game-tank s))
   BACKGROUND)) 

;; Tank -> Image
(check-expect (render-tank T0)
              (overlay/align/offset "right" "bottom" TANK (- (tank-x T0) TANK-WIDTH/2) 0 BLANK))
(check-expect (render-tank TR)
              (overlay/align/offset "right" "bottom" TANK (- (tank-x TR) TANK-WIDTH/2) 0 BLANK))
(check-expect (render-tank TL)
              (overlay/align/offset "right" "bottom" TANK (- (tank-x TL) TANK-WIDTH/2) 0 BLANK))


(define (render-tank t)
  (overlay/align/offset
   "right" "bottom"
   TANK
   (- (clamp TANK-WIDTH/2 (- WIDTH TANK-WIDTH/2) (tank-x t)) TANK-WIDTH/2) 0
   BLANK))

;; ListOfMissiles -> Image
(check-expect (render-missiles empty) BLANK)
(check-expect (render-missiles (cons (make-missile MISSILE-WIDTH/2 MISSILE-HEIGHT/2) empty))
              (overlay/align/offset
               "right" "bottom"
               MISSILE
               0 0 BLANK))
(check-expect (render-missiles (cons (make-missile MISSILE-WIDTH/2 MISSILE-HEIGHT/2) (cons M1 empty)))
              (overlay/align/offset
               "right" "bottom"
               MISSILE
               0 0
               (overlay/align/offset
                "right" "bottom"
                MISSILE
                (- (missile-x M1) MISSILE-WIDTH/2)
                (- (missile-y M1) MISSILE-HEIGHT/2)
                BLANK)))

(define (render-missiles lom)
  (cond [(empty? lom) BLANK]
        [else (overlay/align/offset
               "right" "bottom"
               MISSILE
               (- (clamp MISSILE-WIDTH/2 (- WIDTH MISSILE-WIDTH/2) (missile-x (first lom))) MISSILE-WIDTH/2)
               (- (clamp MISSILE-HEIGHT/2 (- HEIGHT MISSILE-HEIGHT/2) (missile-y (first lom))) MISSILE-HEIGHT/2)
               (render-missiles (rest lom)))]))


;; ListOfInvaders -> Image
(check-expect (render-invaders empty) BLANK)
(check-expect (render-invaders (cons IBL empty))
              (overlay/align/offset
               "right" "bottom"
               INVADER
               0 0 BLANK))
(check-expect (render-invaders (cons IBL (cons I1 empty)))
              (overlay/align/offset
               "right" "bottom"
               INVADER
               0 0
               (overlay/align/offset
                "right" "bottom"
                INVADER
                (- (invader-x I1) INVADER-WIDTH/2)
                (- (invader-y I1) INVADER-HEIGHT/2)
                BLANK)))

(define (render-invaders loi)
  (cond [(empty? loi) BLANK]
        [else (overlay/align/offset
               "right" "bottom"
               INVADER
               (- (clamp INVADER-WIDTH/2 (- WIDTH INVADER-WIDTH/2) (invader-x (first loi))) INVADER-WIDTH/2)
               (- (clamp INVADER-HEIGHT/2 (- HEIGHT INVADER-HEIGHT/2) (invader-y (first loi))) INVADER-HEIGHT/2)
               (render-invaders (rest loi)))]))

;; Game KeyEvent -> Game
;; Handle keys being pressed
(check-expect (handle-key (make-game empty empty T1) " ") (make-game empty (list (make-missile (tank-x T1) (image-height TANK))) T1))
(check-expect (handle-key (make-game empty empty T2)  "a") (make-game empty empty T1))
(check-expect (handle-key (make-game empty empty T1)  "d") (make-game empty empty T2))

(define (handle-key g ke)
  (cond [(key=? ke "a")
         (make-game (game-invaders g) (game-missiles g) (set-dir (game-tank g) 1))]
        [(key=? ke "d")
         (make-game (game-invaders g) (game-missiles g) (set-dir (game-tank g) -1))]
        [(key=? ke " ")
         (make-game (game-invaders g) (shoot-missile (game-tank g) (game-missiles g)) (game-tank g))]
        [else g]))

;; Tank ListOfMissiles -> ListOfMissiles
(check-expect (shoot-missile T1 empty) (list (make-missile (tank-x T1) (image-height TANK))))

(define (shoot-missile t lom)
  (cons (make-missile (tank-x t) (image-height TANK)) lom))

;; Tank -> Tank
;; Swaps the direction
(check-expect (set-dir T1 1) T1)
(check-expect (set-dir T1 -1) T2)
(check-expect (set-dir T2 -1) T2)
(check-expect (set-dir T2 1) T1) 

(define
  (set-dir t dir)
  (make-tank (tank-x t) dir))

;; Game -> Boolean
;; Checks if the game is in a state where it should stop
(check-expect (stop? (make-game (list IREACHED) empty T0)) true)
(check-expect (stop? G1) false)

(define (stop? g) (invader-escape (game-invaders g)))

(define (invader-escape loi)
  (cond [(empty? loi) false]
        [(< (invader-y (first loi)) INVADER-HEIGHT/2) true]
        [else (invader-escape (rest loi))]))

(main G1)
