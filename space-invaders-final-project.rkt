;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  400)
(define HEIGHT 850)

(define TANK-SPEED 9)
(define MISSILE-SPEED 16)

(define TOTAL-INVADERS 15)
(define INVADER-MAX-SPEED 9)
(define INVADER-MAX-Y-ENTRY 300)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define OUTLINE (rectangle WIDTH HEIGHT "outline" "black"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))
(define TANK-Y-POSITION (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))
(define MISSILE-WIDTH/2 (/ (image-width MISSILE) 2))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listofInvader) (listofMissile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1, doesn't move if 0

(define T0 (make-tank (/ WIDTH 2) 0))   ;center motionless
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 (- HEIGHT INVADER-HEIGHT/2) -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader i)
  (... (invader-x i) (invader-y i) (invader-dx i)))


;; listofInvader is one of:
;; - empty
;; - (cons Invader listofInvader)
;; interp. a list of invaders

(define LOI0 empty)
(define LOI1 (cons I1 empty))
(define LOI2 (cons I1 (cons I2 (cons I3 empty))))
(define LOI3 (make-list TOTAL-INVADERS I1))
(define LOI4 (cons I3 (make-list TOTAL-INVADERS I1)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile 50 5))

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; listofMissile is one of:
;; - empty
;; - (cons Missile listofMissile)
;; interp. a list of missiles

(define LOM0 empty)
(define LOM1 (cons M1 empty))
(define LOM2 (cons M1 (cons M2 (cons M3 empty))))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game LOI1 LOM1 T1))
(define G3 (make-game LOI3 empty T0))
(define G4 (make-game LOI3 empty T1))
(define G5 (make-game LOI3 LOM1 T1))
(define G6 (make-game LOI4 LOM1 T1))


;; =================
;; Functions:

;; Game -> Game
;; called to start the game; start with (main G0)
;; no tests for main function
(define (main s)
  (big-bang s
    (on-tick next-frame)   ; Game -> Game
    (to-draw render-game)  ; Game -> Image
    (on-key  handle-key))) ; Game KeyEvent -> Game


;; Game -> Game
;; produces the next frame of the game
(check-expect (next-frame G3) (make-game (next-invaders LOI3 LOM0) LOM0 T0))
(check-expect (next-frame G4) (make-game (next-invaders LOI3 LOM1) LOM0 (make-tank (+ 50 TANK-SPEED) 1)))
(check-expect (next-frame G5) (make-game (next-invaders LOI3 LOM1)
                                         (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty)
                                         (make-tank (+ 50 TANK-SPEED) 1)))
(check-expect (next-frame G6) G6)

;(define (next-frame s) s) ;stub

(define (next-frame s)
  (cond [(game-over? (game-invaders s)) s]
        [else
         (make-game (next-invaders (add-invader (game-invaders s)) (game-missiles s))
                    (next-missiles (game-missiles s) (game-invaders s))
                    (next-tank (game-tank s)))]))


;; listofInvader -> Boolean
;; produces true if an invader has reached HEIGHT and causes a game over
(check-expect (game-over? LOI1) false)
(check-expect (game-over? LOI3) false)
(check-expect (game-over? LOI4) true)
(check-expect (game-over? (cons I2 empty)) true)

;(define (game-over? loi) false) ;stub

(define (game-over? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) (- HEIGHT INVADER-HEIGHT/2))
             true
             (game-over? (rest loi)))]))


;; listofInvader -> listofInvader
;; adds an invader to the loi if number of invaders is less than TOTAL-INVADERS
(check-expect (length (add-invader LOI0)) 1)
(check-expect (length (add-invader LOI2)) 4)
(check-expect (length (add-invader LOI3)) TOTAL-INVADERS)

;(define (add-invader loi) loi) ;stub

(define (add-invader loi)
  (if (not-enough-loi? loi)
      (cons (make-invader (random WIDTH) (- (random INVADER-MAX-Y-ENTRY)) (random-speed-check (random INVADER-MAX-SPEED)))
            loi)
      loi))


;; Natural -> Natural
;; produces spd if its absolute value is greater than 0, else produces new negative random number and recurs
(check-expect (random-speed-check 1) 1)
(check-range (random-speed-check 0) (- INVADER-MAX-SPEED) -1)

;(define (random-speed-check spd) 1) ;stub

(define (random-speed-check spd)
  (cond [(> (abs spd) 0) spd]
        [else
         (random-speed-check (- (random INVADER-MAX-SPEED)))]))


;; listofInvaders listofMissile -> listofInvaders
;; produces the next frame of invaders position and correct list of active invaders
(check-expect (next-invaders LOI0 LOM0) empty)
(check-expect (next-invaders LOI1 LOM0) (cons (make-invader (+ 150 12) (+ 100 12) 12) empty))
(check-expect (next-invaders LOI1 LOM2) empty)

;(define (next-invaders loi lom) loi) ;stub

(define (next-invaders loi lom)
  (cond [(empty? loi) empty]
        [(hit-invader? (first loi) lom)
         (next-invaders (rest loi) lom)]
        [else
         (cons (next-invader (first loi))
               (next-invaders (rest loi) lom))]))


;; Invader listofMissile -> Boolean
;; produces true if invader has been hit by missile
(check-expect (hit-invader? I1 LOM1) false)
(check-expect (hit-invader? I1 LOM2) true)

;(define (hit-invader? i lom) false) ;stub

(define (hit-invader? i lom)
  (cond [(empty? lom) false]
        [(and (x-hit? (first lom) i)
              (y-hit? (first lom) i))
         true]
        [else
         (hit-invader? i (rest lom))]))


;; Missile Invader -> Boolean
;; produces true if missile-x is within invader-x range
(check-expect (x-hit? M1 I1) true)
(check-expect (x-hit? M2 I1) true)
(check-expect (x-hit? M4 I1) false)

;(define (x-hit? m i) false) ;stub

(define (x-hit? m i)
  (or (<= (- (invader-x i) INVADER-WIDTH/2)
          (missile-x m)
          (+ (invader-x i) INVADER-WIDTH/2))    ;center of missile check

      (<= (- (invader-x i) INVADER-WIDTH/2)
          (- (missile-x m) MISSILE-WIDTH/2)
          (+ (invader-x i) INVADER-WIDTH/2))    ;left edge of missile check

      (<= (- (invader-x i) INVADER-WIDTH/2)
          (+ (missile-x m) MISSILE-WIDTH/2)
          (+ (invader-x i) INVADER-WIDTH/2))))  ;right edge of missile check


;; Missile Invader -> Boolean
;; produces true if missile-y or top or bottom of missile is within invader-y range
(check-expect (y-hit? M1 I1) false)
(check-expect (y-hit? M2 I1) true)
(check-expect (y-hit? M4 I1) false)

;(define (y-hit? m i) false) ;stub

(define (y-hit? m i)
  (or (<= (- (invader-y i) INVADER-HEIGHT/2)
          (missile-y m)
          (+ (invader-y i) INVADER-HEIGHT/2))     ;center of missile check
      
      (<= (- (invader-y i) INVADER-HEIGHT/2)
          (- (missile-y m) MISSILE-HEIGHT/2)
          (+ (invader-y i) INVADER-HEIGHT/2))     ;top of missile check
      
      (<= (- (invader-y i) INVADER-HEIGHT/2)
          (+ (missile-y m) MISSILE-HEIGHT/2)
          (+ (invader-y i) INVADER-HEIGHT/2))))   ;bottom of missile check


;; listofInvader -> Boolean
;; returns true if number of invaders is less than TOTAL-INVADERS
(check-expect (not-enough-loi? LOI0) true)
(check-expect (not-enough-loi? LOI2) true)
(check-expect (not-enough-loi? LOI3) false)

;(define (not-enough-loi? loi) false) ;stub

(define (not-enough-loi? loi)
  (< (length loi) TOTAL-INVADERS))


;; Invader -> Invader
;; produces the next frame for invader by increasing x & y by dx, changes direction if reaches edges
(check-expect (next-invader I1) (make-invader (+ 150 12) (+ 100 12) 12))
(check-expect (next-invader (make-invader 0 (/ HEIGHT 2) 10))             (make-invader (+ 0  10) (+ (/ HEIGHT 2) (abs 10))  10))
(check-expect (next-invader (make-invader 7 (/ HEIGHT 2) -10))            (make-invader 0 (+ (/ HEIGHT 2) (abs 10))  10))
(check-expect (next-invader (make-invader (- WIDTH 10) (/ HEIGHT 2) -10)) (make-invader (+ WIDTH -10 -10) (+ (/ HEIGHT 2) (abs -10)) -10))
(check-expect (next-invader (make-invader (- WIDTH 10) (/ HEIGHT 2) 10))  (make-invader WIDTH (+ (/ HEIGHT 2) (abs 10)) -10))
(check-expect (next-invader (make-invader (- WIDTH 5) (/ HEIGHT 2) 10))   (make-invader WIDTH (+ (/ HEIGHT 2) (abs 10)) -10))

;(define (next-invader i) i) ;stub

(define (next-invader i)
  (cond [(>= (+ (invader-x i) (invader-dx i)) WIDTH)
         (make-invader WIDTH
                       (+ (invader-y i) (abs (invader-dx i)))
                       (- (invader-dx i)))]
        [(<= (+ (invader-x i) (invader-dx i)) 0)
         (make-invader 0
                       (+ (invader-y i) (abs (invader-dx i)))
                       (- (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (invader-dx i))
                       (+ (invader-y i) (abs (invader-dx i)))
                       (invader-dx i))]))


;; listofMissile listofInvader -> listofMissile
;; produces the list of active missiles on screen, removes out of bounds missiles
(check-expect (next-missiles LOM0 LOI0) empty)
(check-expect (next-missiles LOM1 LOI0) (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty))
(check-expect (next-missiles (cons (make-missile 150 0) empty) LOI0) empty)
(check-expect (next-missiles (cons (make-missile 150 300) (cons (make-missile 150 0) empty)) LOI0) (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty))
(check-expect (next-missiles LOM2 LOI1) (cons (next-missile M1) empty))

;(define (next-missiles lom loi) lom) ;stub

(define (next-missiles lom loi)
  (cond [(empty? lom) empty]
        [(out? (first lom))
         (next-missiles (rest lom) loi)]
        [(hit-missile? (first lom) loi)
         (next-missiles (rest lom) loi)]
        [else
         (cons (next-missile (first lom))
               (next-missiles (rest lom) loi))]))


;; Missile listofInvader -> Boolean
;; produces true if missile has hit invader
(check-expect (hit-missile? M1 LOI1) false)
(check-expect (hit-missile? M2 LOI1) true)

;(define (hit-missile? m loi) false) ;stub

(define (hit-missile? m loi)
  (cond [(empty? loi) false]
        [(and (x-hit? m (first loi))
              (y-hit? m (first loi)))
         true]
        [else
         (hit-missile? m (rest loi))]))


;; Missile -> Boolean
;; produces true if missile is out of bounds
(check-expect (out? (make-missile 150 0)) true)
(check-expect (out? (make-missile 150 -1)) true)
(check-expect (out? (make-missile 150 (/ HEIGHT 2))) false)

;(define (out? m) false) ;stub

(define (out? m)
  (<= (missile-y m) 0))


;; Missile -> Missile
;; produces the next missile by subtracting y by MISSILE-SPEED
(check-expect (next-missile M1) (make-missile 150 (- 300 MISSILE-SPEED)))
(check-expect (next-missile (make-missile 100 1)) (make-missile 100 (- 1 MISSILE-SPEED)))

;(define (next-missile m) m) ;stub

(define (next-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))


;; Tank -> Tank
;; produces the next frame of tanks location based on movement
(check-expect (next-tank T0)                                    (make-tank (/ WIDTH 2)                                   0))
(check-expect (next-tank (make-tank 150                    -1)) (make-tank (+ 150 (* TANK-SPEED -1))                    -1))
(check-expect (next-tank (make-tank (+ 0 TANK-WIDTH/2)     -1)) (make-tank (+ 0 TANK-WIDTH/2)                           -1))
(check-expect (next-tank (make-tank (+ 0 TANK-WIDTH/2)      1)) (make-tank (+ (+ 0 TANK-WIDTH/2) (* TANK-SPEED 1))       1))
(check-expect (next-tank (make-tank (- WIDTH TANK-WIDTH/2) -1)) (make-tank (+ (- WIDTH TANK-WIDTH/2) (* TANK-SPEED -1)) -1))
(check-expect (next-tank (make-tank (- WIDTH TANK-WIDTH/2)  1)) (make-tank (- WIDTH TANK-WIDTH/2)                        1))
 
;(define (next-tank t) t) ;stub

(define (next-tank t)
  (cond [(>= (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (- WIDTH TANK-WIDTH/2))
         (make-tank (- WIDTH TANK-WIDTH/2) (tank-dir t))]
        [(<= (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (+ 0 TANK-WIDTH/2))
         (make-tank (+ 0 TANK-WIDTH/2) (tank-dir t))]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))


;; Game -> Image
;; renders the current state of the game as image
(check-expect (render-game G0) (overlay (render-invaders (game-invaders G0))
                                        (render-tank     (game-tank G0))
                                        (render-missiles (game-missiles G0))))

;(define (render-game s) BACKGROUND) ;stub

(define (render-game s)
  (overlay (render-invaders (game-invaders s))
           (render-tank     (game-tank s))
           (render-missiles (game-missiles s))))


;; listofInvader -> Image
;; renders an image of all the invaders
(check-expect (render-invaders LOI0) OUTLINE)
(check-expect (render-invaders LOI1) (place-image INVADER (invader-x (first LOI1)) (invader-y (first LOI1)) (render-invaders (rest LOI1))))
(check-expect (render-invaders LOI2) (place-image INVADER (invader-x (first LOI2)) (invader-y (first LOI2)) (render-invaders (rest LOI2))))

;(define (render-invaders loi) BACKGROUND) ;stub

(define (render-invaders loi)
  (cond [(empty? loi) OUTLINE]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi)))]))


;; listofMissiles -> Image
;; renders an image of all the missiles
(check-expect (render-missiles LOM0) BACKGROUND)
(check-expect (render-missiles LOM1) (place-image MISSILE (missile-x (first LOM1)) (missile-y (first LOM1)) (render-missiles (rest LOM1))))
(check-expect (render-missiles LOM2) (place-image MISSILE (missile-x (first LOM2)) (missile-y (first LOM2)) (render-missiles (rest LOM2))))

;(define (render-missiles lom) BACKGROUND) ;stub

(define (render-missiles lom)
  (cond [(empty? lom) BACKGROUND]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom)))]))


;; Tank -> Image
;; renders an image of the tank at correct position
(check-expect (render-tank T0) (place-image TANK (tank-x T0) TANK-Y-POSITION OUTLINE))
(check-expect (render-tank T1) (place-image TANK (tank-x T1) TANK-Y-POSITION OUTLINE))
(check-expect (render-tank T2) (place-image TANK (tank-x T2) TANK-Y-POSITION OUTLINE))

;(define (render-tank t) BACKGROUND) ;stub

(define (render-tank t)
  (place-image TANK (tank-x t) TANK-Y-POSITION OUTLINE))


;; Game KeyEvent -> Game
;; uses the right and left keys to move the tank at TANK-SPEED in each direction and produces a missile when spacebar is pressed
(check-expect (handle-key G0 "left")  (make-game (game-invaders G0) (game-missiles G0) (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key G0 "right") (make-game (game-invaders G0) (game-missiles G0) (make-tank (/ WIDTH 2)  1)))
(check-expect (handle-key G0 " ")     (make-game (game-invaders G0) (new-missiles G0) (game-tank G0)))
(check-expect (handle-key G1 "up")    G1)
(check-expect (handle-key G2 "down")  G2)
(check-expect (handle-key G1 "left")  (make-game (game-invaders G1) (game-missiles G1) (make-tank 50 -1)))
(check-expect (handle-key G2 "right") (make-game (game-invaders G2) (game-missiles G2) (make-tank 50  1)))
(check-expect (handle-key G2 " ")     (make-game (game-invaders G2) (new-missiles G2) (game-tank G2)))
(check-expect (handle-key G6 " ")     G6)
(check-expect (handle-key G6 "left")  G6)

;(define (handle-key s ke) s) ;stub

(define (handle-key s ke)
  (cond [(game-over? (game-invaders s))
         s]
        [(key=? ke " ") 
         (make-game (game-invaders s)
                    (new-missiles s)
                    (game-tank s))]
        [(key=? ke "left") 
         (make-game (game-invaders s)
                    (game-missiles s)
                    (make-tank (tank-x (game-tank s))
                               -1))]
        [(key=? ke "right") 
         (make-game (game-invaders s)
                    (game-missiles s)
                    (make-tank (tank-x (game-tank s))
                               1))]
        [else s]))


;; Game -> listofMissiles
;; produces a new missile at x and y position of tank
(check-expect (new-missiles G0) (cons (make-missile (/ WIDTH 2) TANK-Y-POSITION) empty))
(check-expect (new-missiles G1) (cons (make-missile 50 TANK-Y-POSITION) empty))
(check-expect (new-missiles G2) (cons (make-missile 50 TANK-Y-POSITION) LOM1))

;(define (new-missiles s) empty) ;stub

(define (new-missiles s)
  (cons (make-missile (tank-x (game-tank s)) TANK-Y-POSITION)
        (game-missiles s)))
       
