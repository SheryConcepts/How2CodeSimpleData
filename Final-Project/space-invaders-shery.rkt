;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-shery) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; Constants
;------------

(define HEIGHT 400)

(define WIDTH 600)

(define MISSILE-VELOCITY 10)

(define TANK-VELOCITY 3)

(define HIT-RANGE 10)

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define INVADER-HEIGHT/2 (/ 2 (image-height INVADER)))

(define INVADER-WIDTH/2 (/ 2 (image-width INVADER)))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define RIGHT-WALL (- WIDTH INVADER-WIDTH/2))

(define LEFT-WALL INVADER-WIDTH/2)

(define MTS (empty-scene WIDTH HEIGHT "blue"))

;==========================================================================================

;; Data Definitions:
;-------------------

(define-struct tank (x dx))
;; Tank is (make-tank Number Integer[-1,1])
;; interp. state of tank
;;     tank's y value is (HEIGHT - TANK-HEIGHT/2)
;;     x is its x-axis value in screen coordinates
;;     dx is its current state of moving
;;     -1 -> moving left, 0 -> at halt, 1 -> moving right

(define T1 (make-tank 67 0))     ; at halt
(define T2 (make-tank 56 -1))    ; going left
(define T3 (make-tank 70 1))     ; going right
#;
(define (fn-for-tank t)
  (... (tank-x t)             ; Number
       (tnak-dx t)))          ; Integer[-1,1]



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. state of an invader
;;      x is its horizontal value in screen coordinates
;;      y is its  vertical value in screen coordinates
;;      dx is its pixels per tick along x-axis

(define I1 (make-invader 89 (+ HEIGHT INVADER-HEIGHT/2) 10))                  ; just spawned, going right at velocity of 10
(define I2 (make-invader 190 89 -12))                                         ; going left, at 12 pixels per tick
(define I3 (make-invader RIGHT-WALL 123 14))                                  ; hitting right wall
(define I4 (make-invader LEFT-WALL 189 -13))                                  ; hitting left wall
(define I5 (make-invader 78 HEIGHT -12))                                      ; hitting ground
#;
(define (fn-for-invader i)
  (... (invader-x i)            ; Number
       (invader-y i)            ; Number
       (invader-dx i)))         ; Number



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. state of an missile
;;         x is x-coordinate value
;;         y is y-coordinate value

(define M1 (make-missile (tank-x T1) (+ 20 TANK-HEIGHT/2)))                             ; just created
(define M2 (make-missile 140 190))                                                      ; in air
(define M3 (make-missile (+ (invader-x I2) HIT-RANGE) (+ (invader-y I2) HIT-RANGE)))        ; hitting invader
#;
(define (fn-for-missile m)
  (... (missile-x m)             ; Number
       (missile-y m)))           ; Number



;; ListOfMissile if one of:
;; - empty
;; (cons Missile ListOfMissile)
;; interp. state of all missiles on game

(define LOM0 empty)
(define LOM1 (cons M1 empty))
(define LOM2 (cons M2 (cons M1 empty)))
(define LOM3 (cons M1 (cons M2 (cons M3 empty))))
#;
(define (fn-for-lom lom)
  (cond [(empty? loi) (...)]
        [else (...(fn-for-missile (first lom))
                  (fn-for-lom (rest lom)))]))



;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. state of all the invaders in the game

(define LOI0 empty)
(define LOI1 (cons I1 empty))
(define LOI2 (cons I1 (cons I2 empty)))
(define LOI3 (cons I3 (cons I2 (cons I1 empty))))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (...(fn-for-invader (first loi))
                  (fn-for-loi (rest loi)))]))




(define-struct game (loi lom t r?))
;; Game is (make-game ListOfInvader ListOfMissile Tank Boolean)
;; interp. state of the entire game
;;  - state of all invaders and missiles
;;  - state of tank
;;  - Boolean is game status, running/not-running

(define G0 (make-game empty empty T1 true))
(define G1 (make-game LOI1 LOM1 T2 true))
(define G2 (make-game (cons I2 LOI1) (cons M3 LOM1) T3 true)) 
#;
(define (fn-for-game g)
  (... (fn-for-loi (game-loi g))
       (fn-for-lom (game-lom g))
       (fn-for-t   (game-t g))
       (game-r? g))
  )


;;================================================================================

;; Functions
;------------

;; Game -> Game
;; start game with ...
(define (main g)
  (big-bang g                       ; Game
    (on-tick forward-game)          ; Game -> Game
    (to-draw render)              ; Game -> Image
    (on-key handle-click)           ; Game KeyEvent -> Game
    ))





;; Game -> Game
;; forward game state to next frame in animation

(check-expect (forward-game (make-game (cons (make-invader 89 (+ HEIGHT INVADER-HEIGHT/2) 10) empty)
                                       (cons (make-missile 77 (+ 20 TANK-HEIGHT/2)) empty)
                                       (make-tank (/ WIDTH 2) 0)
                                       true))
              (make-game (cons (make-invader (+ 89 10) (+ (+ HEIGHT INVADER-HEIGHT/2) 10) 10) empty)
                         (cons (make-missile 77 (+ (+ 20 TANK-HEIGHT/2) MISSILE-VELOCITY)) empty)
                         (make-tank (/ WIDTH 2) 0)
                         true))                                 ;; invader and missile just spawned -> game forward

(check-expect (forward-game (make-game (cons (make-invader RIGHT-WALL 123 14)
                                             (cons (make-invader LEFT-WALL 189 -13) empty))
                                       (cons (make-missile 140 190)
                                             (cons (make-missile (tank-x T1) (+ 20 TANK-HEIGHT/2)) empty))
                                       (make-tank 67 -1)
                                       true))
              (make-game (cons (make-invader (+ RIGHT-WALL -14) (+ 123 14) (* -1 14))
                               (cons (make-invader (+ LEFT-WALL 13) (+ 189 13) (* -1 -13)) empty))
                         (cons (make-missile 140  (+ MISSILE-VELOCITY 190))
                               (cons (make-missile (tank-x T1) (+ MISSILE-VELOCITY (+ 20 TANK-HEIGHT/2))) empty))
                         (make-tank 67 -1)
                         true))                               ;; invaders hitting walls -> change their directions

(check-expect (forward-game (make-game (cons (make-invader 187 100 15)
                                             (cons (make-invader RIGHT-WALL 123 14)
                                                   (cons (make-invader LEFT-WALL 189 -13) empty)))
                                       (cons (make-missile 140 190)
                                             (cons (make-missile (tank-x T1) (+ 20 TANK-HEIGHT/2))
                                                   (cons (make-missile 187 100) empty)))
                                       (make-tank 190 98)
                                       true))
              (make-game (cons (make-invader (+ RIGHT-WALL -14) (+ 123 14) (* -1 14))
                               (cons (make-invader (+ LEFT-WALL 13) (+ 189 13) (* -1 -13)) empty))
                         (cons (make-missile 140  (+ MISSILE-VELOCITY 190))
                               (cons (make-missile (tank-x T1) (+ MISSILE-VELOCITY (+ 20 TANK-HEIGHT/2))) empty))
                         (make-tank 190 98)
                         true))                               ;; invader and missile collided -> remove both from their respective lists

(check-expect (forward-game (make-game (cons (make-invader 187 HEIGHT -12)
                                             (cons (make-invader 187 100 15)
                                                   (cons (make-invader RIGHT-WALL 123 14)
                                                         (cons (make-invader LEFT-WALL 189 -13) empty))))
                                       (cons (make-missile 140 190)
                                             (cons (make-missile (tank-x T1) (+ 20 TANK-HEIGHT/2))
                                                   (cons (make-missile 187 100) empty)))
                                       (make-tank 190 98)
                                       true))
              (make-game empty empty (make-tank 190 98) false ))
;; game over dude, invader hit the ground
;; (define (forward-game g) g)  ;stub

(define (forward-game g)
  (cond [(game-over? (game-loi g)) (make-game empty empty (game-t g) false)]
        [else (make-game (forward-loi (change-loi (filter-loi (game-loi g) (game-lom g))))
                         (forward-lom (filter-lom (game-lom g) (game-loi g)))
                         (game-t g)
                         (game-r? g))])
  )




;; ListOfInvader -> Boolean
;; if invader-y equals HEIGHT return true

(check-expect (game-over? empty) false)

(check-expect (game-over? (cons (make-invader 187 HEIGHT -12)
                                (cons (make-invader 187 100 15)
                                      (cons (make-invader RIGHT-WALL 123 14)
                                            (cons (make-invader LEFT-WALL 189 -13) empty)))))
              true)

(check-expect (game-over? (cons (make-invader 127 129 -12)
                                (cons (make-invader 187 100 15)
                                      (cons (make-invader RIGHT-WALL 123 14)
                                            (cons (make-invader LEFT-WALL 189 -13) empty)))))
              false)
;(define (game-over? loi) false)     ;stub 

(define (game-over? loi)
  (cond [(empty? loi) false]
        [else (if (HEIGHT=? (first loi))
                  true
                  (game-over? (rest loi)))]))




;; Invader -> Boolean
;; produce true if invader-y equals HEIGHT

(check-expect (HEIGHT=? (make-invader 187 HEIGHT -12)) true)
(check-expect (HEIGHT=? (make-invader 144 198 12)) false)

;(define (HEIGHT=?  i) false)  ;stub

(define (HEIGHT=? i)
  (= (invader-y i) HEIGHT))         



;; ListOfInvader -> ListOfInvader



;; ListOfInvader ListOfMissile -> ListOfInvades
;; remove Invader from list if its x&y equals any of Missile

(check-expect (filter-loi empty empty) empty)

(check-expect (filter-loi (cons I1 (cons I2 empty)) empty)
              (cons I1 (cons I2 empty)))

(check-expect (filter-loi empty (cons M1 (cons M2 empty))) empty)

(check-expect (filter-loi (cons (make-invader 187 100 15)
                                (cons (make-invader RIGHT-WALL 123 14)
                                      (cons (make-invader LEFT-WALL 189 -13) empty)))
                          (cons (make-missile 140 190)
                                (cons (make-missile (tank-x T1) (+ 20 TANK-HEIGHT/2))
                                      (cons (make-missile 187 100) empty))))
              
              (cons (make-invader RIGHT-WALL 123 14)
                    (cons (make-invader LEFT-WALL 189 -13) empty)))

(check-expect (filter-loi (cons (make-invader 187 100 15)
                                (cons (make-invader RIGHT-WALL 123 14)
                                      (cons (make-invader LEFT-WALL 189 -13) empty)))
                          (cons (make-missile 140 190)
                                (cons (make-missile (tank-x T1) (+ 20 TANK-HEIGHT/2))
                                      (cons (make-missile 189 100) empty))))
              (cons (make-invader 187 100 15)
                    (cons (make-invader RIGHT-WALL 123 14)
                          (cons (make-invader LEFT-WALL 189 -13) empty))))

;(define (filter-loi loi lom) loi)   ;stub

(define (filter-loi loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (i-match? (first loi) lom)
             (filter-loi (rest loi) lom)
             (cons (first loi) (filter-loi (rest loi) lom)))]))





;; Invader ListOfMissile -> Boolean
;; If Invader-xy match any Missile-xy list return true

(check-expect (i-match? (make-invader 187 100 15) (cons (make-missile 140 190)
                                                        (cons (make-missile (tank-x T1) (+ 20 TANK-HEIGHT/2))
                                                              (cons (make-missile 187 100) empty))))
              true) 

(check-expect (i-match? (make-invader 183 121 15) (cons (make-missile 187 100)
                                                        (cons (make-missile 89 123)
                                                              (cons (make-missile 34 189) empty))))
              false)

;(define (i-match? i lom) false)  stub

(define (i-match? i lom)
  (cond [(empty? lom) false]
        [else (if (xy=? i (first lom))
                  true
                  (i-match? i (rest lom)))]))



;; Invader Missile -> Boolean
;; if invader-x,y and missile-x,y are equal return true

(check-expect (xy=? (make-invader 123 76 12) (make-missile 190 76)) false)
(check-expect (xy=? (make-invader 78 62 -12) (make-missile 78 62)) true)

;(define (xy=? i m) false)  ;stub

(define (xy=? i m)
  (and (= (invader-x i) (missile-x m)) (= (invader-y i) (missile-y m))))
      




;; ListOfMissile ListOfInvaders -> ListOfMissiles
;; remove Missile from list if its x&y equals any of Invader

(check-expect (filter-lom empty empty) empty)

(check-expect (filter-lom empty (cons (make-invader 178 90 -15) empty)) empty)

(check-expect (filter-lom (cons (make-missile 90 171) empty) empty) (cons (make-missile 90 171) empty))

(check-expect (filter-lom (cons (make-missile 140 190)
                                (cons (make-missile (tank-x T1) (+ 20 TANK-HEIGHT/2))
                                      (cons (make-missile 187 100) empty)))
                          (cons (make-invader 187 100 15)
                                (cons (make-invader RIGHT-WALL 123 14)
                                      (cons (make-invader LEFT-WALL 189 -13) empty))))
              (cons (make-missile 140 190)
                    (cons (make-missile (tank-x T1) (+ 20 TANK-HEIGHT/2)) empty)))

(check-expect (filter-lom (cons (make-missile 140 190)
                                (cons (make-missile (tank-x T1) (+ 20 TANK-HEIGHT/2))
                                      (cons (make-missile 189 100) empty)))
                          (cons (make-invader 187 100 15)
                                (cons (make-invader RIGHT-WALL 123 14)
                                      (cons (make-invader LEFT-WALL 189 -13) empty))))
              (cons (make-missile 140 190)
                    (cons (make-missile (tank-x T1) (+ 20 TANK-HEIGHT/2))
                          (cons (make-missile 189 100) empty)))) 

;(define (filter-lom lom loi) lom)   ;stub

(define (filter-lom lom loi)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else (if (m-match? (first lom) loi)
                  (filter-lom (rest lom) loi)
                  (cons (first lom) (filter-lom (rest lom) loi)))]))


;; Missile ListOfInvader -> Boolean
;; produce true if Missile-xy match any of Invader-xy
;<examples have been covered in parent function>
(define (m-match? m loi)
  (cond [(empty? loi) false]
        [else (if (m-xy=? m (first loi))
                  true
                  (m-match? m (rest loi)))]))


;; Missile Invader -> Boolean
;; produce true if Missile-xy equals Invader-xy
;<_examples are covered in parent function_>
(define (m-xy=? m i)
  (and (= (invader-x i) (missile-x m)) (= (invader-y i) (missile-y m))))




;; ListOfMissile -> ListOfMissile
;; increase y of every Missile by Missile-Velocity
(check-expect (forward-lom empty) empty)
(check-expect (forward-lom (cons (make-missile 67 190) (cons (make-missile 190 172) empty)))
              (cons (make-missile 67 (+ MISSILE-VELOCITY 190))
                    (cons (make-missile 190 (+ MISSILE-VELOCITY 172)) empty)))
;(define (forward-lom lom) lom)   ;stub
 
(define (forward-lom lom)
  (cond [(empty? lom) empty]
        [else (cons (forward-missile (first lom))
                    (forward-lom (rest lom)))]))




;; Missile -> Missile
;; increase Missile-y by MISSILE-VELOCITY
(check-expect (forward-missile (make-missile 67 190)) (make-missile 67 (+ MISSILE-VELOCITY 190)))
; (define (forward-missile m) m)    ;stub
(define (forward-missile m)
  (make-missile (missile-x m)            
                (+ MISSILE-VELOCITY (missile-y m))))           




;; ListOfInvader -> ListOfInvader
;; increase x&y of Invader by (invader-dx i)

(check-expect (forward-loi empty ) empty)

(check-expect (forward-loi (cons (make-invader 190 70 12)
                                 (cons (make-invader 80 60 -15)
                                       (cons (make-invader 5 90 -12)
                                             (cons (make-invader (- WIDTH 5) 190 14 )empty)))))
              (cons (make-invader (+ 190 12) (+ 70 12) 12)
                    (cons (make-invader (+ 80 -15) (+ 60 (* -1 -15)) -15)
                          (cons (make-invader LEFT-WALL (+ 90 (* -1 -12)) -12)
                                (cons (make-invader RIGHT-WALL (+ 190 14) 14)empty)))))


;(define (forward-loi loi) loi)     ;stub

(define (forward-loi loi)
  (cond [(empty? loi) empty]
        [else (cons (forward-invader (first loi))
                    (forward-loi (rest loi)))]))




;; Invader -> Invader
;; increase/decrease invader-xy by invader-dx

(check-expect (forward-invader (make-invader 190 70 12)) (make-invader (+ 190 12) (+ 70 12) 12))

(check-expect (forward-invader (make-invader 80 60 -15)) (make-invader (+ 80 -15) (+ 60 15) -15))

(check-expect (forward-invader (make-invader 5 90 -12)) (make-invader LEFT-WALL (+ 90 12) -12))

(check-expect (forward-invader (make-invader (- WIDTH 5) 190 14 )) (make-invader RIGHT-WALL (+ 190 14) 14))
;(define (forward-invader i) i)    ;stub

(define (forward-invader i)
  (cond [(< (- RIGHT-WALL
                (invader-x i))
             (invader-dx i)) (make-invader
                              RIGHT-WALL
                              (+ (invader-y i) (always-positive (invader-dx i)))
                              (invader-dx i))]
        [(< (- (invader-x i)
                LEFT-WALL)
             (always-positive(invader-dx i))) (make-invader LEFT-WALL
                                           (+ (invader-y i) (always-positive (invader-dx i)))
                                           (invader-dx i))]
        [(or (= (invader-x i) LEFT-WALL)
             (= (invader-x i) RIGHT-WALL)) (make-invader (+ LEFT-WALL (invader-dx i))
                                           (+ (invader-y i) (always-positive (invader-dx i)))
                                           (invader-dx i))]
        [else (make-invader (+ (invader-x i) (invader-dx i))            
                            (+ (invader-y i) (always-positive (invader-dx i)))            
                            (invader-dx i))]))




;; Number -> Number
;; always return postive Number
(check-expect (always-positive 12) 12)
(check-expect (always-positive -12) 12)

(define (always-positive n)
  (if (< n 0)
      (* -1 n)
      n)
  )






;; ListOfInvader -> ListOfInvader
;; change direction if hiting any wall

(check-expect (change-loi empty) empty)

(check-expect (change-loi (cons (make-invader 190 70 12) (cons (make-invader 80 60 -15) empty)))
              (cons (make-invader 190 70 12) (cons (make-invader 80 60 -15) empty)))

(check-expect (change-loi (cons (make-invader RIGHT-WALL 170 15) (cons (make-invader LEFT-WALL 200 -11) empty)))
              (cons (make-invader RIGHT-WALL 170 (* -1 15)) (cons (make-invader LEFT-WALL 200 (* -1 -11)) empty)))


;(define (change-loi loi) loi)   ;stub

(define (change-loi loi)
  (cond [(empty? loi) empty]
        [else (cons (change-invader (first loi))
                    (change-loi (rest loi)))]))
                            




;; Invader -> Invader
;; multiply invader-dx by -1 if invader-x equals LEFT-WALL or RIGHT-WALL return invader

(check-expect (change-invader (make-invader 170 187 -12)) (make-invader 170 187 -12))

(check-expect (change-invader (make-invader RIGHT-WALL 127 -12)) (make-invader RIGHT-WALL 127 (* -1 -12)))

;(define (change-invader i) i)    ;stub

(define (change-invader i)
  (cond [(or (= (invader-x i) LEFT-WALL) (= (invader-x i) RIGHT-WALL))
         (make-invader (invader-x i)            
                       (invader-y i)            
                       (* -1 (invader-dx i)))]
        [else i]))
  



;; Game -> Image
;; Produce image of of current state of game
;; !!!
(define (render g) MTS)



;; Game KeyEvent -> Game
;; handle state changes wen clicks are pressed
;; !!!
(define (handle-click g ke) g)


