;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants

(define HEIGHT 400)

(define WIDTH 300)

(define INVADER-VELOCITY 2)

(define TANK-VELOCITY 1.5)

(define MISSILE-VELOCITY 1)

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

(define TANK-Y (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define MTS (empty-scene WIDTH HEIGHT))

;; ========================================================================================


;; Data Definitions:

(define-struct tank (x dx))
;; Tank is (make-tank Number Integer[-1,1])
;; inrep. current state of tank
;;         x is x-coordinates of screen
;;        dx is directions of Tank
;;        0 for halt, 1 for right, -1 for left
(define T1 (make-tank (/ WIDTH 2) 0))             ; center a halt
(define T2 (make-tank 45 1))                      ; going right
(define T3 (make-tank 60 -1))                     ; going left
#;
(define (fn-for-tank t)
  (... (tank-x t)
       (tank-dx t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Integer)
;; interp. state of Invader
;;     x and y are its scrren coordinates
;;     integer is its direction across x-axis
;;     -1 for left, 1 for right
(define I1 (make-invader 50 60 1))                                               ; traveling diagonally in right direction
(define I2 (make-invader (- WIDTH (/ (image-width INVADER) 2))                    ; hitting right wall
                         90
                         1))
(define I3 (make-invader (+ 0 (/ (image-width INVADER) 2))                        ; hitting left wall
                         73    
                         -1))
(define I4 (make-invader 90 (+ 0 (/ (image-height INVADER) 2)) 1))                ; hitting ground
#;
(define (fn-for-invader i)
  (... (invader-x i)
       (invader-y i)
       (invader-dx i)))



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. x and y screen coordinates of missile
(define M1 (make-missile 90 (- HEIGHT TANK-Y)))                                 ; just released out of tank
(define M2 (make-missile 90 140))                                    ; traveling in air
(define M3 (make-missile (invader-x I1) (invader-y I1)))             ; hitting invader
#;
(define (fn-for-missile m)
  (... (missile-x m)
       (missile-y m)))


;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. state of all invaders currently in game
(define LOI1 empty)
(define LOI3 (cons I1 empty))
(define LOI2 (cons I1 (cons I2 (cons I3 (cons I4 empty)))))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (fn-for-invader (first loi))
                   (fn-for-loi (rest loi)))]))


;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. state of all Missiles
(define LOM1 empty)
(define LOM2 (cons M1 (cons M2 (cons M3 empty))))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (fn-for-missile (first lom))
                   (fn-for-lom (rest lom)))]))


(define-struct game (loi lom t))
;; Game is (make-game ListOfInvader ListOfMissile Tank)
;; interp. current state of the game, with current number of
;;         invaders, missiles and the tank.
(define G1 (make-game LOI2 LOM2 T1))
#;
(define (fn-for-game g)
  (...(fn-for-loi (game-loi g))
      (fn-for-loi (game-lom g))
      (game-t g)))

;;=================================================
;; Functions:

;; Game -> Game
;; start the game with ...
(define (main g)
  (big-bang g
    (on-tick forward)                        ;; Game -> Game
    (to-draw render)                         ;; Game -> Image
    (on-key handle-key-press)                ;; Game KeyEvent -> Game
    (on-release handle-key-release           ;; Game KeyEvent -> Game
                )))



;; Game -> Game
;; forward the state of game
;; (define (forward g) g) ;stu
(define (forward g)
  (cond [(destroyed? (game-loi g) (game-lom g)) (remove-destroyed g)] 
        [else
         (make-game (handle-invaders (game-loi g))
                    (handle-missiles (game-lom g))
                    (game-t g))]))
 


;; ListOfInvaders ListOfMissiles -> Boolean
;; produce true if missile-x&y = any invader-x&y
(check-expect (destroyed? (cons I1 (cons I2 (cons I3 empty))) (cons M1 (cons M2 (cons M3 empty)))) true)
(check-expect (destroyed? (cons I2 (cons I3 empty)) (cons M1 (cons M2 empty))) false)
;; (define (destroyed? g) false)   ;stub
(define (destroyed? loi lom)
  (cond [(or (empty? loi) (empty? lom)) false]
        [else (if (xy-align? loi (first lom))
                  true
                  (destroyed? loi (rest lom)))])) 



;; ListOfInvader Missile -> Boolean
;; check of Misslile-xy equal any Invader-xy
(check-expect (xy-align? (cons I1 (cons I2 (cons I3 empty))) M3) true)
(check-expect (xy-align? (cons I2 (cons I3 empty)) M1) false)
;;(define (xy-align? loi m) false)  ; stub
(define (xy-align? loi m)
  (cond [(empty? loi) false]
        [else (if (inv-mis-xy=? (first loi) m)
                  true
                  (xy-align? (rest loi) m))]))


;; Missile Invader -> Boolean
;; produce true if Missile-xy equals Invader-xy
(check-expect (inv-mis-xy=? I1 M3) true)
(check-expect (inv-mis-xy=? I2 M3) false)
;; (define (inv-mis-xy=? m i) false)     ;stub
(define (inv-mis-xy=? i m)
  (if (and (= (missile-x m) (invader-x i)) (= (missile-y m) (invader-y i)))
      true
      false))
  


;; Game -> Game
;; remove destroyed items in LOI and LOM
(check-expect (remove-destroyed (make-game (cons I1 (cons I2 (cons I3 empty))) (cons M1 (cons M2 (cons M3 empty))) T1))
              (make-game (cons I2 (cons I3 empty)) (cons M1 (cons M2 empty)) T1))

(check-expect (remove-destroyed (make-game (cons I1 (cons I2 (cons I3 empty))) (cons M1 (cons M2 empty)) T1))
              (make-game (cons I1 (cons I2 (cons I3  empty))) (cons M1 (cons M2 empty)) T1))
                                
 
;; (define (remove-destroyed g) g)  ;stub

(define (remove-destroyed g)
  (make-game (remove-invader (game-loi g) (game-lom g))
             (remove-missile (game-lom g) (game-loi g))
             (game-t g)))



;; ListOInvaders ListOfMissiles -> ListOfInvader
;; remove the matching item from the first list and return remaining list
;; if mathcing item not found return first list 
(check-expect (remove-intersection (cons I1 (cons I2 (cons I3 empty))) (cons M1 (cons M2 (cons M3 empty))))
              (cons I2 (cons I3 empty)))

(check-expect (remove-intersection (cons I1 (cons I2 (cons I3 empty))) (cons M1 (cons M2 empty)))
              (cons I1 (cons I2 (cons I3 empty))))

;; (define (remove-invaders loi lom) loi)  ;stub
(define (remove-invaders loi lom)
  (cond [(empty? loi) empty]
        [else (cons (remove-invader (first loi) lom)
                    (remove-invaders (rest loi) lom))]))



;; Invader ListOfMissiles -> Invader
;; if Invader is in ListOfMissile return false
(define (remove


;; Game -> Game
;; detect if any invader-y is equal to HEIGHT
;; !!!
(define (game-over? g) g)
        

;; Game -> Game
;; produce game over state !!!???
;; !!!
(define (over g) g)


;; ListOfMissile -> ListOfMissile
(check-expect (handle-missiles empty) empty)
(check-expect (handle-missiles (cons (make-missile 45 78) (cons (make-missile 89 100) empty)))
              (cons (make-missile (+ 45 MISSILE-VELOCITY) (+ 78 MISSILE-VELOCITY))
                    (cons (make-missile (+ 89 MISSILE-VELOCITY) (+ 100 MISSILE-VELOCITY)) empty)))
;; move missles forward
;; (define (handle-missiles lom) lom)  ;stub
(define (handle-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (handle-missile (first lom))
                    (handle-missiles (rest lom)))]))



;; Missile -> Missile
;; increase x and y of missile by speed
(check-expect (handle-missile (make-missile 89 100)) (make-missile (+ 89 MISSILE-VELOCITY) (+ 100 MISSILE-VELOCITY)))
;; (define (handle-missile m) m) ;stub
(define (handle-missile m)
  (make-missile (+ MISSILE-VELOCITY (missile-x m))
                (+ (missile-y m) MISSILE-VELOCITY)))



;; ListOfInvador -> ListOfInvador
;; create and move invaders 
(define (handle-invaders loi)
  (move-invaders (spawn-invaders loi)))




;; ListOfInvaders -> ListOfInvaders
;; move invaders at 45 degree angle
(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (move-invader (first loi))
                    (move-invaders (rest loi)))]))


;; Invader -> Invader
;; move single invader at 45 degree
;; along right initially, when hit change its direction
(check-expect (move-invader (make-invader 50 60 1))                                                                   
              (make-invader (+ 50 INVADER-VELOCITY) (+ 60 INVADER-VELOCITY) 1))                   ;; moving to right, diagonally

(check-expect (move-invader (make-invader 50 60 -1))                                                                   
              (make-invader (- 50 INVADER-VELOCITY) (+ 60 INVADER-VELOCITY) -1))                   ;; move to left, diagonally       

;; (define (move-invader i) i) ;stub

(define (move-invader i)
  (cond [(hitting? i) (move (change-direction i))]
        [else (move i)]
        ))




;; Invader -> Boolean
;; produce true if invader is hitting any wall
(check-expect (hitting? (make-invader (- WIDTH (/ (image-width INVADER) 2)) 90 1)) true)
(check-expect (hitting? (make-invader (+ 0 (/ (image-width INVADER) 2)) 90 -1)) true)
(check-expect (hitting? (make-invader 130 80 1)) false)
                       
;; (define (hitting? i) false) ;stub
(define (hitting? i)
  (cond [(or (= (invader-x i) (- WIDTH (/ (image-width INVADER) 2)))
             (= (invader-x i) (+ 0 (/ (image-width INVADER) 2))))
         true]
        [else false]
        ))




;; Invader -> Invader
;; change direction of invader
(check-expect (change-direction (make-invader (- WIDTH (/ (image-width INVADER) 2)) 90 1))
              (make-invader (- WIDTH (/ (image-width INVADER) 2))90 -1))           ;; change direction from right to left

(check-expect (change-direction (make-invader (+ 0 (/ (image-width INVADER) 2)) 90 -1))
              (make-invader (+ 0 (/ (image-width INVADER) 2)) 90 1))           ;; change direction from left to right       
 
;; (define (change-direction i)  i)  ;stub

(define (change-direction i)
  (cond [(= (invader-dx i) 1)
         (make-invader (invader-x i)
                       (invader-y i)
                       -1)]
        [(= (invader-dx i) -1)
         (make-invader (invader-x i)
                       (invader-y i)
                       1)]))




;; Invader -> Invader
;; move invader 1 step
;; < tests have been done in parent function
;; (define (move i) i)  ;stub
(define (move i)
  (cond [(= (invader-dx i) 1)
         (make-invader (+ INVADER-VELOCITY (invader-x i))
                       (+ INVADER-VELOCITY (invader-y i))
                       1)]
        [(= (invader-dx i) -1)
         (make-invader  (- (invader-x i) INVADER-VELOCITY)
                        (+ INVADER-VELOCITY (invader-y i))
                        -1)]))





         
;; ListOfInvaders -> ListOfInvader
;; spawn invaders at random position, add them to list
(define (spawn-invaders loi)
  (cond [(empty? loi) (cons (spawn 600) empty)]
        [else (if (< (length loi) 4)
                  (cons (spawn 600) loi)
                  loi)]
        )
  )



;; Number -> Invader
;; spawn single invader at random position
(define (spawn n)
  (make-invader WIDTH (random n) 1)  
  )


;; Game -> Image
;; produce image of current state of game
;; !!!
(define (render g) MTS)



;; Game KeyEvent -> Game
;; move tank when arrow-keys are pressed and shoot when x is pressed
;; <tests are covered by child functions>
;; (define (handle-key-press g ke) g)          ;stub

(define (handle-key-press g ke)
  (cond [(key=? ke "x") (create-missile g) ]
        [(key=? ke "left") (move-tank-left g)]
        [(key=? ke "right") (move-tank-right g)]
        [else g]))



;; Game KeyEvent -> Game
;; reset the tank-dx state to 0
(check-expect (handle-key-release (make-game LOI2 LOM2 (make-tank 87 0)) "x")
              (make-game LOI2 LOM2 (make-tank 87 0)))

(check-expect (handle-key-release (make-game LOI2 LOM2 (make-tank 84 -1)) "left")
              (make-game LOI2 LOM2 (make-tank 84 0)))

(check-expect (handle-key-release (make-game LOI2 LOM2 (make-tank 89 1)) "right")
              (make-game LOI2 LOM2 (make-tank 89 0)))
;; (define (handle-key-release g ke) g)   ;stub
(define (handle-key-release g ke)
  (if (or (key=? ke "left") (key=? ke "right"))
      (make-game (game-loi g)
                 (game-lom g)
                 (make-tank (tank-x (game-t g)) 0))
      g))





;; Game -> Game
;; create missile and add it to list
(check-expect (create-missile (make-game LOI2 empty T1))
              (make-game LOI2 (cons (make-missile (tank-x T1) TANK-Y) empty) T1))

(check-expect (create-missile (make-game LOI2 LOM2 T1))
              (make-game LOI2 (cons (make-missile (tank-x T1) TANK-Y) LOM2) T1))
;; (define (create-missiles g) g)  ;stub

(define (create-missile g)
  (cond [(empty? (game-lom g))
         (make-game (game-loi g)
                    (cons (make-missile (tank-x (game-t g)) TANK-Y) empty)
                    (game-t g))]
        [else
         (make-game (game-loi g)
                    (cons (make-missile (tank-x (game-t g)) TANK-Y) (game-lom g))
                    (game-t g))]))
  


;; Game -> Game
;; increase tank-x by TANK-VELOCITY
(check-expect (move-tank-left (make-game LOI2 LOM2 (make-tank 87 0)))
              (make-game LOI2 LOM2 (make-tank (+ 87 TANK-VELOCITY) 1))) 
;;(define (move-tank-left g) g)  ; stub
(define (move-tank-left g)
  (make-game (game-loi g)
             (game-lom g)
             (make-tank (+ (tank-x (game-t g)) TANK-VELOCITY) 1)))




;; Game -> Game
;; decrease tank-x by TANK-VELOCITY
(check-expect (move-tank-right (make-game LOI2 LOM2 (make-tank 87 0)))
              (make-game LOI2 LOM2 (make-tank (- TANK-VELOCITY 87) -1))) 
;;(define (move-tank-left g) g)  ; stub
(define (move-tank-right g)
  (make-game (game-loi g)
             (game-lom g)
             (make-tank (- TANK-VELOCITY (tank-x (game-t g))) -1)))





