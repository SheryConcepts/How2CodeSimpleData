;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-shery) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define LEFT-WALL (INVADER-WIDTH/2))

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

(define (fn-for-tank t)
  (... (tank-x t)             ; Number
       (tnak-dx t)))          ; Integer[-1,1]



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. state of an invader
;;      x is its horizontal value in screen coordinates
;;      y is its  vertical value in screen coordinates
;;      dx is its pixels per tick along x-axis

(define I1 (make-invader 89 (+ HIEGHT INVADER-HEIGHT/2) 10))                  ; just spawned, going right at velocity of 10
(define I2 (make-invader 190 89 -12))                                         ; going left, at 12 pixels per tick
(define I3 (make-invader RIGHT-WALL 123 14))                                  ; hitting right wall
(define I4 (make-invader LEFT-WALL 189 -13))                                  ; hitting left wall
(define I4 (make-invader 78 HEIGHT -12))                                      ; hitting ground

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
(define M3 (make-missile (+ invader-x I2 HIT-RANGE) (+ invader-y I2 HIT-RANGE)))        ; hitting invader

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
(define LOM3 (cons M1 (cons M2 (M3 empty))))

(define (fn-for-lom lom)
  
  )