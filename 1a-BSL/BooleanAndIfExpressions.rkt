;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname BooleanAndIfExpressions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Predecates are the functions that return boolean (true/false) values
(require 2htdp/image)

true
false

(define HEIGHT 100)
(define WIDTH 200)

(= HEIGHT WIDTH)
(> HEIGHT WIDTH)
(< HEIGHT WIDTH)

(define R1 (rectangle HEIGHT WIDTH "solid" "red"))
(define R2 (rectangle WIDTH HEIGHT "solid" "red"))

(> (image-width R1)
   (image-width R2))

;if expressions let us do branching in our code
;syntax:    (if <questionExpression>
;                <evaluateIfTrue>
;                <evaluteIfFalse>)

; stepper feature of DrRacket is very useful

(if (> (image-width R1)
       (image-height R1))
    "wide"
    "tall")

(and (> (image-width R1) (image-width R2))
     (<= (image-width R1) (image-width R2))
     (= (image-width R1) (image-width R2)))

(or (> (image-width R1) (image-width R2))
     (<= (image-width R1) (image-width R2))
     (= (image-width R1) (image-width R2)))