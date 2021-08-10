;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname FunctionDefinitionsStarter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;function-definition-starter
;functions make our code more comprehensble to others and our future selves
;functions make our code reusable
;output unique value depending upon input
(require 2htdp/image)

(define (bulb c)
  (circle 20 "solid" c))

(define GREEN "green")
(define RED "red")
(define YELLOW "yellow")

(above (bulb GREEN)
       (bulb RED)
       (bulb YELLOW))

(bulb (string-append "re" "d"))