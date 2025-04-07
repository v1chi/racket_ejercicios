#lang racket

#|
; Defina cuatro elementos usando define, un entero, un decimal, y string, y una lambda que sume dos números

(define func (lambda (a b)
    (+ a b)
))

(func 35 4)


;● Introduzca localmente dos identificadores locales y retorne el máximo. Los valores son ingresados por pantalla

(define mayor (lambda ()
    (define x (read))
    (define y (read))
    (if (> x y ) x y)
))

(mayor)

|#

;  Lea los valores a, b, c de una ecuación cuadrática y muestre las soluciones x1 y x2


(define a (read))
(define b (read))
(define c (read))

(define discriminante 
    (sqrt ( - (* b b ) (* 4 a c)))
)

(define x1 
    (/ (+ (- 0 b) discriminante) (* 2 a))
)

(define x2 
    (/ (- (- 0 b) discriminante) (* 2 a))
)

(displayln x1)
(displayln x2)
