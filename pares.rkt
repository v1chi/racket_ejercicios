#lang racket

(define p '(7 . 2))
(define p2 '(5 . 70))


; incrementar en 1 cada elemento del par
(define pair-add1 (lambda (p)
    ( cons (+ 1 (car p))  (+ 1 (cdr p)))
))
(pair-add1 p)

; invertir valores del par
(define pair-reverse (lambda (p)
    ( cons (cdr p) ( car p))
))

(pair-reverse p)

;par con suma de 2
(define pair-add (lambda (p1 p2)
    ( cons (+ (car p1) (car p2)) ( + (cdr p1) (cdr p2)) )
))

(pair-add p p2)