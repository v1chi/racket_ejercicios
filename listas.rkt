#lang racket

; Defina una función que suma todos los elementos de una lista
(define sl(lambda (lista)
    (cond
    [(empty? lista) 0]
    [(+ (car lista) (sl (cdr lista)))]
)))

(sl '(2 3 4))

; Defina una función que reciba una lista y sume todos los números pares

(define slp(lambda (lista)
    (cond
    [(empty? lista) 0]
    [ ( = (modulo (car lista) 2) 0) 
    (+ (car lista) (slp (cdr lista)))]
    [else (slp(cdr lista))]
)))

(slp '(2 3 4 10 12 23))


; Defina una función que recibe un número entero n y retorne una lista con los numeros naturales desde n

(define gl(lambda (n)
    (if (=  n 0) '(0)
    (cons n (gl (- n 1)))
)))

(gl 5)