#lang racket

;función que reciba una lista de largo 3 y muestre cada elemento de la lista

(define func (lambda (list)
    (displayln( car list))
    (displayln (car (cdr list)))
    (displayln (car (cdr (cdr list))))
))