#lang racket

; Lista con menores que n

(define list_menor_n (lambda (n lista)
    (cond
    [(empty? lista) '()]
    [(< (car lista) n) (cons (car lista) (list_menor_n n (cdr lista)))]
    [else (list_menor_n n (cdr lista))]
)))

(list_menor_n 5 '(1 2 3 1 5 6))

; buscar n en lista

(define buscar (lambda (n lista)
    (cond
    [(empty? lista) #f]
    [(= (car lista) n) #t]
    [else (buscar n (cdr lista))]
)))

(buscar 5 '(1 2 3 1 5 6))
(buscar 5 '(1 2 3 1 6))

; rpn, repite un numero t n veces

(define rpm (lambda (t n)
    (if (= 0 n) '()
    (cons t (rpm t (- n 1))))
))

(rpm 5 3)

; TDA cola: create, pop, push

(define create(lambda () '()))
(create)

(define pop(lambda (cola)(cdr cola)))
(pop '(1 2 3))

(define push(lambda (n cola) (append cola (list n)))) ; intentar hacer con reverse
(push 40 '(1 2 3))

(push 4 (push 5 (create)))

; Generar listas decrecientes por cada elemento de la lista que se pasa por parametro

(define gl(lambda (n)
    (if (=  n 0) '(0)
    (cons n (gl (- n 1)))
)))


(define gln (lambda (lista)
    (if (empty? lista) '()
    (append (gl (car lista)) (gln (cdr lista)))
)))

(gln '(5 3 7 1 2))