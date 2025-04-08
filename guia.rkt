#lang racket

; sumar numeros primos de una lista

(define (esPrimoAux n i)
    (cond
    [ (and (< i n) (= (modulo n i) 0)) #f]
    [(= i n) #t]
    [else (esPrimoAux n (+ i 1))]
    )
)

(define es_primo(lambda (n)
    (if (= n 1 ) #f
    (esPrimoAux n 2)
)))

(define sumar-lista-primos(lambda (list)
    (cond
    [ (empty? list) 0]
    [ (if ( es_primo (car list))
        (+ (car list) (sumar-lista-primos(cdr list)))
        (+ 0 (sumar-lista-primos(cdr list)))
    )]
    )
))

(define l1 (list 1 2 3 4 5))
(sumar-lista-primos l1)

; recibo un numero y me retorna la lista sin ese numero

(define (remove_el list n)
    (cond
    [ (empty? list) '()]
    [ (= (car list) n ) (remove_el (cdr list) n)]
    [else (cons (car list) (remove_el ( cdr list) n))]
    )
)

(remove_el '(1 2 3 4 5 4 2) 4)

#|
1 2 3 4 5 6, n = 4
1 (remove 2 3 4 5 6)
2 (remove 3 4 5 6)
3 (remove 4 5 6)
(remove 5 6)
5 (remove 6)
6 remove ()
()
6
5 6
3 5 6
2 3 5 6
1 2 3 5 6
|#
