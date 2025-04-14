#lang racket

; 1. sumar numeros primos de una lista

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

(sumar-lista-primos '(1 2 3 4 5))

; 2. aplanar lista

(define aplanar_lista(lambda (lista)
    (cond
    [(empty? lista) '()]
    [(list? (car lista)) (append (aplanar_lista (car lista)) (aplanar_lista(cdr lista)))]
    [else (cons (car lista) (aplanar_lista (cdr lista)))]
)))

(aplanar_lista '( 1 2 3 ( 2 3 (4 5)) 1 2 3 ))

; 3. Defina una función que retorne los n últimos elementos de una lista

(define retorn_n_elementos(lambda (n lista)
    (cond
   [(= (length lista) n) lista]
   [else (retorn_n_elementos n (cdr lista))]
)))

(retorn_n_elementos 3 '(1 2 3 4 5))

; 4. Defina una función que devuelva el valor absoluto de todos los elementos de una lista

(define absoluto(lambda (lista)
    (cond
    [(empty? lista) '()]
    [(< (car lista) 0) (cons (* (- 0 1) (car lista)) (absoluto (cdr lista)))] 
    [else (cons (car lista) (absoluto (cdr lista)))]
)))

(absoluto '(1 -2 -3 4 -5 4 2))

;5. recibo un numero y me retorna la lista sin ese numero

(define (remove_el list n)
    (cond
    [ (empty? list) '()]
    [ (= (car list) n ) (remove_el (cdr list) n)]
    [else (cons (car list) (remove_el ( cdr list) n))]
    )
)

(remove_el '(1 2 3 4 5 4 2) 4)