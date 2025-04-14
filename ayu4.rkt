#lang racket

; Escribir una función la cual retorne otra lista con v o f de acuerdo a si son primos o no 

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

(define verificar_primos(lambda (list)
    (cond
    [ (empty? list) '()]
    [ (if ( es_primo (car list))
        (cons #t (verificar_primos(cdr list)))
        (cons #f (verificar_primos(cdr list)))
    )]
    )
))

(verificar_primos '(2 4 3 5 8 13))

; 2. aplanar lista

(define aplanar_lista(lambda (lista)
    (cond
    [(empty? lista) '()]
    [(list? (car lista)) (append (aplanar_lista (car lista)) (aplanar_lista(cdr lista)))]
    [else (cons (car lista) (aplanar_lista (cdr lista)))]
)))

(aplanar_lista '( 1 2 3 ( 2 3 (4 5)) 1 2 3 ))

#|
( 1 2 3 ( 2 3 (4 5)) 1 2 3 )
1 + aplanar lista cdr
   2 + aplanar lista cdr
       3 + al cdr
            
|#