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

(displayln "--------------------------")
(sumar-lista-primos '(1 2 3 4 5))

; 2. aplanar lista

(define aplanar_lista(lambda (lista)
    (cond
    [(empty? lista) '()]
    [(list? (car lista)) (append (aplanar_lista (car lista)) (aplanar_lista(cdr lista)))]
    [else (cons (car lista) (aplanar_lista (cdr lista)))]
)))

(displayln "--------------------------")
(aplanar_lista '( 1 2 3 ( 2 3 (4 5)) 1 2 3 ))

; 3. Defina una función que retorne los n últimos elementos de una lista

(define retorn_n_elementos(lambda (n lista)
    (cond
   [(= (length lista) n) lista]
   [else (retorn_n_elementos n (cdr lista))]
)))

(displayln "--------------------------")
(retorn_n_elementos 3 '(1 2 3 4 5))

; 4. Defina una función que devuelva el valor absoluto de todos los elementos de una lista

(define absoluto(lambda (lista)
    (cond
    [(empty? lista) '()]
    [(< (car lista) 0) (cons (* (- 0 1) (car lista)) (absoluto (cdr lista)))] 
    [else (cons (car lista) (absoluto (cdr lista)))]
)))

(displayln "--------------------------")
(absoluto '(1 -2 -3 4 -5 4 2))

; 5. recibo un numero y me retorna la lista sin ese numero

(define (remove_el list n)
    (cond
    [ (empty? list) '()]
    [ (= (car list) n ) (remove_el (cdr list) n)]
    [else (cons (car list) (remove_el ( cdr list) n))]
    )
)

(displayln "--------------------------")
(remove_el '(1 2 3 4 5 4 2) 4)

; 6. Definir la función "cuadrante" que dado dos valores (x y) retorna en que cuadrante de un plano cartesiano pertenece, si se encuentra en un eje tiene que retornar 0

(define cuadrante(lambda (x y)
    (cond
    [(or (= x 0) (= y 0)) 0]
    [(and (> x 0) (> y 0)) 1]
    [(and (< x 0) (> y 0)) 2]
    [(and (< x 0) (< y 0)) 3]
    [else 4]
)))

(displayln "--------------------------")
(cuadrante 1 -2)

; 7.  Definir la función "multi" que toma 3 parámetros, una función anónima, una lista, otra lista, tiene que retornar el cuadrado de las mult de los elementos de la lista 

(define multi(lambda (l1 l2)
    (if (and (empty? l1) (empty? l2)) '()
    (cons (expt (* (car l1) (car l2)) 2) (multi (cdr l1) (cdr l2)))
)))
(displayln "--------------------------")
(multi '(1 2 3) '(2 3 4))

; 8. Definir la función "full-comparar" que dada dos listas devuelve verdadero si es que
;contienen los mismos elementos en el mismo orden (ignorando paréntesis) y retornar falso en otro caso

(define full_comparar(lambda (l1 l2)
    (cond
    [(and (empty? l1) (empty? l2)) #t]
    [(and (list? (car l1)) (not (list? (car l2)))) (if (not (= (car (car l1)) (car l2))) #f  (full_comparar (append (cdr (car l1)) (cdr l1) ) (cdr l2)))]
    [(and (list? (car l2)) (not (list? (car l1)))) (if (not (= (car (car l2)) (car l1))) #f  (full_comparar (append (cdr (car l2)) (cdr l2) ) (cdr l1)))]
    [(and (list? (car l1)) (list? (car l2))) (if (not (= (car (car l1)) (car (car l2)))) #f (full_comparar (append (cdr (car l1)) (cdr l1)) (append (cdr (car l2)) (cdr l2)) ))]
    [(not(= (car l1) (car l2))) #f ] 
    [else (full_comparar (cdr l1) (cdr l2))]
)))

(displayln "--------------------------")
(full_comparar '(4 (5 6)) '((4 5) 6))
(full_comparar '(3 2 1) '(1 (3 2)))


; 9. Definir la función "mediana" que devuelve la mediana de una lista de números ordenados

(define size_lista (lambda (l)
    (if (empty? l) 
    0
    (+ 1 (size_lista (cdr l)))
)))

(define mediana_impar (lambda (l size) 
    (if (= (size_lista l) (+ 0.5 (/ size 2))) 
    (car l)
    (mediana_impar (cdr l) size) 
)))

(define mediana_par (lambda (l size) 
    (if (= (size_lista l) (+ 1 (/ size 2))) 
    (/ ( + (car l) (car (cdr l))) 2)
    (mediana_par (cdr l) size) 
)))

(define mediana (lambda (l)
    (if (= (modulo (size_lista l) 2) 0) 
    (mediana_par l (size_lista l))
    (mediana_impar l (size_lista l)) 
)))

(displayln "--------------------------")
(mediana '(1 2 3))
(mediana '(1 2 3 4))

; 10. discriminante de una función cuadrática

(define discriminante (lambda (a b c) sqrt ( - (* b b ) (* 4 a c))))

(displayln "--------------------------")
(discriminante 1 2 3)

