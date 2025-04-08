#lang racket

; funcion que vea si es palindromo --> invertir lista y ver si son iguales

(define (reversar lista)
  (if (empty? lista)
      '()
      (append (reversar (cdr lista)) (list (car lista)))
    )
)

(define (palindromo l)
    (if (equal? l (reversar l))
    #t
    #f
))

;(define list1 (list 1 2 3 2 1))
;(displayln (palindromo list1))


; insercion intercalada

#|
(define (intercalar lista elemento)
    (if (empty? lista)
      '()
      (append (list (car lista) elemento)  (intercalar (cdr lista) elemento)
)))
|#


(define (intercalar lista elemento)
    (cond
        [ (empty? lista) '()]
        [ (equal? (length lista) 1) (car lista)]
        [
            else (cons (car lista) (cons elemento (intercalar (cdr lista) elemento))

        )]
    )
)

(intercalar (list 1 3) 0)
