#lang plai
#|
Practica 1.
|#

;; Ejercicio 1
(define (area-total generatriz diametro)
  (+ ( * pi generatriz (/ diametro 2)) (* pi (* (/ diametro 2) (/ diametro 2)))))
; podriamos usar una variable para el radio dado que se repite 3 veces en la funcion 
;; Ejercicio 2
(define (decremental? a b c d)
  (if (and (> a b) (> b c) (> c d)) #t #f)
  )
;; Ejercicio 3
(define (area-heron a b c)
  (let ([y (/ (+ a b c) 2)]) (sqrt (* (* y (- y a)) (* (- y b) (- y c))))))
  
;; Ejercicio 4
(define (primera-letra s)
  (if(char-alphabetic? (obtener-letra s))
     (if(vocal?(obtener-letra s))"Vocal" "Consonante")
     "Ingrese una cadena válida"))

(define (vocal? letter)
  (member letter '(#\a #\e #\i #\o #\u)))

(define (obtener-letra str) (string-ref str 0))

;; Ejercicio 5
(define (par? n)
  (if (even? n)#t #f))

;; Ejercicio 6
(define (impar? n)
  (if (even? n)#f #t))

;; Ejercicio 7
(define (en-rango-o-fuera? x a b)
  (cond
    [(and (mayorIgual x a) (mayorIgual b x)) #t]
    [(or (< x (- a 10)) (> x (+ a 10))) #t]
    [else #f]
    ))

(define (mayorIgual a b)
  (if (or (= a b) (> a b)) #t #f))

;; Ejercicio 8
(define (calculadora operacion n m)
  (match operacion
    ["first" n]
    ["second" m]
    ["sum" (+ n m)]
    ["mul" (* n m)]
    ["div-exact" (quotient n m)]
    ["div" (/ n m)]))
