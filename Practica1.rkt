#lang plai
#|
Practica 1.
|#

;; Ejercicio 1
(define (area-total generatriz diametro)
  (error "Sin implementar"))

;; Ejercicio 2
(define (decremental? a b c d)
  (error "Sin implementar"))

;; Ejercicio 3
(define (area-heron a b c)
  (error "Sin implementar"))

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
  (error "Sin implementar"))

;; Ejercicio 8
(define (calculadora operacion n m)
  (error "Sin implementar"))
