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
  (error "Sin implementar"))

;; Ejercicio 4
(define (primera-letra s)
  (error "Sin implementar"))

;; Ejercicio 5
(define (par? n)
  (error "Sin implementar"))

;; Ejercicio 6
(define (impar? n)
  (error "Sin implementar"))

;; Ejercicio 7
(define (en-rango-o-fuera? x a b)
  (error "Sin implementar"))

;; Ejercicio 8
(define (calculadora operacion n m)
  (error "Sin implementar"))
