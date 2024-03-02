#lang plai
#|
Practica 2.
|#

;; Ejercicio 1
(define (mapea func ls)
  (if (empty? ls) ls (cons (func (first ls)) (mapea func (rest ls)))))

;; Ejercicio 2
(define (get-by-index index ls)
  (cond
    [(< index 0) (error "El indice no puede ser menor que 0")]
    [(and (> index -1) (empty? ls)) (error "El indice es mayor o igual al tamaño de la lista")]
    [(= index 0) (car ls)]
    [else (get-by-index (- index 1)(rest ls))]))

;; Ejercicio 3
;; Define el tipo Figura aquí.
(define-type Figura
  [cuadrado (lado number?)]
  [circulo (diametro number?)])

;; Ejercicio 4
(define (area figura)
  (cond
  [(cuadrado? figura) (* (cuadrado-lado figura) (cuadrado-lado figura))]
  [(circulo? figura) (* (* (/ (circulo-diametro figura) 2) (/ (circulo-diametro figura) 2)) pi)]
  [else (error "El parametro recibido no es del tipo figura")]))

;; Ejercicio 5
(define (perimetro figura)
  (cond
  [(cuadrado? figura) (* 4 (cuadrado-lado figura))]
  [(circulo? figura) (* (circulo-diametro figura) pi)]
  [else (error "El parametro recibido no es del tipo figura")]))

;; Ejercicio 6

(define-type ArbolDeBusqueda
  [vacio]
  [nodo (elem number?) (izq ArbolDeBusqueda?) (der ArbolDeBusqueda?)])

;; Ejercicio 6a
(define (elimina ar el)
  (error "Sin implementar"))

;; Ejercicio 6b
(define (contiene? ar el)
  (error "Sin implementar"))

;; Ejercicio 6c
(define (filtrar-arbol ar pred)
  (error "Sin implementar"))
