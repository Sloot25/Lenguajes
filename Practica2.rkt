#lang plai
#|
Practica 2.
Profesora Karla Ramírez Pulido
Ayudante de laboratorio José Eliseo Ortíz Montaño
Alumnos Etni Sarai Castro Sierra, Hugo Osvaldo Paniagua Broca y Diego Jesús Vidal Aguilar 
Grupo: 7092
|#

;; Ejercicio 1
;; Recibe una función y una lista, y devuelve una lista con el resultado de aplicar la función a cada elemento de la lista original.

(define (mapea func ls)
  (if (empty? ls) ls (cons (func (first ls)) (mapea func (rest ls)))))

;; Ejercicio 2
;; Recibe un índice y una lista, y devuelve el elemento de la lista que se encuentre en ese índice.

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
;; Calcula el área de la figura

(define (area figura)
  (cond
  [(cuadrado? figura) (* (cuadrado-lado figura) (cuadrado-lado figura))]
  [(circulo? figura) (* (* (/ (circulo-diametro figura) 2) (/ (circulo-diametro figura) 2)) pi)]
  [else (error "El parametro recibido no es del tipo figura")]))

;; Ejercicio 5
;; Calcula el perímetro de la figura

(define (perimetro figura)
  (cond
  [(cuadrado? figura) (* 4 (cuadrado-lado figura))]
  [(circulo? figura) (* (circulo-diametro figura) pi)]
  [else (error "El parametro recibido no es del tipo figura")]))

;; Ejercicio 6
;; Definicion del arbol de busqueda

(define-type ArbolDeBusqueda
  [vacio]
  [nodo (elem number?) (izq ArbolDeBusqueda?) (der ArbolDeBusqueda?)])

(define (addElem ar el)
 (cond
  [(vacio? ar) (nodo el (vacio) (vacio))]
  [(nodo? ar) (if (< (nodo-elem ar) el) (nodo (nodo-elem ar) (nodo-izq ar) (addElem (nodo-der ar) el))
                  (nodo (addElem (nodo-izq ar) el) (nodo-der ar)))]
  [else (error "El arbol ingresado no es un arbol de busqueda")]))

;; Ejercicio 6a
;; Definición de eliminar en el arbold ebúsqueda

(define (elimina ar el)
  (cond
    [(vacio? ar) vacio]
    [(nodo? ar) (cond
                   [(= (nodo-elem ar) el) (if (vacio? (nodo-izq ar)) (nodo-der ar) (nodo (buscaMaximo (nodo-izq ar)) (eliminaMaximo (nodo-izq ar)) (nodo-der ar)))]
                   [(< (nodo-elem ar) el)  (nodo (nodo-elem ar) (nodo-izq ar) (elimina (nodo-der ar) el))]
                   [else (nodo (nodo-elem ar) (elimina (nodo-izq ar) el) (nodo-der ar))]) ]
    [else (error "No has pasado un arbol como parametro")]))

#|La funcion no recibe arboles vacios, es solo una funcion auxiliar |#

(define (eliminaMaximo ar)
  (cond
    [(vacio? (nodo-der ar)) (nodo-izq ar)]
    [else (nodo (nodo-elem ar) (nodo-izq ar) (eliminaMaximo (nodo-der ar)))]))
(define (buscaMaximo ar)
  (cond
    [(vacio? (nodo-der ar)) (nodo-elem ar)]
    [else (buscaMaximo (nodo-der ar))]))

;; Ejercicio 6b
;; Verifica si está el elemento en el arbol binario

(define (contiene? ar el)
  (cond
    [(vacio? ar) #f]
    [(= (nodo-elem ar) el) #t]
    [(< (nodo-elem ar) el) (contiene? (nodo-der ar) el)]
    [(> (nodo-elem ar) el) (contiene? (nodo-izq ar) el)]
    [else (error "No has ingresado un arbol binario de busqueda")]))

;; Ejercicio 6c
;; devuelve un árbol que contiene todos los elementos del original que cumplen con el predicado.

 (define (filtrar-arbol ar pred)
  (cond
    [(vacio? ar) ar]
    [(nodo? ar) (if (pred (nodo-elem ar))
                    (nodo (nodo-elem ar) (filtrar-arbol (nodo-izq ar) pred) (filtrar-arbol (nodo-der ar) pred))
                    (elimina (nodo (nodo-elem ar) (filtrar-arbol (nodo-izq ar) pred) (filtrar-arbol (nodo-der ar) pred)) (nodo-elem ar)))]
    [else (error "No has ingresado un arbol correctamente")]))
