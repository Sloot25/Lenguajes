#lang plai

(require "grammars.rkt")

;; parse :: s-exp -> CFSBAE
(define (parse s-expr)
  (cond
    [(number? s-expr) (num s-expr)]
    [(equal? s-expr 'true ) (bool #t)]
    [(equal? s-expr 'false) (bool #f)]
    [(string? s-expr) (str s-expr)]
    [(symbol? s-expr) (id s-expr)]
    [else (let ([cabeza (car s-expr)])
            (cond
              [(or (equal? cabeza 'sub1) (equal? cabeza 'add1) (equal? cabeza 'sqrt) (equal? cabeza 'not)
                  (equal? cabeza 'zero?) (equal? cabeza 'num? ) (equal? cabeza 'str?) (equal? cabeza 'bool?)
                  (equal? cabeza 'bool?) (equal? cabeza 'str-length) (equal? cabeza 'str-first))
               (if (= (length (rest s-expr)) 1) (aridaduno s-expr) (error "Es una funcion de un solo parametro"))]
              [(or (equal? cabeza 'modulo) (equal? cabeza 'expt)) (if (= (length (rest s-expr)) 2) 
                                                                    (aridaddos s-expr) (error "Es una funcion de dos parametros"))]
              [else (if (= (length (rest s-expr)) 0)
                        (if (list? cabeza) (multiaridad s-expr)
                        (error 'Parse "Necesitas un parametro"))
                        (multiaridad s-expr))]
              ))]))

;; Define el parámetro que deben de tomar las funciones
;; Listado de funciones sub1, add1, sqrt, not, zero?. number?.string, boolean, string-length, string-first, string-last
(define (aridaduno s-expr)
  (let ([cabeza (car s-expr)])
    (match cabeza
      ['sub1 (op sub1 (list (parse (second s-expr))))]
      ['add1 (op add1 (list (parse (second s-expr))))]
      ['sqrt (op sqrt (list (parse (second s-expr))))]
      ['not (op not (list (parse (second s-expr))))]
      ['zero? (op zero? (list(parse (second s-expr))))] ;; Estas declaraciones pueden estar mal 
      ['num? (op number? (list (parse (second s-expr))))]
      ['str? (op string? (list (parse (second s-expr))))]
      ['bool? (op boolean? (list (parse (second s-expr))))]
      ['str-length (op string-length (list (parse (second s-expr))))]
      ['str-first (op string-first (list (parse (second s-expr))))]
      ['str-last (op string-last (list (parse (second s-expr))))]
      )))

;; Regresa el primer caracter de un string 
(define (string-first str)
  (string-ref str 0))

;; Regresa el el resto de la cadena menos el primer caracter 
(define (string-last str)
  (string-ref str (- (string-length str) 1)))

;; Define operaciones de aridad 2
(define (aridaddos s-expr)
  (let ([cabeza (car s-expr)])
    (match cabeza
      ['modulo (op modulo (list (parse (second s-expr)) (parse (third s-expr))))]
      ['expt (op expt (list (parse (second s-expr)) (parse (third s-expr))))])))

;; Revisa si la expresion 'e' se encuentra en algun elemento de la lista 'l'
(define (pertenece l e)
  (cond
    [(empty? l) #f]
    [(equal? e (first l)) #t]
    [else (pertenece (rest l) e)]))

;; Define el funcionamiento de un ormap para los elementos del args
(define (myOr args)
  (ormap (lambda (x) x) args))

;; Define el funcionamiento de un anmap para los elementos del args
(define (myAnd args)
  (andmap (lambda (x) x) args))

;; Revisa que la aridad d ela operaci
(define (multiaridad s-expr)
  (let ([cabeza (car s-expr)])
    (match cabeza
      ['+ (op + (map parse (rest s-expr)))] ;; posible solucion para recibir como lista
      ['- (op - (map parse (rest s-expr)))]
      ['/ (op / (map parse (rest s-expr)))]
      ['* (op * (map parse (rest s-expr)))]
      ['min (op min (map parse (rest s-expr)))]
      ['max (op max (map parse (rest s-expr)))]
      ['and (op myAnd (map parse (rest s-expr)))]
      ['< (op < (map parse (rest s-expr)))]
      ['> (op > (map parse (rest s-expr)))]
      ['<= (op <= (map parse (rest s-expr)))]
      ['>= (op >= (map parse (rest s-expr)))]
      ['= (op = (map parse (rest s-expr)))]
      ['or (op myOr (map parse (rest s-expr)))]
      ['fun (fun (limpFun (first (rest s-expr))) (parse (second (rest s-expr)))) ]
      ['if (iF (parse (first (rest s-expr))) (parse (second (rest s-expr))) (parse (third (rest s-expr))))]
      [else (app (parse cabeza) (myMap (rest s-expr)))]
      )))
(define (limpFun s-expr)
  (if (check-duplicates s-expr) (error "Hay IDs Duplicados")
      s-expr))
(define (myMap s-expr)
  (if (empty? s-expr) '()
      (cons (parse (first s-expr)) (myMap (rest s-expr)))))
