#lang plai

(require "grammars.rkt")

;; Parametros:
;;  - s-expr: number ó symbol ó string ó list
;; Tipo del valor de retorno: WAE
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
              [else (if (= (length (rest s-expr)) 0) (error "Necesitas un parametro")
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

;; Regresa el ultimo caracter de un string
(define (string-last str)
  (string-ref str (- (string-length str) 1)))

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

;; Ayuda a desarollar y definir la variable y el cuerpo del with 
(define (asignarWith s-expr carga)
  (if (empty? s-expr) '()
      (let ([par (first s-expr)])
        (cond
          [(not (= (length par) 2)) (error "Hay una variable libre")]
          [(pertenece carga (first par)) (error "La variable ya ha sido declarada")]
          [else (cons (binding (first par) (parse (second par))) (asignarWith (rest s-expr) (cons (first par) carga)))])))) 

;; Desarolla el multiwith de la s-expr que recibe como argumento.
(define (asignarWithEstrellita s-expr)
  (if (empty? s-expr) '()
      (let ([par (first s-expr)])
        (cond
          [(not (= (length par) 2)) (error "Hay una variable libre")]
          [else (cons (binding (first par) (parse (second par))) (asignarWithEstrellita (rest s-expr) ))]))))

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
      ['with (with
              (asignarWith (second s-expr) '()) (parse (third s-expr)))]
      ['with* (with* (asignarWithEstrellita (second s-expr)) (parse (third s-expr)))]
      )))
