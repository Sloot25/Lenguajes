#lang plai

(require "grammars.rkt")

;; parse :: s-exp -> CFSBWAE
;; Parametros:
;;  - s-expr: number ó symbol ó string ó list
;; Tipo del valor de retorno: WAE
(define (parse s-expr)
  (cond
    [(number? s-expr) (numS s-expr)]
    [(equal? s-expr 'true ) (boolS #t)]
    [(equal? s-expr 'false) (boolS #f)]
    [(string? s-expr) (strS s-expr)]
    [(symbol? s-expr) (idS s-expr)]
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
      ['sub1 (opS sub1 (list (parse (second s-expr))))]
      ['add1 (opS add1 (list (parse (second s-expr))))]
      ['sqrt (opS sqrt (list (parse (second s-expr))))]
      ['not (opS not (list (parse (second s-expr))))]
      ['zero? (opS zero? (list(parse (second s-expr))))] ;; Estas declaraciones pueden estar mal 
      ['num? (opS number? (list (parse (second s-expr))))]
      ['str? (opS string? (list (parse (second s-expr))))]
      ['bool? (opS boolean? (list (parse (second s-expr))))]
      ['str-length (opS string-length (list (parse (second s-expr))))]
      ['str-first (opS string-first (list (parse (second s-expr))))]
      ['str-last (opS string-last (list (parse (second s-expr))))]
      )))

;; Regresa el primer caracter de un string 
(define (string-first str)
  (string-ref str 0))

;; Regresa el ultimo caracter de un string
(define (string-last str)
  (string-ref str (- (string-length str) 1)))

;; Definición necesaria para cuando tenemos aridad 2.
(define (aridaddos s-expr)
  (let ([cabeza (car s-expr)])
    (match cabeza
      ['modulo (opS modulo (list (parse (second s-expr)) (parse (third s-expr))))]
      ['expt (opS expt (list (parse (second s-expr)) (parse (third s-expr))))])))

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
      ['+ (opS + (map parse (rest s-expr)))] ;; posible solucion para recibir como lista
      ['- (opS - (map parse (rest s-expr)))]
      ['/ (opS / (map parse (rest s-expr)))]
      ['* (opS * (map parse (rest s-expr)))]
      ['min (opS min (map parse (rest s-expr)))]
      ['max (opS max (map parse (rest s-expr)))]
      ['and (opS myAnd (map parse (rest s-expr)))]
      ['< (opS < (map parse (rest s-expr)))]
      ['> (opS > (map parse (rest s-expr)))]
      ['<= (opS <= (map parse (rest s-expr)))]
      ['>= (opS >= (map parse (rest s-expr)))]
      ['= (opS = (map parse (rest s-expr)))]
      ['or (opS myOr (map parse (rest s-expr)))]
      ['with (withS
              (asignarWith (second s-expr) '()) (parse (third s-expr)))]
      ['with* (with*S (asignarWithEstrellita (second s-expr)) (parse (third s-expr)))]
      ['fun (funS (limpFun (first (rest s-expr))) (parse (second (rest s-expr)))) ]
      ['if (iFS (parse (first (rest s-expr))) (parse (second (rest s-expr))) (parse (third (rest s-expr))))]
      ['cond (conDS (conditionParser (rest s-expr)) (getElse (rest s-expr)))]
      [else (appS (parse cabeza) (myMap (rest s-expr)))]
      )))

;;Revisa y limpia los id's duplicados en nuestra s-expresión.
(define (limpFun s-expr)
  (if (check-duplicates s-expr) (error "Hay IDs Duplicados")
      s-expr))

;; Revisa si la expresión es vacia para saber si hace el parseo con la cabeza o solamente opera con '() 
(define (myMap s-expr)
  (if (empty? s-expr) '()
      (cons (parse (first s-expr)) (myMap (rest s-expr)))))

(define (conditionParser s-expr)
  (if (equal? 'else (first (first s-expr))) '()
      (let ([cabeza (first s-expr)])
        (cons (condition (parse (first cabeza)) (parse (second cabeza))) (conditionParser (rest s-expr))))))
;; Buscador de la sentencia de control "Else"
(define (getElse s-expr)
  (if (equal? 'else (first (first s-expr))) (parse (second (first s-expr)))
      (getElse (rest s-expr))))
