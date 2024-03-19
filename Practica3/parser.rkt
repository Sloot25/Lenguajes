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
    [else (let ([cabeza (car s-expr)])
            (cond
              [(or (equals? cabeza 'sub1) (equals? cabeza 'add1) (equals? cabeza 'sqrt) (equals? cabeza 'not)
                  (equals? cabeza 'zero?) (equals? cabeza 'num? ) (equals? cabeza 'str?) (equals? cabeza 'bool?)
                  (equals? cabeza 'bool?) (equals? cabeza 'str-length) (equals? cabeza 'str-first))
               (if (= (length (rest s-expr)) 1) (aridaduno s-expr) (error "Es una funcion de un solo parametro"))]
              [(or (equals? cabeza 'modulo) (equals? cabeza 'expt)) (if (= (length (rest s-expr))) 2)
                                                                    (aridaddos s-expr) (error "Es una funcion de dos parametros")]
              [else (if (= (length (rest s-expr)) 0) (error "Necesitas un parametro")
                        (multiaridad s-expr))]
              ))]))

(define (aridaduno s-expr)
  (let ([cabeza (car s-expr)])
    (match cabeza
      ['sub1 (op sub1 (parse (second s-expr)))]
      ['add1 (op add1 (parse (second s-expr)))]
      ['sqrt (op sqrt (parse (second s-expr)))]
      ['not (op not (parse (second s-expr)))]
      ['zero? (op zero? (parse (second s-expr)))] ;; Estas declaraciones pueden estar mal 
      ['num? (op number? (parse (second s-expr)))]
      ['str? (op string? (parse (second s-expr)))]
      ['bool? (op bool? (parse (second s-expr)))]
      ['str-length (op string-length (parse (second s-expr)))]
      ['str-first (op string-first (parse (second s-expr)))]
      )))


(define (aridaddos s-expr)
  (let ([cabeza (car s-expr)])
    (match cabeza
      ['modulo (op modulo (parse (second s-expr)) (parse (third s-expr)))]
      ['expt (op expt (parse (second s-expr) (third s-expr)))])))

(define (asignarWith s-expr)
  (cond
    [(null? s-expr) '()]
    [(empty? (second s-expr)) (error "Hay variables libres declaradas")]
    [else (cons ())]))
(define (multiaridad s-expr)
  (let ([cabeza (car s-expr)])
    (match cabeza
      ['+ (op + (list (parse (rest s-expr))))] ;; posible solucion para recibir como lista
      ['- (op - (parse (rest s-expr)))]
      ['/ (op / (parse (rest s-expr)))]
      ['* (op * (parse (rest s-expr)))]
      ['min (op min (parse (rest s-expr)))]
      ['max (op max (parse (rest s-expr)))]
      ['< (op < (parse (rest s-expr)))]
      ['> (op > (parse (rest s-expr)))]
      ['<= (op <= (parse (rest s-expr)))]
      ['>= (op >= (parse (rest s-expr)))]
      ['= (op = (parse (rest s-expr)))]
      ['with ]
      )))