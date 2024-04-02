#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Parametros:
;;  - sub-id: symbol
;;  - value: WAE
;;  - expr: WAE
;; Tipo del valor de retorno: WAE
(define (substWithEstrellita args cuerpo sub-id value)
  (if (empty? args) '()
      (type-case Binding (first args)
        [binding (with-id with-value) (if (equal? with-id sub-id)
                                          (cons (binding with-id (subst sub-id value with-value)) (rest args))
                                          (cons (binding with-id (subst sub-id value with-value)) (substWithEstrellita (rest args) cuerpo sub-id value)))
                 ])))

;; Encargada de sustituir segun las variables ligadas dentro del with.
;; Recibe la variable de ligado y el valor que va a sustituir
(define (substWith args cuerpo sub-id value)
  (if (empty? args) '()
      (type-case Binding (first args)
        [binding (with-id with-value) (cons (binding with-id (subst sub-id value with-value)) (substWith (rest args) cuerpo sub-id value))])))
            

;; Revisa si un id determinado (parametro de la funcion) esta repetido en la lista
;; Recibe como parametro el id a ver si esta repetido
define (idRepetido args sub-id)
  (if (empty? args) #f
      (type-case Binding (first args)
        [binding (with-id with-value) (if (equal? with-id sub-id) #t (idRepetido (rest args) sub-id))])))


;; Limpia y deja a args sin ningun elemento dentro de el.
(define (cleanMap args)
    (if (empty? args) '()
        (if (WAE? (first args))
            (cons (first args) (cleanMap (rest args)))
            (cons (parse (first args)) (cleanMap (rest args))))))

;; Sustituye segun el id el valor con el "value expr" que se le dotó
(define (subst sub-id value expr)
  (type-case WAE expr
    [id (x) (if (equal? x sub-id) value (id x))]
    [num (x) (num x)]
    [bool (x) (bool x)]
    [str (x) (str x)]
    ;;[op (f args) (op f (map (lambda (ex) (subst sub-id value ex)) args))] ;; Si le pones (parse value) al id se arregla esto, pero se jode el subs
    [op (f args) (op f (cleanMap (map (lambda (ex) (subst sub-id value ex)) args)))]
    [with (args cuerpo) (if (idRepetido args sub-id)
                            (with (substWith args cuerpo sub-id value) cuerpo)
                            (with (substWith args cuerpo sub-id value) (subst sub-id value cuerpo))
                            )]
    [with* (args cuerpo) (if (idRepetido args sub-id)
                            (with* (substWithEstrellita args cuerpo sub-id value) cuerpo)
                            (with* (substWithEstrellita args cuerpo sub-id value) (subst sub-id value cuerpo))
                            )]))

;; Parametros:
;;  - expr: WAE
;; Tipo del valor de retorno: number ó boolean ó string
(define (interp expr)
  (type-case WAE expr
    [id (x) (error  "Variable libre")] ;; Identifica a una variable libre
    [num (x) x]
    [bool (x) x]
    [str (x) x]
    [op (f args) (cond 
                       [(= 1 (length args))
                        (cond
                          [(or (equal? - f) (equal? sqrt f) (equal? f sub1) (equal? f add1) (equal? f not)
                               (equal? f zero?) (equal? f number? ) (equal? f string?) (equal? f boolean?)
                               (equal? f string-length) (equal? f string-first)) (f (interp(first args)))]
                          [else (interp(first args))])]
                       [(equal? f myOr) (f (map interp args))]
                       [(equal? f myAnd) (f (map interp args))]
                       [else
                        (cond
                          [(equal? < f) (< (interp (first args)) (interp (op min (rest args))))]
                          [(equal? <= f) (<= (interp (first args)) (interp (op min (rest args))))]
                          [(equal? > f) (> (interp (first args)) (interp (op max (rest args))))]
                          [(equal? >= f) (>= (interp (first args)) (interp (op max (rest args))))]
                          [(equal? = f) (myEqual args)]
                          [(equal? / f) (myDivision args)]
                          [else (f (interp (first args)) (interp (op f (rest args))))])])]
    [with (args cuerpo) (interp (interpWith args cuerpo))]
    [with* (args cuerpo) (interp (interpWithEstrellita args cuerpo))]))

;; Divide al penultimo elemento del args entre el ultimo y a este resultado lo multiplica por el resto de los elementos del args
(define (myDivision args)
  (if (= (length args) 2) (/ (interp (first args)) (interp (second args)))
      (* (interp (first args)) (myDivision (rest args)))))

;; Revisa si todos los argumentos son iguales
(define (myEqual args)
  (cond
    [(empty? args) #t]
    [(= 1 (length args)) #t]
    [(= (interp (first args)) (interp (second args))) (myEqual (rest args))]
    [else #f]))

;; Hace las sustituciones necesarias dentro el cuerpo del with
(define (interpWith args cuerpo)
  (if (empty? args) cuerpo
      (type-case Binding (first args)
        [binding (with-id with-value) (interpWith (rest args) (subst with-id with-value cuerpo))]
        )))

;; Hace las sustituciones necesarias por el cuerpo del multiwith
(define (interpWithEstrellita args cuerpo)
  (if (empty? args) cuerpo
      (type-case Binding (first args)
        [binding (with-id with-value) (interpWithEstrellita (substWithEstrellita (rest args) cuerpo with-id with-value) (subst with-id with-value cuerpo))])))
