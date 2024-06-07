#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require rebellion/base/converter )


;; Funcion quje nos permite hacer el mapeo
(define (myMapInterp lista env acc)
  (if (empty? lista) acc
  (myMapInterp (rest lista) env (cons (value->primitive (interp (first lista) env)) acc))))

;; RCFSBAE x Env -> RCFSBAE-Val
(define (interp ast env)
   (type-case RCFSBAE ast
    [id (x) (lookup x env)]
    [num (numero) (numV numero)]
    [bool (lit) (boolV lit)]
    [str (s) (strV s)]
    [op (f args) (cond
                   [(= 1 (length args))
                        (cond
                          [(or (equal? - f) (equal? sqrt f) (equal? f sub1) (equal? f add1) (equal? f not)
                               (equal? f zero?) (equal? f number? ) (equal? f string?) (equal? f boolean?)
                               (equal? f string-length) (equal? f string-first)) (primitive->Value(f (value->primitive(interp (first args) env))))]
                          [else (interp (first args) env)])]
                   [(equal? f myOr) (primitive->Value (f (myMapInterp args env '())))]
                   [(equal? f myAnd) (primitive->Value (f (myMapInterp args env '())))]
                   [else
                    (cond
                      [(equal? < f) (primitive->Value (< (value->primitive(interp (first args) env)) (value->primitive(interp (op min (rest args)) env))))]
                      [(equal? <= f) (primitive->Value (<= (value->primitive(interp (first args) env)) (value->primitive(interp (op min (rest args)) env))))]
                      [(equal? > f) (primitive->Value (> (value->primitive (interp (first args) env)) (value->primitive(interp (op max (rest args)) env))))]
                      [(equal? >= f) (primitive->Value (>= (value->primitive (interp (first args) env)) (value->primitive(interp (op max (rest args)) env))))]
                      [(equal? = f) (primitive->Value (myEqual args env))]
                      [(equal? / f) (primitive->Value (myDivision args env))]
                      [else (primitive->Value (f (value->primitive (interp (first args) env)) (value->primitive(interp (op f (rest args)) env))))])])]
    [fun (params body) (closureV params body env)]
    [app (funcion args) (let* ([closure (interp funcion env)]
                              [cuerpoClosure (closureV-body closure)]
                              [paramsClosure (closureV-params closure)]
                              [envClosure (closureV-env closure)])                   
                              (if (empty? args)
                                  (if (empty? paramsClosure)
                                      (interp cuerpoClosure envClosure)
                                      (error 'interp "Argumentos no corresponden a los de la funcion"))
                                  (interp (app (fun (rest paramsClosure) cuerpoClosure) (rest args))
                                                    (cons-env (first paramsClosure ) (interp (first args) env) envClosure))))]
                                  ;;(interp (app (fun (rest paramsClosure) (parse (interp cuerpoClosure
                                    ;;                                                   (cons-env (first paramsClosure) (interp (first args) envClosure) envClosure)))) (rest args)) envClosure)))]
    [iF (test then else) (if (value->primitive (interp test env)) (interp then env) (interp else env))]
    [rec (list body) (let ([nuevoAmbiente (interpBindingsCyclically list env)])
                       (interp body nuevoAmbiente))]
    ))

;; Revisa los Bindings 
(define (interpBindingsCyclically list env)
  (if (empty? list) env
      (type-case Binding (first list)
        [binding (id valor) (if (empty? list) env
                                (interpBindingsCyclically (rest list) (cyclically-bind-and-interp id valor env)))])))

;; Divide al penultimo elemento del args entre el ultimo y a este resultado lo multiplica por el resto de los elementos del args
(define (myDivision args env)
  (if (= (length args) 2) (/ (value->primitive(interp (first args) env)) (value->primitive (interp (second args) env)))
      (* (value->primitive (interp (first args) env)) (myDivision (rest args) env))))

;; Revisa si todos los argumentos son iguales
(define (myEqual args env)
  (cond
    [(empty? args) #t]
    [(= 1 (length args)) #t]
    [(= (value->primitive (interp (first args) env)) (value->primitive (interp (second args) env))) (myEqual (rest args))]
    [else #f]))

;; symbol x Env -> RCFSBAE-Val
;; lookup :: symbol, Env -> Value
(define (lookup search-id env)
  (type-case Env env
    [mt-env () (error 'interp (string-append "Variable libre " (convert-backward string<->symbol search-id)))]
    [cons-env (identificador valor restante) (if (equal? search-id identificador)
                                                  valor
                                                  (lookup search-id restante))]
    [cons-rec-env (id box restEnv) (if (equal? search-id id)
                                   (unbox box)
                                   (lookup search-id restEnv))]))

;; Revisa si cumple las primitivas deseadas
(define (primitive->Value p)
  (cond
    [(number? p) (numV p)]
    [(boolean? p) (boolV p)]))

;; Revisa que sea un valor esperado, si no devuelve error 
(define (value->primitive p)
  (type-case Value p
    [numV (x) x]
    [boolV (x) x]
    [strV (x) x]
    [else (error 'interp "Closure usado como valor")]))

(define (cyclically-bind-and-interp id value env)
  (let* ([contenedor (box (numV 1729))]
        [ambiente (cons-rec-env id contenedor env)]
        [valor (interp value ambiente)])
    (begin
      (set-box! contenedor valor)
      ambiente)))
