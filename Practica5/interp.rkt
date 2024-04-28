#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
;; No supe como escribir el map
(define (myMapInterp lista env acc)
  (if (empty? lista) acc
  (myMapInterp (rest lista) env (cons (value->primitive (interp (first lista) env)) acc))))

;; interp :: CFSBAE, Env -> Value
(define (interp ast env)
   (type-case CFSBAE ast
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
                            
    [iF (test then else) (if (value->primitive (interp test env)) (interp then env) (interp else env))]
    ))
;; Divide al penultimo elemento del args entre el ultimo y a este resultado lo multiplica por el resto de los elementos del args
(define (myDivision args env)
  (if (= (length args) 2) (/ (value->primitive(interp (first args) env)) (value->primitive (interp (second args) env)))
      (* (value->primitive (interp (first args) env)) (myDivision (rest args)))))

;; Revisa si todos los argumentos son iguales
(define (myEqual args env)
  (cond
    [(empty? args) #t]
    [(= 1 (length args)) #t]
    [(= (value->primitive (interp (first args) env)) (value->primitive (interp (second args) env))) (myEqual (rest args))]
    [else #f]))
;; lookup :: symbol, Env -> Value
(define (lookup search-id env)
  (type-case Env env
    [mt-env () (error 'lookUp "Id no encontrado")]
    [cons-env (identificador valor restante) (if (equal? search-id identificador)
                                                  valor
                                                  (lookup search-id restante))]))

(define (primitive->Value p)
  (cond
    [(number? p) (numV p)]
    [(boolean? p) (boolV p)]))
(define (value->primitive p)
  (type-case Value p
    [numV (x) x]
    [boolV (x) x]
    [strV (x) x]
    [else (error 'interp "Closure usado como valor")]))