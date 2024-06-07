#lang plai

;; Definicion del binding 
(define-type Binding
  [binding (id symbol?) (value RCFSBAE?)])

;; Definicion de los tipos dentro del lenguaje RCFSBAE
(define-type RCFSBAE
  [num (literal number?)]
  [id (literal symbol?)]
  [bool (literal boolean?)]
  [str (literal string?)]
  [op (operator procedure?) (args (listof RCFSBAE?))]
  [fun (params (listof symbol?)) (body RCFSBAE?)]
  [app (function RCFSBAE?) (args (listof RCFSBAE?))]
  [iF (test-expr RCFSBAE?) (then-expr RCFSBAE?) (else-expr RCFSBAE?)]
  [rec (bindings (listof Binding?)) (body RCFSBAE?)])

;; Definicion de un valor en nuestro lenguaje
(define-type Value
  [numV (literal number?)]
  [boolV (literal boolean?)]
  [strV (literal string?)]
  [closureV (params (listof symbol?)) (body RCFSBAE?) (env Env?)])

;; Definicion de las cajitas que vamos a usar para cuando trabajemos con memoria 
(define (boxed-Value? v)
  (and (box? v) (Value? (unbox v))))

;; Definicion de ambiente
(define-type Env
  [mt-env]
  [cons-env (id symbol?) (value Value?) (rest Env?)]
  [cons-rec-env (id symbol?) (value boxed-Value?) (rest Env?)])
