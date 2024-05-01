#lang plai

;; Definicion del lenguaje CFSBAE
(define-type CFSBAE
  [num (literal number?)]
  [id (literal symbol?)]
  [bool (literal boolean?)]
  [str (literal string?)]
  [op (operator procedure?) (args (listof CFSBAE?))]
  [fun (params (listof symbol?)) (body CFSBAE?)]
  [app (function CFSBAE?) (args (listof CFSBAE?))]
  [iF (test-expr CFSBAE?) (then-expr CFSBAE?) (else-expr CFSBAE?)])

;; Definicion de un valor en nuestro lenguaje dependiendo de la literal
(define-type Value
  [numV (literal number?)]
  [boolV (literal boolean?)]
  [strV (literal string?)]
  [closureV (params (listof symbol?)) (body CFSBAE?) (env Env?)])

;; Definicion de ambiente
(define-type Env
  [mt-env]
  [cons-env (id symbol?) (value Value?) (rest Env?)])
