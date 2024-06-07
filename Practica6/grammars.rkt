#lang plai

(define-type Binding
  [binding (id symbol?) (value RCFSBAE?)])

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

(define-type Value
  [numV (literal number?)]
  [boolV (literal boolean?)]
  [strV (literal string?)]
  [closureV (params (listof symbol?)) (body RCFSBAE?) (env Env?)])

(define (boxed-Value? v)
  (and (box? v) (Value? (unbox v))))

(define-type Env
  [mt-env]
  [cons-env (id symbol?) (value Value?) (rest Env?)]
  [cons-rec-env (id symbol?) (value boxed-Value?) (rest Env?)])
