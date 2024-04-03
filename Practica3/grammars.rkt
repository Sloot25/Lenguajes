#lang plai

(define-type Binding
  [binding (id symbol?) (value WAE?)])

;; Definicion de una expresion del tipo WAE
(define-type WAE
  [id (literal symbol?)]
  [num (literal number?)]
  [bool (literal boolean?)]
  [str (literal string?)]
  [op (operator procedure?) (args (listof WAE?))]
  [with (assigns (listof Binding?)) (body WAE?)]
  [with* (assigns (listof Binding?)) (body WAE?)])
