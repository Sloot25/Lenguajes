#lang plai

(define-type Binding
  [binding (id symbol?) (value WAE?)])

(define-type WAE
  [id (literal symbol?)]
  [num (literal number?)]
  [bool (lieteral boolean?)]
  [str (literal string?)]
  [op (operator procedure?) (args (listof WAE?))]
  [with (assigns (listof Binding?)) (body WAE?)]
  [with* (assigns (listof Binding?)) (body WAE?)])
