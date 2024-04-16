#lang plai

(define-type Binding
  [binding (id symbol?) (value CFSBWAE?)])

(define-type Condition
  [condition  (test-expr CFSBWAE?) (then-expr CFSBWAE?)])

;; Lenguaje con azucar sintactica
(define-type CFSBWAE
  [numS (literal number?)]
  [idS (literal symbol?)]
  [boolS (literal boolean?)]
  [strS (literal string?)]
  [opS (operator procedure?) (args (listof CFSBWAE?))]
  [withS (bindings (listof Binding?)) (body CFSBWAE?)]
  [with*S (bindings (listof Binding?)) (body CFSBWAE?)]
  [funS (params (listof symbol?)) (body CFSBWAE?)]
  [appS (function CFSBWAE?) (args (listof CFSBWAE?))]
  [iFS (test-expr CFSBWAE?) (then-expr CFSBWAE?) (else-expr CFSBWAE?)]
  [conDS (conds (listof Condition?)) (else-expr CFSBWAE?)])

(define-type CFSBAE
  [num (literal number?)]
  [id (literal symbol?)]
  [bool (literal boolean?)]
  [str (literal string?)]
  [op (operator procedure?) (args (listof CFSBAE?))]
  [fun (params (listof symbol?)) (body CFSBAE?)]
  [app (function CFSBAE?) (args (listof CFSBAE?))]
  [iF (test-expr CFSBAE?) (then-expr CFSBAE?) (else-expr CFSBAE?)])