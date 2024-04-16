#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; desugar :: CFSBWAE -> CFSBAE
(define (desugar expr)
  (type-case CFSBWAE expr
    [numS (x) (num x)]
    [idS (x) (id x)]
    [boolS (x) (bool x)]
    [strS (x) (str x)]
    [opS (operator args) (op operator (map desugar args))]
    [withS (bindings body) (app (fun (desugarWith bindings) (desugar body)) (map desugar (getArgsWith bindings)) ) ]
    [with*S (bindings body) (if (= (length bindings) 1) (desugar (withS bindings body))
                                (desugar (withS (list (first bindings)) (with*S (rest bindings) body))))]
    [funS (params body) (fun params (desugar body))]
    [appS (funs args) (app (desugar funs) (map desugar args))]
    [iFS (test then else) (iF (desugar test) (desugar then) (desugar else))]
    [conDS (conds else) (type-case Condition (first conds)
                          [condition (test then)
                                     (if (= 1 (length conds)) (iF (desugar test) (desugar then) (desugar else))
                                         (iF (desugar test) (desugar then) (desugar (conDS (rest conds) else))))])]))


(define (desugarWith bindings)
  (if (empty? bindings) '()
      (let ([bi (first bindings)])
        (type-case Binding bi
          [binding (sym value) (cons sym (desugarWith (rest bindings)))]))))


(define (getArgsWith bindings)
  (if (empty? bindings) '()
      (let ([bi (first bindings)])
        (type-case Binding bi
          [binding (sym value) (cons value (getArgsWith (rest bindings)))]))))