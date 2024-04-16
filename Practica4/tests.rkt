#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

(test (parse 'x) (idS 'x))
(test (parse 10) (numS 10))
(test (parse 'true) (boolS #t))
(test (parse 'false) (boolS #f))
(test (parse "Hola") (strS "Hola"))

(test (parse '{sub1 1}) (opS sub1 (list (numS 1))))
(test/exn (parse '{sub1 1 2}) "")

(test (parse '{num? 1}) (opS number? (list (numS 1))))
(test/exn (parse '{num? 2 1}) "")

(test (parse '{modulo 1 2}) (opS modulo (list (numS 1) (numS 2))))
(test/exn (parse '{modulo 1 2 3}) "")

(test (parse '{min 1}) (opS min (list (numS 1))))
(test/exn (parse '{min}) "")

(test (parse '{with {{x 10} {y 20}} x})
      (withS
       (list (binding 'x (numS 10)) (binding 'y (numS 20)))
       (idS 'x)))
(test/exn (parse '{with {{x 10} {x 20}} x}) "")

(test (parse '{with* {{x 10} {y 20}} x})
      (with*S
       (list (binding 'x (numS 10)) (binding 'y (numS 20)))
       (idS 'x)))

(test (parse '{with* {{x 10} {x 20}} x})
      (with*S
       (list (binding 'x (numS 10)) (binding 'x (numS 20)))
       (idS 'x)))

(test (parse '{cond {{zero? 0} 10} {{> 1 0} 1} {else 0}})
      (conDS (list (condition (opS zero? (list (numS 0)))
                              (numS 10))
                   (condition (opS > (list (numS 1) (numS 0)))
                              (numS 1)))
             (numS 0)))

(test (parse '{if {zero? 10} {fun {x} {* x x}} 0})
      (iFS (opS zero? (list (numS 10)))
           (funS (list 'x) (opS * (list (idS 'x) (idS 'x))))
           (numS 0)))

(test (parse '{{fun {x} {zero? x}} {+ 10 1}})
      (appS (funS (list 'x) (opS zero? (list (idS 'x))))
            (list (opS + (list (numS 10) (numS 1))))))

(test (parse '{if {{fun {} true}} 0 1})
      (iFS (appS (funS '() (boolS #t)) '())
           (numS 0)
           (numS 1)))

(test/exn (parse '{fun {x x} x}) "")

(test (desugar (numS 0)) (num 0))
(test (desugar (idS 'x)) (id 'x))
(test (desugar (boolS #t)) (bool #t))
(test (desugar (boolS #f)) (bool #f))
(test (desugar (strS "Hola")) (str "Hola"))

(test (desugar (parse '{sub1 1})) (op sub1 (list (num 1))))

(test (desugar (parse '{num? 1})) (op number? (list (num 1))))

(test (desugar (parse '{modulo 1 2})) (op modulo (list (num 1) (num 2))))

(test (desugar (parse '{min 1})) (op min (list (num 1))))

(test (desugar (parse '{str-length "Hi!"}))
      (op string-length (list (str "Hi!"))))

(test (desugar (parse '{with {{x 10} {y 10}} {+ x y}}))
      (app (fun (list 'x 'y) (op + (list (id 'x) (id 'y))))
           (list (num 10) (num 10))))

(test (desugar (parse '{with* {{x 10} {y x} {x y}}
                              {+ x y}}))
      (app (fun (list 'x)
                (app (fun (list 'y)
                          (app (fun (list 'x)
                                    (op + (list (id 'x) (id 'y))))
                               (list (id 'y))))
                     (list (id 'x))))
           (list (num 10))))

(test (desugar (parse '{cond {true 10} {false 1} {else 0}}))
      (iF (bool #t)
          (num 10)
          (iF (bool #f)
              (num 1)
              (num 0))))

