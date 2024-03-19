#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./interp.rkt"))

(define (prueba e)
  (interp (parse e)))

(test (parse 'x) (id 'x))
(test (parse 10) (num 10))
(test (parse 'true) (bool #t))
(test (parse 'false) (bool #f))
(test (parse "Hola") (str "Hola"))

(test (parse '{sub1 1}) (op sub1 (list (num 1))))
(test/exn (parse '{sub1 1 2}) "")

(test (parse '{num? 1}) (op number? (list (num 1))))
(test/exn (parse '{num? 2 1}) "")

(test (parse '{modulo 1 2}) (op modulo (list (num 1) (num 2))))
(test/exn (parse '{modulo 1 2 3}) "")

(test (parse '{min 1}) (op min (list (num 1))))
(test/exn (parse '{min}) "")

(test (parse '{with {{x 10} {y 20}} x})
      (with
       (list (binding 'x (num 10)) (binding 'y (num 20)))
       (id 'x)))
(test/exn (parse '{with {{x 10} {x 20}} x}) "")

(test (parse '{with* {{x 10} {y 20}} x})
      (with*
       (list (binding 'x (num 10)) (binding 'y (num 20)))
       (id 'x)))

(test (parse '{with* {{x 10} {x 20}} x})
      (with*
       (list (binding 'x (num 10)) (binding 'x (num 20)))
       (id 'x)))

(test (subst 'x (num 1) (num 20)) (num 20))
(test (subst 'x (num 1) (bool #t)) (bool #t))
(test (subst 'x (num 1) (str "Hola")) (str "Hola"))
(test (subst 'x (num 1) (id 'y)) (id 'y))
(test (subst 'x (num 1) (id 'x)) (num 1))

(test (subst 'x (num 1) (parse '{+ x x x}))
      (parse '{+ 1 1 1}))

(test (subst 'x (num 1) (parse '{with {{a 1} {x x} {b x}} x}))
      (parse '{with {{a 1} {x 1} {b 1}} x}))
(test (subst 'x (num 1) (parse '{with {{a 1} {b 1}} x}))
      (parse '{with {{a 1} {b 1}} 1}))

(test (subst 'x (num 1) (parse '{with* {{a 1} {x x} {b x}} x}))
      (parse '{with* {{a 1} {x 1} {b x}} x}))
(test (subst 'x (num 1) (parse '{with* {{a 1} {b 1}} x}))
      (parse '{with* {{a 1} {b 1}} 1}))

(test/exn (prueba 'foo) "")
(test (prueba 1234) 1234)
(test (prueba 'true) #t)
(test (prueba 'false) #f)
(test (prueba '"Hi") "Hi")
(test (prueba '{+ 1 2 3}) 6)
(test (prueba '{- 1 2 3}) -4)
(test (prueba '{* 1 2 -3}) -6)
(test (prueba '{* 1 2 -3 0}) 0)
(test (prueba '{/ 1 2}) (/ 1 2))
(test (prueba '{min 20 90 -1 0}) -1)
(test (prueba '{max 20 90 -1 0}) 90)
(test (prueba '{sqrt 20}) 4.47213595499958)
(test (prueba '{modulo 3 2}) 1)
(test (prueba '{sub1 3}) 2)
(test (prueba '{sub1 (add1 (expt 3 3))}) 27)
(test (prueba '{str-length "Hello"}) 5)
(test (prueba '{> 10 9}) #t)
(test (prueba '{> 1 2}) #f)
(test (prueba '{> 10 9 8 7 6 5 4 3 2 1}) #t)
(test (prueba '{> 1 2 3 4 5 6 7 8}) #f)
(test (prueba '{= 1 2 3 4 5 6 7 8}) #f)
(test (prueba '{= 10 10}) #t)
(test (prueba '{zero? 10}) #f)
(test (prueba '{zero? 0}) #t)
(test (prueba '{num? 10}) #t)
(test (prueba '{bool? 10}) #f)
(test (prueba '{bool? {and {zero? {add1 1}}
                           {num? "Hello"}}}) #t)
(test (prueba '{or {zero? {+ -1 1}}
                   {num? {sub1 0}}
                   {bool? {str? "Hi"}}}) #t)
(test/exn (prueba 'a) "")
(test (prueba '{with {[x 2] [y 3]} {+ x 3 y}}) 8)
(test (prueba '{with {{x 5} {y 1}} {+ x y}}) 6)
(test (prueba '{with* {{x 5} {y 1}} {+ x y}}) 6)
(test (prueba '{with* {{x 5} {y {+ x 1}}} {+ x y}}) 11)
(test/exn (prueba'{with {{a 2} {b {+ a a}}} b}) "")
(test (prueba'{with* {{a 2} {b {+ a a}}} b}) 4)
(test/exn (prueba'{with {{y 1} {x y}} x}) "")
(test (prueba'{with* {{x y} {y 1}} x}) 1)
(test (prueba '{with {{x 5}}
                     {with {{x 2}}
                           {+ x x}}}) 4)
(test (prueba '{with ([x 2] [y 1] [z 3]) (+ x y z)}) 6)
(test (prueba'{with ([x 2] [y 1] [z 3]) (/ x y z)}) 2/3)

(test (prueba '{with ([x 1]
                      [y 1])
                     {with ([x 5]
                            [y x])
                           (+ x y)}}) 6)
(test (prueba '{with ([x 1]
                      [y 1])
                     {with* ([x 5]
                             [y x])
                            (+ x y)}}) 10)

(test/exn (prueba '{with ([x 1]
                          [x 2])
                         {+ x x}})
          "")

(test (prueba '{with {{x 10}}
                     {with* {{y x} {z {+ x x}} {x 0} {a {add1 x}}}
                            {+ x {with {{b a}}
                                       a}}}}) 1)
