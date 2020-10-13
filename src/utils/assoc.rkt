#lang typed/racket

(provide (all-defined-out))

(: assoc->fun (All (a) (-> (Listof (Pair a a)) (-> a a))))
(define (assoc->fun xs)
  (λ ([k : a])
     (match (assoc k xs)
       [#f k]
       [(cons _ v) v])))

(: zip-fresh (-> (Listof Symbol) (Listof (Pair Symbol Symbol))))
(define (zip-fresh xs)
  (map (λ ([x : Symbol]) (cons x (gensym x))) xs))
