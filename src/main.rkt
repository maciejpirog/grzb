#lang typed/racket

(require typed/rackunit
         z3/smt)

(define (foo)
 (with-new-context
  (declare-const x Int/s)
  (declare-const y Int/s)
  (declare-const z Int/s)
  (assert! (>/s x y))
  (assert! (>/s y z))
  (let* ([m (check-sat/model)]
         [c (get-context)])
    (if (symbol? m) "sorry :("
        (model->string c m)))
  )
)

(define (boo)

  (define (k)
    (dynamic-declare-const 'x Int/s)
    (dynamic-declare-const 'y Int/s)
    (dynamic-declare-const 'z Int/s)
  )
  
  (with-new-context
    (k)
    (assert! (>/s 'x 'y))
    (assert! (>/s 'y 'z))
    (let* ([m (check-sat/model)]
           [c (get-context)])
      (if (symbol? m) "sorry :("
          (model->string c m)))
  )
)

(: fff (-> (Syntaxof Any) Syntax))
(define (fff x) x)