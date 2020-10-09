#lang typed/racket

(require "expr.rkt")
(require "../utils/error.rkt")
(require "../utils/parse-utils.rkt")

(provide parse-a parse-b)

(: parse-a (-> (Syntaxof Any) (Parse-monad A-expr)))
(define (parse-a s)
  (let ([e (syntax-e s)])
    (cond
      [(exact-integer? e)
       (return (a-const e))]
      [(symbol? e)
       (return (a-var e))]
      [else
       (let ([ss (syntax->list s)])
         (if (not (and (list? ss) (pair? ss)))
             (ouch! s "Ill-formed arithmetic expression")
             (let ([head (syntax->datum (first ss))])
               (cond
                 [(a-oper? head)
                  (bind (apply combine (map parse-a (rest ss)))
                        (λ ([xs : (Listof A-expr)]) (return (a-op head xs))))]
                 [(pair? (cdr ss))
                  (let ([sec (syntax->datum (second ss))])
                    (if (a-oper? sec)
                        (ouch! s "Illegal syntax: infix operator \""
                                 (symbol->string sec) "\"?")
                        (ouch! s "Arithmetic expression: unknown operator")))]
                 [else
                  (ouch! s "Arithmetic expression: unknown operator")]))))])))

(: parse-b (-> (Syntaxof Any) (Parse-monad B-expr)))
(define (parse-b s)
  (let ([e (syntax-e s)])
    (cond
      [(boolean? e)
       (return (b-const e))]
      [(eq? e 'true)
       (return (b-const #t))]
      [(eq? e 'false)
       (return (b-const #f))]
      [(symbol? e)
       (return (b-var e))]
      [else
       (let ([ss (syntax->list s)])
         (if (not (and (list? ss) (pair? ss)))
             (ouch! s "Ill-formed boolean expression \"()\"")
             (let ([head (syntax->datum (first ss))])
               (cond
                 [(b-oper? head)
                  (bind (apply combine (map parse-b (rest ss)))
                        (λ ([xs : (Listof B-expr)]) (return (b-op head xs))))]
                 [(b-cmpr? head)
                  (bind (apply combine (map parse-a (rest ss)))
                        (λ ([xs : (Listof A-expr)]) (return (b-cmp head xs))))]
                 [(pair? (cdr ss))
                  (let ([sec (syntax->datum (second ss))])
                    (if (or (b-oper? sec) (b-cmpr? sec))
                        (ouch! s "Illegal syntax: infix operator \""
                                 (symbol->string sec) "\"?")
                        (ouch! s "Boolean expression: unknown operator")))]
                 [else
                  (ouch! s "Boolean expression: unknown operator")]))))])))
