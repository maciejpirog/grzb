#lang typed/racket

(require "core-lang.rkt")
(require "core-logic.rkt")
(require "core-expr.rkt")
(require "error.rkt")

(provide (struct-out pos) Pos Parse-monad parse)

; Position

(struct pos
  ([file : Any]
   [line : Any]
   [col  : Any])
  #:transparent #:type-name Pos)

(: get-pos (-> Syntax Pos))
(define (get-pos s)
  (pos (syntax-source s) (syntax-line s) (syntax-column s)))

; Parser

(define-type (Parse-monad a)
  (Try (User-error Pos) a))

(: ouch! (All (a b) (-> Syntax String * (Parse-monad a))))
(define (ouch! p . xs)
  (raise-error (user-error (get-pos p) xs)))

(: parse-a (-> Syntax (Parse-monad A-expr)))
(define (parse-a s)
  (let ([e (syntax-e s)])
    (cond
      [(exact-integer? e)
       (return (a-const e))]
      [(symbol? e)
       (return (a-var e))]
      [(and (list? e) (pair? e)) ; -- note [1]
       (let* ([ss (syntax->list s)]
              [head (syntax->datum (first ss))])
         (if (and (pair? ss) (a-oper? head))
             (bind (apply combine (map parse-a (rest ss)))
                   (λ ([xs : (Listof A-expr)]) (return (a-op head xs))))
             (ouch! s "Parse error in arithmetic expression (unknown operator)")))]
      [else (ouch! s "Parse error in arithmetic expression")])))

; [1] This part is quite repetitive, but I don't see a way to abstract it out,
;     as the abstraction would have to be parametrised with the predicate that
;     classifies symbols (a-oper? here) and its predicate (which would require
;     some crazy dependency on the classifier, which I don't know how to express
;     in typed racket.

(: parse-b (-> Syntax (Parse-monad B-expr)))
(define (parse-b s)
  (let ([e (syntax-e s)])
    (cond
      [(boolean? e)
       (return (b-const e))]
      [(symbol? e)
       (return (b-var e))]
      [(and (list? e) (pair? e)) ; -- note [1] above
       (let* ([ss (syntax->list s)]
              [head (syntax->datum (first ss))])
         (cond
           [(and (pair? ss) (b-oper? head))
            (bind (apply combine (map parse-b (rest ss)))
                  (λ ([xs : (Listof B-expr)]) (return (b-op head xs))))]
           [(and (pair? ss) (b-cmpr? head))
            (bind (apply combine (map parse-a (rest ss)))
                  (λ ([xs : (Listof A-expr)]) (return (b-cmp head xs))))]
           [else (ouch! s "Parse error in boolean expression (unknown operator)")]))]
      [else (ouch! s "Parse error in boolean expression")])))

(: parse-log (-> Syntax (Parse-monad Log-expr)))
(define (parse-log s)
  (let ([e (syntax-e s)])
    (cond
      [(boolean? e)
       (return (log-const e))]
      [(symbol? e)
       (return (log-var e))]
      [(and (list? e) (pair? e)) ; -- note [1] above
       (let* ([ss (syntax->list s)]
              [head (syntax->datum (first ss))])
         (cond
           [(and (pair? ss) (b-oper? head))
            (bind (apply combine (map parse-log (rest ss)))
                  (λ ([xs : (Listof Log-expr)]) (return (log-op head xs))))]
           [(and (pair? ss) (b-cmpr? head))
            (bind (apply combine (map parse-a (rest ss)))
                  (λ ([xs : (Listof A-expr)]) (return (log-cmp head xs))))]
           [else (ouch! s "Parse error in an assertion formula (unknown operator)")]))]
      [else (ouch! s "Parse error in an assertion formula")])))

(: parse (-> Syntax (Parse-monad (Core Pos))))
(define (parse s)

  (: make (-> (Core-cons Pos) (Core Pos)))
  (define (make c)
    (make-core (get-pos s) c))

  (: make-comp (-> (Listof (Core Pos)) (Core Pos)))
  (define (make-comp xs)
    (cond
      [(null? xs)
       (make (skip))]
      [(and (pair? xs) (null? (rest xs)))
       (first xs)]
      [else
       (make (comp (first xs)
                   (make-comp (rest xs))))]))
  
  (let ([e (syntax-e s)])
    (if (not (and (list? e) (pair? e)))
        (ouch! s "Parse error: Ill-formed command. Did you forget a \"(\"?")
        (let* ([ss (syntax->list s)]
               [head (syntax->datum (first ss))])
          (cond

            [(and (eq? head 'skip) (= (length ss) 1))
             (return (make (skip)))]
            [(eq? head 'skip)
             (ouch! s "Parse error: The correct form is \"(skip)\"")]

            [(and (= (length ss) 3) (eq? (syntax->datum (second ss)) ':=) (symbol? head))
             (bind (parse-a (third ss)) (λ ([e : A-expr])
             (return (make (assign head e)))))]

            [(and (eq? head 'while) (= (length ss) 4))
             (combine3 (parse-log (second ss))
                       (parse-b   (third ss))
                       (parse     (fourth ss))
                       (λ ([inv : Log-expr] [b : B-expr] [c : (Core Pos)])
                         (return (make (while inv b c)))))]
            [(eq? head 'while)
             (ouch! s
               "Parse error: The correct form is \"(while <invariant> <condition> <body>)\"")]

            [(and (eq? head 'assert) (= (length ss) 2))
             (bind (parse-log (second ss)) (λ ([f : Log-expr])
             (return (make (annot f)))))]
            [(eq? head 'assert)
             (ouch! s "Parse error: The correct form is \"(assert <formula>)\"")]

            [(eq? head 'begin)
             (bind (apply combine (map parse (rest ss))) (λ ([xs : (Listof (Core Pos))])
             (return (make-comp xs))))]

            [else (ouch! s "Parse error: ill-formed command")])))))

(define t
  #'(begin
      (x := 8)
      (i := 4)
      (while {> x i} (>= x i)
        (begin
          (i := (- i 1))
          (x := (+ x 1))
          (skip)
          (assert (> x 0))))
      (x := (+ x 1))))