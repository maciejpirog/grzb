#lang typed/racket

(require "expr.rkt")
(require "parse-expr.rkt")
(require "while.rkt")
(require "../logic.rkt")
(require "../utils/error.rkt")
(require "../utils/parse-utils.rkt")

(provide parse-program)

(: parse (-> (Syntaxof Any) (Parse-monad (Core Pos))))
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

  (let ([ss (syntax->list s)])
    (if (not (and (list? ss) (pair? ss)))
        (ouch! s "Ill-formed statement. Did you forget a \"(\"?")
        (let* ([head (syntax->datum (first ss))])
          (cond

            [(and (eq? head 'skip) (= (length ss) 1))
             (return (make (skip)))]
            [(eq? head 'skip)
             (ouch! s "The correct form is \"(skip)\"")]

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
               "The correct form is \"(while <invariant> <condition> <statement>)\"")]

            [(and (eq? head 'while*) (= (length ss) 5))
             (combine4 (parse-log (second ss))
                       (parse-a   (third ss))
                       (parse-b   (fourth ss))
                       (parse     (fifth ss))
                       (λ ([inv : Log-expr] [d : A-expr] [b : B-expr] [c : (Core Pos)])
                          (return (make (while* inv d b c)))))]
            [(eq? head 'while*)
             (ouch! s
               "The correct form is \"(while* <invariant> <variant> <condition> <statement>)\"")]

            [(and (eq? head 'if) (= (length ss) 4))
             (combine3 (parse-b (second ss))
                       (parse   (third ss))
                       (parse   (fourth ss))
                       (λ ([b : B-expr] [t : (Core Pos)] [e : (Core Pos)])
                          (return (make (if-stm b t e)))))]
            [(eq? head 'if)
             (ouch! s
               "The correct form is \"(if <condition> <statement> <statement>)\"")]

            [(and (eq? head 'assert) (= (length ss) 2))
             (bind (parse-log (second ss)) (λ ([f : Log-expr])
             (return (make (annot f)))))]
            [(eq? head 'assert)
             (ouch! s "The correct form is \"(assert <formula>)\"")]

            [(and (eq? head 'axiom) (= (length ss) 2))
             (bind (parse-log (second ss)) (λ ([f : Log-expr])
             (return (make (axiom f)))))]
            [(eq? head 'axiom)
             (ouch! s "The correct form is \"(axiom <formula>)\"")]

            [(and (eq? head 'check) (= (length ss) 2))
             (bind (parse-log (second ss)) (λ ([f : Log-expr])
             (return (make (check f)))))]
            [(eq? head 'check)
             (ouch! s "The correct form is \"(check <formula>)\"")]

            [(eq? head 'begin)
             (bind (apply combine (map parse (rest ss))) (λ ([xs : (Listof (Core Pos))])
             (return (make-comp xs))))]

            [(symbol? head)
             (ouch! s "Unrecognized statement \"" (symbol->string head) "\"")]

            [else (ouch! s "Ill-formed statement")])))))

(: parse-def (-> (Syntaxof Any) (Parse-monad (Core Pos))))
(define (parse-def s)

  (: make (-> (Core-cons Pos) (Core Pos)))
  (define (make c)
    (make-core (get-pos s) c))
  
  (let ([ss (syntax->list s)])
    (if (not (and (list? ss) (pair? ss)))
        (ouch! s "Ill-formed definition. Did you forget a \"(\"?")
        (let* ([head (syntax->datum (first ss))])
          (cond

            [(and (eq? head 'axiom) (= (length ss) 2))
             (bind (parse-log (second ss)) (λ ([f : Log-expr])
             (return (make (axiom f)))))]
            [(eq? head 'axiom)
             (ouch! s "The correct form is \"(axiom <formula>)\"")]

            [else (ouch! s "Ill-formed daefinition")])))))

(: parse-program (-> (Listof (Syntaxof Any)) (Parse-monad (Listof (Core Pos)))))
(define (parse-program xs)
  (let-values ([(defs s) (split-at-right xs 1)])
    (apply combine (append (map parse-def defs)
                           (map parse s)))))

