#lang typed/racket

(require "expr.rkt")
(require "parse-expr.rkt")
(require "while.rkt")
(require "../logic.rkt")
(require "../utils/error.rkt")
(require "../utils/parse-utils.rkt")

(provide parse-program)

; helpers for pasing call convention ("ref" and "val")

(define-type Call-convention
  (U 'val 'ref))

(define-predicate call-convention? Call-convention)

(: call-convention->arg-expr (-> Call-convention Symbol Arg-expr))
(define (call-convention->arg-expr cc s)
  (match cc
    ['ref (by-ref s)]
    ['val (a-var s)]))

; parsing

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

  (: parse-arg (-> (Syntaxof Any) (Parse-monad Arg-expr)))
  (define (parse-arg a)
    (if (symbol? (syntax-e a))
        (return (by-ref (syntax-e a))) ; default for variable without annotation
        (let ([aa (syntax->list a)])
          (cond
            [(and (list? aa) (= (length aa) 2))
             (let ([cc (syntax->datum (first aa))]
                   [s  (syntax->datum (second aa))])
               (if (and (call-convention? cc) (symbol? s))
                   (return (call-convention->arg-expr cc s))
                   (parse-a a)))]
            [else (parse-a a)]))))

  (let ([ss (syntax->list s)])
    (if (not (and (list? ss) (pair? ss)))
        (ouch! s "Ill-formed statement. Did you forget a \"(\"?")
        (let* ([head (syntax->datum (first ss))])
          (cond

            [(and (eq? head 'skip) (= (length ss) 1))
             (return (make (skip)))]
            [(eq? head 'skip)
             (ouch! s "The correct form is \"(skip)\"")]

            [(and (eq? head 'dummy-po) (= (length ss) 1))
             (return (make (dummy-po)))]
            [(eq? head 'dummy-po)
             (ouch! s "The correct form is \"(dummy-po)\"")]

            [(and (= (length ss) 3) (eq? (syntax->datum (second ss)) ':=) (symbol? head))
             (bind (parse-a (third ss)) (λ ([e : A-expr])
                   (return (make (assign head e)))))]

            [(and (= (length ss) 3) (eq? (syntax->datum (second ss)) ':=)
                  (pair? (syntax-e (first ss)))
                  (syntax? (car (syntax-e (first ss))))
                  (syntax? (cdr (syntax-e (first ss)))))
             (let ([x (syntax->datum (car (syntax-e (first ss))))])
               (if (symbol? x)
                   (combine2 (parse-a (cdr (syntax-e (first ss))))
                             (parse-a (third ss))
                             (λ ([j : A-expr] [e : A-expr])
                                (return (make (store x j e)))))
                   (ouch! s "The correct form is \"((x . expr1) := expr2)\"")))]

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

            [(eq? head 'begin)
             (bind (apply combine (map parse (rest ss))) (λ ([xs : (Listof (Core Pos))])
                   (return (make-comp xs))))]

            [(symbol? head)
             (bind (apply combine (map parse-arg (rest ss))) (λ ([xs : (Listof Arg-expr)])
                   (return (make (proc-call head xs)))))]
            
            [else (ouch! s "Ill-formed statement")])))))

(: parse-garnish (-> (Syntaxof Any) (Parse-monad (Garnish Pos))))
(define (parse-garnish s)

  (: make (-> (Garnish-data Pos) (Garnish Pos)))
  (define (make c)
    (with-meta (get-pos s) c))

  (let ([ss (syntax->list s)])
    (if (not (and (list? ss) (pair? ss)))
        (ouch! s "Ill-formed statement. Did you forget a \"(\"?")
        (let* ([head (syntax->datum (first ss))])
          (cond

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

            [(and (eq? head 'define) (= (length ss) 5))
             (let ([raw-pattern (second ss)]
                   [raw-pre     (third ss)]
                   [raw-post    (fourth ss)]
                   [raw-body    (fifth ss)])
               (combine4
                 (let ([symb-list (syntax->datum raw-pattern)])
                   (if (and (list? symb-list) (pair? symb-list) (andmap symbol? symb-list))
                     (return symb-list)
                     (ouch! s (string-append "Malformed definition head\n"
                                             "The correct form is \"(define (name arg ...) "
                                             "<precondition> <postcondition> <body>\""))))
                 (parse-log raw-pre)
                 (parse-log raw-post)
                 (parse     raw-body)
                 (λ ([head : (Listof Symbol)] [pre : Log-expr]
                     [post : Log-expr] [body : (Core Pos)])
                    (return (make (def (first head)
                                       (rest head)
                                       pre post body))))))]
            [(eq? head 'define)
             (ouch! s (string-append "The correct form is \"(define (name arg ...) "
                                     "<precondition> <postcondition> <body>\""))]

            [else (ouch! s "Ill-formed definition")])))))

(: make-program (All (pos) (-> (Listof (Garnish pos)) (Core pos) (Program pos))))
(define (make-program gs c)

  ; Type checker looses arguments to type constructors in type predicates for
  ; generated predicates, so... :(
  (: filter-axiom (-> (Garnish pos) (Listof (With-meta pos Axiom))))
  (define (filter-axiom a)
    (match a
      [(with-meta m (axiom f))
       (list (with-meta (get-meta a) (axiom f)))]
      [_ null]))
  
  (: filter-check (-> (Garnish pos) (Listof (With-meta pos Check))))
  (define (filter-check a)
    (match a
      [(with-meta m (check f))
       (list (with-meta (get-meta a) (check f)))]
      [_ null]))
  
  (: filter-def (-> (Garnish pos) (Listof (With-meta pos (Def pos)))))
  (define (filter-def a)
    (match a
      [(with-meta m (def n as p pp b))
       (list (with-meta (get-meta a) (def n as p pp b)))]
      [_ null]))
  
  (program (append-map filter-axiom gs)
           (append-map filter-check gs)
           (append-map filter-def   gs)
           c))

(: parse-program (-> (Listof (Syntaxof Any)) (Parse-monad (Program Pos))))
(define (parse-program xs)
  (let-values ([(ys y) (split-at-right xs 1)])
    (if (null? y)
        (error "Impossible: Empty program in parse-program!")
        (combine2 (apply combine (map parse-garnish ys))
                  (parse (car y))
                  (λ ([gs : (Listof (Garnish Pos))] [c : (Core Pos)])
                     (return (make-program gs c)))))))
