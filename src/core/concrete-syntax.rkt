#lang typed/racket

(require "core-lang.rkt")
(require "core-logic.rkt")
(require "core-expr.rkt")
(require "error.rkt")

(provide (struct-out pos) Pos Parse-monad parse-program)

; Position

(struct pos
  ([file : Any]
   [line : Any]
   [col  : Any])
  #:transparent #:type-name Pos)

(: get-pos (-> (Syntaxof Any) Pos))
(define (get-pos s)
  (pos (syntax-source s) (syntax-line s) (syntax-column s)))

; Parser

(define-type (Parse-monad a)
  (Try (User-error Pos) a))

(: ouch! (All (a b) (-> (Syntaxof Any) String * (Parse-monad a))))
(define (ouch! p . xs)
  (raise-error
    (user-error (get-pos p) 'parse-error (apply string-append xs))))

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

; The following two are internal to the "parse-log" procedure, but if we put these two
; inside, the compiler loops forever :(
(define-type Induction-type
  (U 'induction 'induction<))

(define-predicate induction-type? Induction-type)

(: parse-log (-> (Syntaxof Any) (Parse-monad Log-expr)))
(define (parse-log s)

  (: make-quant (-> Quantifier-name (Syntaxof Any) (Syntaxof Any) (Parse-monad Log-expr)))
  (define (make-quant n vars f)
    (combine2
      (let ([vars-list (syntax->datum vars)])
        (if (and (list? vars-list) (pair? vars-list) (andmap symbol? vars-list))
            (return vars-list)
            (ouch! s "Malformed variable list in " (symbol->string n))))
      (parse-log f)
      (λ ([vs : (Listof Symbol)] [fp : Log-expr])
         (return (log-quant n vs fp)))))

  (: make-init-name (-> Symbol Symbol))
  (define (make-init-name s)
    (string->symbol (string-append "init-" (symbol->string s))))

  (: make-init (-> (Listof Any) (Parse-monad Log-expr)))
  (define (make-init xs)
    (if (andmap symbol? xs)
        (return (log-op 'and
          (map (λ ([s : Symbol]) (log-= (a-var s) (a-var (make-init-name s)))) xs)))
        (ouch! s "Not a variable in \"init\"")))
  
  (: make-induction (-> Induction-type (Syntaxof Any) (Syntaxof Any) (Parse-monad Log-expr)))
  (define (make-induction type vars f)
    (combine2
      (let ([vars-list (syntax->datum vars)])
        (if (and (list? vars-list) (pair? vars-list)
                 (andmap symbol? vars-list) (null? (cdr vars-list)))
            (return (car vars-list))
            (ouch! s "Malformed variable list in the \"induction\" macro")))
      (parse-log f)
      (λ ([x : Symbol] [fp : Log-expr])
         (return
          (match type
            ['induction
             (log-impl (subst x (a-const 0) fp)
                       (log-quant 'forall (list x)
                                  (log-impl (log->= (a-var x) (a-const 0))
                                            fp
                                            (subst x (a-op '+ (list (a-var x) (a-const 1))) fp)))
                       (log-quant 'forall (list x)
                                  (log-impl (log->= (a-var x) (a-const 0)) fp)))]
            ['induction<
             (let ([y (gensym x)])
               (log-impl (log-quant 'forall (list x)
                                    (log-impl (log->= (a-var x) (a-const 0))
                                              (log-quant 'forall (list y)
                                                         (log-impl (log->= (a-var y) (a-const 0))
                                                                   (log-<  (a-var y) (a-var x))
                                                                   (subst x (a-var y) fp)))
                                              fp))
                         (log-quant 'forall (list x)
                                    (log-impl (log->= (a-var x) (a-const 0)) fp))))])))))

  (let ([e (syntax-e s)])
    (cond
      [(boolean? e)
       (return (log-const e))]
      [(eq? e 'true)
       (return (log-const #t))]
      [(eq? e 'false)
       (return (log-const #f))]
      [(symbol? e)
       (ouch! s "Illegal syntax: bare variable \""
                (symbol->string e) "\" in an assertion.\n"
                "Use \"(" (symbol->string e) ")\" "
                "if you mean a nullary relation.")]
      [else
       (let ([ss (syntax->list s)])
         (if (not (and (list? ss) (pair? ss)))
             (ouch! s "Ill-formed assertion \"()\"")
             (let ([head (syntax->datum (first ss))])
               (cond
                 [(log-oper? head)
                  (bind (apply combine (map parse-log (rest ss)))
                        (λ ([xs : (Listof Log-expr)]) (return (log-op head xs))))]
                 [(and (log-cmpr? head) (>= (length ss) 2))
                  (bind (apply combine (map parse-a (rest ss)))
                        (λ ([xs : (Listof A-expr)]) (return (log-cmp head xs))))]
                 [(log-cmpr? head)
                  (ouch! s "Expected at least 2 arguments to \""
                           (symbol->string head) "\"")]
                 [(and (quantifier-name? head) (= (length ss) 3))
                  (make-quant head (second ss) (third ss))]
                 [(eq? head 'forall)
                  (ouch! s "The correct form is \"(forall (x y ... z) <formula>)\"")]
                 [(eq? head 'exists)
                  (ouch! s "The correct form is \"(exists (x y ... z) <formula>)\"")]
                 [(eq? head 'init)
                  (make-init (map syntax->datum (rest ss)))]
                 [(and (induction-type? head) (= (length ss) 3))
                  (make-induction head (second ss) (third ss))]
                 [(induction-type? head)
                  (ouch! s "The correct form is \"(induction (x) <formula>)\"")]
                 [(symbol? head)
                  (bind (apply combine (map parse-a (rest ss)))
                        (λ ([xs : (Listof A-expr)]) (return (log-rel head xs))))]
                 [(pair? (cdr ss))
                  (let ([sec (syntax->datum (second ss))])
                    (if (or (log-oper? sec) (log-cmpr? sec))
                        (ouch! s "Illegal syntax: infix operator \""
                                 (symbol->string sec) "\"?")
                        (ouch! s "Ill-formed assertion")))]
                 [else
                  (ouch! s "Ill-formed assertion")]))))])))

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
