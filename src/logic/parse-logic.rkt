#lang typed/racket

(require "../core/expr.rkt")
(require "../core/parse-expr.rkt")
(require "logic-internals.rkt")
(require "../utils/error.rkt")
(require "../utils/parse-utils.rkt")
(require "../utils/assoc.rkt")

(provide parse-log)

(define-type Induction-type
  (U 'induction 'induction<))

(define-predicate induction-type? Induction-type)

(: parse-log (-> (Syntaxof Any) (Parse-monad Log-expr)))
(define (parse-log s)

  (: parse-quant (-> Quantifier-name (Syntaxof Any) (Syntaxof Any) (Parse-monad Log-expr)))
  (define (parse-quant name vars f)
    (combine2
      (let ([vars-list (syntax->datum vars)])
        (if (and (list? vars-list) (pair? vars-list) (andmap symbol? vars-list))
            (return vars-list)
            (ouch! s "Malformed variable list in " (symbol->string name))))
      (parse-log f)
      (λ ([vs : (Listof Symbol)] [fp : Log-expr])
         (return (make-quant name vs fp)))))

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
                  (parse-quant head (second ss) (third ss))]
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
