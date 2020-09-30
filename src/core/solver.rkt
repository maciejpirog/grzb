#lang typed/racket

(require "proof-obligations.rkt")
(require "core-expr.rkt")
(require "core-logic.rkt")
(require "core-lang.rkt")
(require "error.rkt")

(require z3/smt)

(provide Var-mode discharge discharge*)

(define-type Var-mode
  (U 'real 'integer))

(: var-mode (-> Var-mode Z3-Sort))
(define (var-mode s)
  (if (eq? s 'real) Real/s Int/s))

(: declare-vars (-> Var-mode Log-expr (Listof Z3-Ast)))
(define (declare-vars vm f)
  (map (λ ([s : Symbol]) (dynamic-declare-const s (var-mode vm)))
       (log-free-vars f)))

(: declare-rels (-> Var-mode Log-expr (Listof (U Smt-Func Z3-Ast))))
(define (declare-rels vm f)
  (map (λ ([sa : (Pair Symbol Exact-Nonnegative-Integer)])
          (dynamic-declare-fun (car sa) (make-list (cdr sa) (var-mode vm)) Bool/s))
       (log-rels f)))

(: assert-formula (-> Var-mode Log-expr Void))
(define (assert-formula vm f)

  (: a-oper->s-oper (-> A-oper (-> Smt-Expr Smt-Expr * Smt-Expr)))
  (define (a-oper->s-oper s)
    (match s
      ['+ +/s] ['- -/s] ['* */s] ['/ //s] ['% mod/s])) 

  (: make-formula-a (-> A-expr Smt-Expr))
  (define (make-formula-a e)
    (match e
      [(a-const n) n]
      [(a-var x)   x]
      [(a-op s xs) (if (pair? xs)
                       (apply (a-oper->s-oper s)
                              (map make-formula-a xs))
                       0)]))

  (: log-oper->s-oper (-> Log-oper (-> Smt-Expr Smt-Expr * Smt-Expr)))
  (define (log-oper->s-oper s)
    (match s
      ['and and/s] ['or or/s] ['impl =>/s]))

  (: log-cmpr->s-oper (-> Log-cmpr (-> Smt-Expr Smt-Expr Z3-Ast)))
  (define (log-cmpr->s-oper s)
    (match s
      ['= =/s] ['> >/s] ['>= >=/s] ['< </s] ['<= <=/s]))

  (: make-pairwise (-> (-> Smt-Expr Smt-Expr Z3-Ast) (Listof Smt-Expr) Smt-Expr))
  (define (make-pairwise op xs)
    (cond
      [(null? xs)        true/s]
      [(null? (cdr xs))  true/s]
      [(null? (cddr xs)) (op (first xs) (second xs))]
      [else              (and/s (op (first xs) (second xs))
                                (make-pairwise op (rest xs)))]))

  (: make-formula (-> Log-expr Smt-Expr))
  (define (make-formula f)
    (match f
      [(log-const #t)
       true/s]
      
      [(log-const #f)
       false/s]
      
      [(log-var x)
       (error "log-var")]
      
      [(log-op 'not xs)
       (if (pair? xs)
           (not/s (make-formula (first xs)))
           false/s)]
      
      [(log-op 'iff xs)
       (make-pairwise =/s (map make-formula xs))]
      
      [(log-op s xs)
       (if (pair? xs)
           (apply (log-oper->s-oper s)
                  (map make-formula xs))
           false/s)]
      
      [(log-cmp s xs)
       (if (and (pair? xs) (pair? (rest xs)))
           (make-pairwise (log-cmpr->s-oper s) (map make-formula-a xs))
           (error "Impossible: expected at least 2 arguments to" s))]
           
      
      [(log-quant 'forall vs g)
       (dynamic-forall/s vs
                         (map (const (var-mode vm)) vs)
                         (make-formula g))]

      [(log-quant 'exists vs g)
       (dynamic-exists/s vs
                         (map (const (var-mode vm)) vs)
                         (make-formula g))]
      
      [(log-rel s xs)
       (apply @/s (list* s (map make-formula-a xs)))]))

  (assert! (make-formula f)))

(: discharge (All (meta) (-> Var-mode
                             (Proof-obligation meta)
                             (U 'ok 'dunno String))))
(define (discharge vm ob)
  (with-new-context
    (declare-vars   vm (proof-obligation-f ob))
    (declare-rels   vm (proof-obligation-f ob))
    (assert-formula vm (log-not (proof-obligation-f ob)))
    (let* ([m (check-sat/model)]
           [c (get-context)])
      (if (symbol? m)
          (match m ['unsat 'ok] ['unknown 'dunno])
          (model->string c m)))))

(: discharge* (All (meta) (-> Var-mode
                              (Listof (Proof-obligation meta))
                              (Listof (User-error meta)))))
(define (discharge* vm xs)

  (: disc-one (-> (Proof-obligation meta) (Listof (User-error meta))))
  (define (disc-one ob)
    (let ([r (discharge vm ob)]
          [meta (proof-obligation-meta ob)])
      (if (symbol? r)
          (match r
            ['ok    null]
            ['dunno (list (user-error (proof-obligation-meta ob)
                                      'smt-dunno
                                      ob))])
          (list (user-error (proof-obligation-meta ob)
                            'smt-counterexample
                            (cons ob r))))))
  
  (apply append (map disc-one xs)))
