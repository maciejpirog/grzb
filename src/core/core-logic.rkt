#lang typed/racket

(require "core-expr.rkt")

(provide (all-defined-out))

; Constructors of syntax of the logic used in assertions

(struct log-const
  ([val : Boolean])
  #:transparent #:type-name Log-const)

(struct log-var ; not used at the moment
  ([x : Symbol])
  #:transparent #:type-name Log-var)

(struct log-op
  ([o    : Log-oper]
   [args : (Listof Log-expr)])
  #:transparent #:type-name Log-op)

(struct log-cmp
  ([o    : Log-cmpr]
   [args : (Listof A-expr)])
  #:transparent #:type-name Log-cmp)

(struct log-quant
  ([qname : Quantifier-name]
   [vars  : (Listof Symbol)]
   [f     : Log-expr])
  #:transparent #:type-name Log-quant)

(struct log-rel
  ([o    : Symbol]
   [args : (Listof A-expr)])
  #:transparent #:type-name Log-rel)

; Syntax
                    
(define-type Log-oper
  (U 'and 'or 'not 'impl 'iff))

(define-predicate log-oper? Log-oper)

(define-type Log-cmpr
  (U '= '< '<= '> '>=))

(define-predicate log-cmpr? Log-cmpr)

(define-type Quantifier-name
  (U 'forall 'exists))

(define-predicate quantifier-name? Quantifier-name)

(define-type Log-expr
  (U Log-const Log-var Log-op Log-cmp Log-quant Log-rel))

(define-predicate log-expr? Log-expr)

; Some helpers

(: log-and (-> Log-expr * Log-expr))
(define (log-and . xs)
  (log-op 'and xs))

(: log-not (-> Log-expr Log-expr))
(define (log-not a)
  (log-op 'not (list a)))

(: log-impl (-> Log-expr * Log-expr))
(define (log-impl . xs)
  (log-op 'impl xs))

(: log-= (-> A-expr A-expr Log-expr))
(define (log-= a b)
  (log-cmp '= (list a b)))

(: log-< (-> A-expr A-expr Log-expr))
(define (log-< a b)
  (log-cmp '< (list a b)))

(: log->= (-> A-expr A-expr Log-expr))
(define (log->= a b)
  (log-cmp '>= (list a b)))

; Reify Boolean expressions as logic formulas

(: reify-bool (-> B-expr Log-expr))
(define (reify-bool b)
  (match b
    [(b-const x)  (log-const x)]
    [(b-var x)    (log-var x)]
    [(b-op s xs)  (log-op s (map reify-bool xs))]
    [(b-cmp s xs) (log-cmp s xs)]))

; Free variables and relations

(: log-free-vars (-> Log-expr (Listof Symbol)))
(define (log-free-vars e)
  (match e
    [(log-const v)      null]
    [(log-var x)        (error "log-var")]
    [(log-op o xs)      (if (pair? xs)
                            (apply set-union (map log-free-vars xs))
                            null)]
    [(log-cmp o xs)     (if (pair? xs)
                            (apply set-union (map a-free-vars xs))
                            null)]
    [(log-quant n vs f) (set-subtract (log-free-vars f) vs)]
    [(log-rel o xs)     (if (pair? xs)
                            (apply set-union (map a-free-vars xs))
                             null)]))

(: log-rels (-> Log-expr (Listof (Pair Symbol Exact-Nonnegative-Integer))))
(define (log-rels e)
  (match e
    [(log-const v)      null]
    [(log-var x)        (error "log-var")]
    [(log-op o xs)      (if (pair? xs)
                            (apply set-union (map log-rels xs))
                             null)]
    [(log-cmp o xs)     null]
    [(log-quant n vs f) (log-rels f)]
    [(log-rel o xs)     (list (cons o (length xs)))]))

(: close (-> Log-expr Log-expr))
(define (close f)
  (let ([fv (log-free-vars f)])
    (if (null? fv) f
        (log-quant 'forall fv f))))

; Substitution

(: subst-a (-> Symbol A-expr Log-expr Log-expr))
(define (subst-a x e f)
  (match f
    [(log-const v)
     (log-const v)]
    [(log-var y)
     (log-var y)]
    [(log-op o fs)
     (log-op o (map (lambda ([g : Log-expr]) (subst-a x e g)) fs))]
    [(log-cmp o as)
     (log-cmp o (map (lambda ([g : A-expr]) (a-subst-a x e g)) as))]
    [(log-quant n vs g)
     (if (member x vs) f
         (log-quant n vs (subst-a x e g)))]
    [(log-rel o as)
     (log-rel o (map (lambda ([g : A-expr]) (a-subst-a x e g)) as))]))

(: subst-b (-> Symbol B-expr Log-expr Log-expr))
(define (subst-b x e f)
  (match f
    [(log-const v)
     (log-const v)]
    [(log-var y)
     (if (eq? x y) (reify-bool e) f)]
    [(log-op o fs)
     (log-op o (map (lambda ([g : Log-expr]) (subst-b x e g)) fs))]
    [(log-cmp o fs)
     (log-cmp o fs)]
    [(log-quant n vs g)
     (if (member x vs) f
         (log-quant n vs (subst-b x e g)))]
    [(log-rel o fs)
     (log-rel o fs)]))

(: subst (-> Symbol Some-expr Log-expr Log-expr))
(define (subst x e f)
  (if (a-expr? e)
      (subst-a x e f)
      (subst-b x e f)))

; Pretty printing

(: split-and (-> Log-expr (Listof Log-expr)))
(define (split-and g)
  (match g
    [(log-op 'and xs) (append-map split-and xs)]
    [h (list h)]))

(: extract-assumptions (-> Log-expr (Pair (Listof Log-expr) Log-expr)))
(define (extract-assumptions f)
  (match f
    [(log-op 'impl xs)
     (let*-values ([(init last) (split-at-right xs 1)]
                   [(lextr) (extract-assumptions (car last))])
       (cons (append (append-map split-and init) (car lextr))
             (cdr lextr)))]
    [f (cons null f)]))

(: log-pretty-print (-> Log-expr Any))
(define (log-pretty-print e)
  (match e
    [(log-const v) v]
    [(log-var x) x]
    [(log-op o es) (list* o (map log-pretty-print es))]
    [(log-cmp o es) (list* o (map a-pretty-print es))]
    [(log-quant n vs f) (list n vs (log-pretty-print f))]
    [(log-rel o es) (list* o (map a-pretty-print es))]))
