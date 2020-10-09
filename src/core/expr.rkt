#lang typed/racket

(provide (all-defined-out))

; Arithmetic expressions

(struct a-const
  ([val : Integer])
  #:transparent #:type-name A-const)

(struct a-var
  ([x : Symbol])
  #:transparent #:type-name A-var)

(struct a-op
  ([o : A-oper]
   [args : (Listof A-expr)])
  #:transparent #:type-name A-op)

(define-type A-oper
  (U '+ '- '/ '* '%))

(define-predicate a-oper? A-oper)

(define-type A-expr
  (U A-const A-var A-op))

(define-predicate a-expr? A-expr)

; Boolean expressions

(struct b-const
  ([val : Boolean])
  #:transparent #:type-name B-const)

(struct b-var
  ([x : Symbol])
  #:transparent #:type-name B-var)

(struct b-op
  ([o : B-oper]
   [args : (Listof B-expr)])
  #:transparent #:type-name B-op)

(struct b-cmp
  ([o : B-cmpr]
   [args : (Listof A-expr)])
  #:transparent #:type-name B-cmp)

(define-type B-oper
  (U 'and 'or 'not))

(define-predicate b-oper? B-oper)

(define-type B-cmpr
  (U '= '< '<= '> '>=))

(define-predicate b-cmpr? B-cmpr)

(define-type B-expr
  (U B-const B-var B-op B-cmp))

(define-predicate b-expr? B-expr)

; Expressions of any type

(define-type Some-expr
  (U A-expr B-expr))

; Free variables

(: a-free-vars (-> A-expr (Listof Symbol)))
(define (a-free-vars e)
  (match e
    [(a-const v) null]
    [(a-var x)   (list x)]
    [(a-op o xs) (if (pair? xs)
                     (apply set-union (map a-free-vars xs))
                     null)]))

(: b-free-vars (-> B-expr (Listof Symbol)))
(define (b-free-vars e)
  (match e
    [(b-const v)  null]
    [(b-var x)    (list x)]
    [(b-op o xs)  (if (pair? xs)
                      (apply set-union (map b-free-vars xs))
                      null)]
    [(b-cmp o xs) (if (pair? xs)
                      (apply set-union (map a-free-vars xs))
                      null)]))

; Substitution

(: a-subst-a (-> Symbol A-expr A-expr A-expr))
(define (a-subst-a x e ee)
  (match ee
    [(a-const v)
     (a-const v)]
    [(a-var y)
     (if (eq? x y) e ee)]
    [(a-op s es)
     (a-op s (map (lambda ([f : A-expr]) (a-subst-a x e f)) es))]))

(: b-subst-a (-> Symbol A-expr B-expr B-expr))
(define (b-subst-a x e ee)
  (match ee
    [(b-const v)
     (b-const v)]
    [(b-var x)
     (b-var x)]
    [(b-op s es)
     (b-op s (map (lambda ([f : B-expr]) (b-subst-a x e f)) es))]
    [(b-cmp s es)
     (b-cmp s (map (lambda ([f : A-expr]) (a-subst-a x e f)) es))]))

(: b-subst-b (-> Symbol B-expr B-expr B-expr))
(define (b-subst-b x e ee)
  (match ee
    [(b-const v)
     (b-const v)]
    [(b-var y)
     (if (eq? x y) e ee)]
    [(b-op s es)
     (b-op s (map (lambda ([f : B-expr]) (b-subst-b x e f)) es))]
    [(b-cmp s es)
     (b-cmp s es)]))

; Pretty printing

(: a-pretty-print (-> A-expr Any))
(define (a-pretty-print e)
  (match e
    [(a-const v) v]
    [(a-var x) x]
    [(a-op o es) (list* o (map a-pretty-print es))]))

(: b-pretty-print (-> B-expr Any))
(define (b-pretty-print e)
  (match e
    [(b-const v) v]
    [(b-var x) x]
    [(b-op o es) (list* o (map b-pretty-print es))]
    [(b-cmp o es) (list* o (map a-pretty-print es))]))
