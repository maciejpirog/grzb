#lang typed/racket

(provide (all-defined-out))

; Arithmetic expressions

(struct a-const
  ([val : Integer])
  #:transparent #:type-name A-const)

(struct a-var
  ([x : Symbol])
  #:transparent #:type-name A-var)

(struct a-select ; run-time only constructor
  ([l : Lexpr]
   [i : A-expr])
  #:transparent #:type-name A-select)

(struct a-op
  ([o    : A-oper]
   [args : (Listof A-expr)])
  #:transparent #:type-name A-op)

(define-type A-oper
  (U '+ '- '/ '* '%))

(define-predicate a-oper? A-oper)

(define-type A-expr
  (U A-const A-var A-select A-op))

(define-predicate a-expr? A-expr)

; Arrays

(struct lexpr-store
  ([x   : Lexpr]
   [i   : A-expr]
   [val : A-expr])
  #:transparent #:type-name Lexpr-store)

(define-type Lexpr
  (U Symbol Lexpr-store))

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

; List free variables

(: lexpr-free-vars (-> Lexpr (Listof Symbol)))
(define (lexpr-free-vars l)
  (match l
    [(lexpr-store x i e) (append (lexpr-free-vars x) (a-free-vars i) (a-free-vars e))]
    [_ null]))

(: a-free-vars (-> A-expr (Listof Symbol)))
(define (a-free-vars e)
  (match e
    [(a-const v)     null]
    [(a-var x)       (list x)]
    [(a-select x i)  (append (lexpr-free-vars x) (a-free-vars i))]
    [(a-op o xs)     (if (pair? xs)
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

; List arrays

(: lexpr-list-arrays (-> Lexpr (Listof Symbol)))
(define (lexpr-list-arrays l)
  (match l
    [(lexpr-store x i e)
     (append (lexpr-list-arrays x)
             (a-list-arrays i)
             (a-list-arrays e))]
    [x (if (symbol? x)
           (list x)
           (error "Impossible: malformed Lexpr"))])) ; Type-checker too weak to know this :(

(: a-list-arrays (-> A-expr (Listof Symbol)))
(define (a-list-arrays e)
  (match e
    [(a-const v)     null]
    [(a-var x)       null]
    [(a-select x i)  (set-union (lexpr-list-arrays x) (a-list-arrays i))]
    [(a-op o xs)     (if (pair? xs)
                         (apply set-union (map a-list-arrays xs))
                         null)]))

; Renaming variables

(: lexpr-rename (-> (-> Symbol Symbol) Lexpr Lexpr))
(define (lexpr-rename f l)
  (match l
    [(lexpr-store x i e)
     (lexpr-store (lexpr-rename f x) (a-rename f i) (a-rename f e))]
    [x x]))

(: a-rename (-> (-> Symbol Symbol) A-expr A-expr))
(define (a-rename f e)
  (match e
    [(a-const v)     (a-const v)]
    [(a-var x)       (a-var (f x))]
    [(a-select x i)  (a-select (lexpr-rename f x) (a-rename f i))]
    [(a-op o xs)     (a-op o (map (λ ([g : A-expr]) (a-rename f g)) xs))]))

; Substitution

(: lexpr-subst-a (-> Symbol A-expr Lexpr Lexpr))
(define (lexpr-subst-a x e l)
  (match l
    [(lexpr-store y i f)
     (lexpr-store (lexpr-subst-a x e y) (a-subst-a x e i) (a-subst-a x e f))]
    [x x]))

(: a-subst-a (-> Symbol A-expr A-expr A-expr))
(define (a-subst-a x e ee)
  (match ee
    [(a-const v)
     (a-const v)]
    [(a-var y)
     (if (eq? x y) e ee)]
    [(a-select y i)
     (a-select (lexpr-subst-a x e y) (a-subst-a x e i))]
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

; Store substitution

(: lexpr-subst-store (-> Symbol A-expr A-expr Lexpr Lexpr))
(define (lexpr-subst-store x i l ll)
  (match ll
    [(lexpr-store f y j)
     (lexpr-store (lexpr-subst-store x i l f)
                  (a-subst-store x i l y)
                  (a-subst-store x i l j))]
    [s
     (if (eq? s x)
         (lexpr-store s i l)
         s)]))

(: a-subst-store (-> Symbol A-expr A-expr A-expr A-expr))
(define (a-subst-store x i e ee)
  (match ee
    [(a-const v)
     (a-const v)]
    [(a-var y)
     (a-var y)]
    [(a-select y j)
     (a-select (lexpr-subst-store x i e y) (a-subst-store x i e j))]
    [(a-op s es)
     (a-op s (map (lambda ([f : A-expr]) (a-subst-store x i e f)) es))]))

; Pretty printing

(: lexpr-pretty-print (-> Lexpr Any))
(define (lexpr-pretty-print l)
  (match l
    [(lexpr-store l i e)
     (list 'store (a-pretty-print i) (a-pretty-print e) (lexpr-pretty-print l))]
    [x x]))

(: a-pretty-print (-> A-expr Any))
(define (a-pretty-print e)
  (match e
    [(a-const v)     v]
    [(a-var x)       x]
    [(a-select x i)  (cons (lexpr-pretty-print x) (a-pretty-print i))]
    [(a-op o es)     (list* o (map a-pretty-print es))]))

(: b-pretty-print (-> B-expr Any))
(define (b-pretty-print e)
  (match e
    [(b-const v) v]
    [(b-var x) x]
    [(b-op o es) (list* o (map b-pretty-print es))]
    [(b-cmp o es) (list* o (map a-pretty-print es))]))
