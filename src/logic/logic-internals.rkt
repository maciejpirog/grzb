#lang typed/racket

(require "../core/expr.rkt")
(require "../utils/assoc.rkt")

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
   [args : (Listof Log-rel-arg)])
  #:transparent #:type-name Log-rel)

; Syntax
                    
(define-type Log-oper
  (U 'and 'or 'not 'impl 'iff))

(define-predicate log-oper? Log-oper)

(define-type Log-cmpr
  (U '= '< '<= '> '>=))

(define-predicate log-cmpr? Log-cmpr)

(define-type Quantifier-name
  (U 'forall 'exists 'forall-array 'exists-array))

(define-predicate quantifier-name? Quantifier-name)

(define-type Log-rel-arg
  (U A-expr Lexpr))

(define-type Log-expr
  (U Log-const Log-var Log-op Log-cmp Log-quant Log-rel))

(define-predicate log-expr? Log-expr)

; Syntax constructors shorthands

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

; Free variables, arrays, and relations

(: log-rel-arg-free-vars (-> Log-rel-arg (Listof Symbol)))
(define (log-rel-arg-free-vars a)
  (cond [(a-expr? a)
         (a-free-vars a)]
        [(lexpr-store? a)
         (match a
           [(lexpr-store x i v)
            (set-union (a-free-vars i)
                       (a-free-vars v)
                       (log-rel-arg-free-vars x))])]
        [(symbol? a) null]))

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
    [(log-rel o xs)     (let ([ys (map log-rel-arg-free-vars xs)])
                          (if (pair? ys)
                              (apply set-union ys)
                              null))]))

(: log-rel-arg-list-arrays (-> Log-rel-arg (Listof Symbol)))
(define (log-rel-arg-list-arrays a)
  (cond [(a-expr? a)
         (a-list-arrays a)]
        [(lexpr-store? a)
         (match a
           [(lexpr-store x i v)
            (set-union (a-list-arrays i)
                       (a-list-arrays v)
                       (log-rel-arg-list-arrays x))])]
        [(symbol? a) (list a)]))

(: log-list-arrays (-> Log-expr (Listof Symbol)))
(define (log-list-arrays e)
  (match e
    [(log-const v)      null]
    [(log-var x)        (error "log-var")]
    [(log-op o xs)      (if (pair? xs)
                            (apply set-union (map log-list-arrays xs))
                            null)]
    [(log-cmp o xs)     (if (pair? xs)
                            (apply set-union (map a-list-arrays xs))
                            null)]
    [(log-quant n vs f) (set-subtract (log-list-arrays f) vs)]
    [(log-rel o xs)     (if (pair? xs)
                            (apply set-union (map log-rel-arg-list-arrays xs))
                            null)]))

(: log-rels (-> Log-expr (Listof (Pair Symbol (Listof Boolean))))) ; #t = a-expr
(define (log-rels e)
  (match e
    [(log-const v)      null]
    [(log-var x)        (error "log-var")]
    [(log-op o xs)      (if (pair? xs)
                            (apply set-union (map log-rels xs))
                             null)]
    [(log-cmp o xs)     null]
    [(log-quant n vs f) (log-rels f)]
    [(log-rel o xs)     (list (cons o (map a-expr? xs)))]))

; Every time we create a node with a quantifier, we generate fresh variables.
; It is enough to avoid capture, as gensym guarantees uniqueness and variables
; bound by a quantifier cannot ever become free again.

(: make-quant (-> Quantifier-name (Listof Symbol) Log-expr Log-expr))
(define (make-quant name vs fp)
  (if (null? vs) fp
    (let ([fs (zip-fresh vs)])
      (log-quant name
                 (map (λ ([p : (Pairof Symbol Symbol)]) (cdr p)) fs)
                 (rename (assoc->fun fs) fp)))))

(: make-quant-array (-> Quantifier-name (Listof Symbol) Log-expr Log-expr))
(define (make-quant-array name vs fp)
  (if (null? vs) fp
    (let ([fs (zip-fresh vs)])
      (log-quant name
                 (map (λ ([p : (Pairof Symbol Symbol)]) (cdr p)) fs)
                 (rename-array (assoc->fun fs) fp)))))

(: close-with (-> Quantifier-name Log-expr Log-expr))
(define (close-with n f)
  (let ([fv (log-free-vars f)])
    (if (null? fv) f
        (make-quant n fv f))))

(: close-with-array (-> Quantifier-name Log-expr Log-expr))
(define (close-with-array n f)
  (let ([fv (log-list-arrays f)])
    (if (null? fv) f
        (make-quant-array n fv f))))

(: close-universally (-> Log-expr Log-expr))
(define (close-universally f)
  (close-with-array 'forall-array
    (close-with 'forall f)))

(: close-existentially (-> Log-expr Log-expr))
(define (close-existentially f)
  (close-with-array 'exists-array
    (close-with 'exists f)))

; Rename quantified variables to avoid capture and in wp for procedure call

(: log-rel-arg-rename (-> (Symbol -> Symbol) Log-rel-arg Log-rel-arg))
(define (log-rel-arg-rename r a)
  (cond [(a-expr? a)
         (a-rename r a)]
        [else (lexpr-rename r a)]))

(: rename (-> (Symbol -> Symbol) Log-expr Log-expr))
(define (rename r f)
  (match f
    [(log-const v)
     (log-const v)]
    [(log-var y)
     (log-var (r y))]
    [(log-op o fs)
     (log-op o (map (λ ([g : Log-expr]) (rename r g)) fs))]
    [(log-cmp o as)
     (log-cmp o (map (λ ([g : A-expr]) (a-rename r g)) as))]
    [(log-quant n vs g)
     (log-quant n vs (rename r g))] ; vs are distinct thanks to gensym
    [(log-rel o as)
     (log-rel o (map (λ ([g : Log-rel-arg]) (log-rel-arg-rename r g)) as))]))

(: log-rel-arg-rename-array (-> (Symbol -> Symbol) Log-rel-arg Log-rel-arg))
(define (log-rel-arg-rename-array r a)
  (cond [(a-expr? a)     (a-rename-array r a)]
        [else        (lexpr-rename-array r a)]))

(: rename-array (-> (Symbol -> Symbol) Log-expr Log-expr))
(define (rename-array r f)
  (match f
    [(log-const v)
     (log-const v)]
    [(log-var y)
     (log-var y)]
    [(log-op o fs)
     (log-op o (map (λ ([g : Log-expr]) (rename-array r g)) fs))]
    [(log-cmp o as)
     (log-cmp o (map (λ ([g : A-expr]) (a-rename-array r g)) as))]
    [(log-quant n vs g)
     (log-quant n vs (rename-array r g))] ; vs are distinct thanks to gensym
    [(log-rel o as)
     (log-rel o (map (λ ([g : Log-rel-arg]) (log-rel-arg-rename-array r g)) as))]))

; Substitution

(: log-rel-arg-subst-a (-> Symbol A-expr Log-rel-arg Log-rel-arg))
(define (log-rel-arg-subst-a x e a)
  (cond [(a-expr? a)     (a-subst-a x e a)]
        [else        (lexpr-subst-a x e a)]))

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
     (log-rel o (map (lambda ([g : Log-rel-arg]) (log-rel-arg-subst-a x e g)) as))]))

(: subst-arg (-> Symbol Arg-expr Log-expr Log-expr))
(define (subst-arg x e f)
  (if (a-expr? e)
      (subst-a x e f)
      (subst-a x (a-var (by-ref-var e)) f)))

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

; Store substitution

(: log-rel-arg-subst-store (-> Symbol A-expr A-expr Log-rel-arg Log-rel-arg))
(define (log-rel-arg-subst-store x i e a)
  (cond [(a-expr? a)     (a-subst-store x i e a)]
        [else        (lexpr-subst-store x i e a)]))

(: subst-store (-> Symbol A-expr A-expr Log-expr Log-expr))
(define (subst-store x i e f)
  (match f
    [(log-const v)
     (log-const v)]
    [(log-var y)
     (log-var y)]
    [(log-op o fs)
     (log-op o (map (λ ([g : Log-expr]) (subst-store x i e g)) fs))]
    [(log-cmp o fs)
     (log-cmp o (map (λ ([g : A-expr]) (a-subst-store x i e g)) fs))]
    [(log-quant n vs g)
     (log-quant n vs (subst-store x i e g))]
    [(log-rel o fs)
     (log-rel o (map (λ ([g : Log-rel-arg]) (log-rel-arg-subst-store x i e g))
                     fs))]))

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

(: print-formula (-> Log-expr Void))
(define (print-formula f)
  (match (extract-assumptions f)
    [(cons xs g)
     (map (λ ([h : Log-expr]) (printf "* ~a~n" (log-pretty-print h)))
          xs)
     (map (λ ([h : Log-expr]) (printf "=> ~a~n" (log-pretty-print h)))
          (split-and g))])
  (void))

(: log-pretty-print (-> Log-expr Any))
(define (log-pretty-print e)

  (: rel-arg-pretty-print (-> Log-rel-arg Any))
  (define (rel-arg-pretty-print r)
    (if (a-expr? r)
        (a-pretty-print r)
        (list 'quote (lexpr-pretty-print r))))
  
  (match e
    [(log-const v) v]
    [(log-var x) x]
    [(log-op o es) (list* o (map log-pretty-print es))]
    [(log-cmp o es) (list* o (map a-pretty-print es))]
    [(log-quant n vs f) (list n vs (log-pretty-print f))]
    [(log-rel o es) (list* o (map rel-arg-pretty-print es))]))
