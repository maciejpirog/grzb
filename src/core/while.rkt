#lang typed/racket

(require "expr.rkt")
(require "../logic.rkt")

(provide (all-defined-out))

; Program-syntax

(struct skip
  ()
  #:transparent #:type-name Skip)

(struct (meta) comp
  ([l : (Core meta)]
   [r : (Core meta)])
  #:transparent #:type-name Comp)

(struct assign
  ([x : Symbol]
   [e : A-expr])
  #:transparent #:type-name Assign)

(struct store
  ([x : Symbol]
   [i : A-expr]
   [e : A-expr])
  #:transparent #:type-name Store)

(struct (meta) while
  ([inv : Log-expr]
   [b   : B-expr]
   [c   : (Core meta)])
  #:transparent #:type-name While)

(struct (meta) while*
  ([inv : Log-expr]
   [dec : A-expr]
   [b   : B-expr]
   [c   : (Core meta)])
  #:transparent #:type-name While*)

(struct (meta) if-stm
  ([b : B-expr]
   [t : (Core meta)]
   [e : (Core meta)])
  #:transparent #:type-name If-stm)

(struct annot
  ([val : Log-expr])
  #:transparent #:type-name Annot)

(struct axiom
  ([val : Log-expr])
  #:transparent #:type-name Axiom)

(struct check
  ([val : Log-expr])
  #:transparent #:type-name Check)

; Syntax

(define-type (Core-cons meta)
  (U Skip (Comp meta) Assign Store (While meta) (While* meta)
     (If-stm meta) Annot Axiom Check))

(define-type (Core meta)
  (Pair meta (Core-cons meta)))

(: make-core (All (meta) (-> meta (Core-cons meta) (Core meta))))
(define (make-core m c)
  (cons m c))

; Syntax helpers

(: core-meta (All (meta) (-> (Core meta) meta)))
(define (core-meta c)
  (car c))

(: core-data (All (meta) (-> (Core meta) (Core-cons meta))))
(define (core-data c)
  (cdr c))

; Extract axioms

(: list-axioms (All (meta) (-> (Core meta) (Listof Log-expr))))
(define (list-axioms s)
  (match (core-data s)
    [(axiom f) (list (from-axiom f))]
    [(comp a b) (append (list-axioms a) (list-axioms b))]
    [_ null]))
