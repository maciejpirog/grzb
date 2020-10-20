#lang typed/racket

(require "expr.rkt")
(require "../logic.rkt")

(provide (all-defined-out))

; Thing that have metadata

(struct (m d) with-meta
  ([meta : m]
   [data : d])
  #:transparent #:type-name With-meta)

(: get-meta (All (m d) (-> (With-meta m d) m)))
(define get-meta with-meta-meta)

(: get-data (All (m d) (-> (With-meta m d) d)))
(define get-data with-meta-data)

; Syntax of commands

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

; Syntax

(define-type (Core-cons meta)
  (U Skip (Comp meta) Assign Store (While meta) (While* meta)
     (If-stm meta) Annot))

(define-type (Core meta)
  (With-meta meta (Core-cons meta)))

(: make-core (All (meta) (-> meta (Core-cons meta) (Core meta))))
(define (make-core m c)
  (with-meta m c))

; Programs (axioms, definitions, checks, a command)

(struct axiom
  ([val : Log-expr])
  #:transparent #:type-name Axiom)

(struct check
  ([val  : Log-expr])
  #:transparent #:type-name Check)

(struct (meta) def
  ()
  #:transparent #:type-name Def)

(define-type (Garnish-data meta)
  (U Axiom Check (Def meta)))

(define-type (Garnish meta)
  (With-meta meta (Garnish-data meta)))

(struct (meta) program
  ([axioms : (Listof (With-meta meta Axiom))]
   [checks : (Listof (With-meta meta Check))]
   [defs   : (Listof (With-meta meta (Def meta)))]
   [cmd    : (Core meta)])
   #:transparent #:type-name Program)

(: list-axioms (All (meta) (-> (Program meta) (Listof Log-expr))))
(define (list-axioms p)

  ; no idea why the type checker needs this :(
  (: get-data-aux (-> (With-meta meta Axiom) Axiom))
  (define get-data-aux get-data)
  
  (map (compose from-axiom (compose axiom-val get-data-aux)) (program-axioms p)))

  