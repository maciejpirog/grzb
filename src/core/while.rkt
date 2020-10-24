#lang typed/racket

(require "expr.rkt")
(require "../logic.rkt")
(require "../utils/assoc.rkt")

(provide (all-defined-out))

; Things that have metadata

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

(struct proc-call
  ([name : Symbol]
   [args : (Listof Arg-expr)])
  #:transparent #:type-name Proc-call)

(struct annot
  ([val : Log-expr])
  #:transparent #:type-name Annot)

(struct dummy-po
  ()
  #:transparent #:type-name Dummy-po)

; Syntax

(define-type (Core-cons meta)
  (U Skip (Comp meta) Assign Store (While meta) (While* meta)
     (If-stm meta) Proc-call Annot Dummy-po))

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
  ([name : Symbol]
   [args : (Listof Symbol)]
   [pre  : Log-expr]
   [post : Log-expr]
   [body : (Core meta)])
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

; Auxiliary syntax manipulation

(: list-axioms (All (meta) (-> (Program meta) (Listof Log-expr))))
(define (list-axioms p)

  ; no idea why the type checker needs this :(
  (: get-data-aux (-> (With-meta meta Axiom) Axiom))
  (define get-data-aux get-data)
  
  (map (compose close-universally (compose axiom-val get-data-aux))
       (program-axioms p)))

(: proc-specification (All (meta) (-> (Program meta) Symbol
                                      (Values (Listof Symbol) Log-expr Log-expr))))
(define (proc-specification p s)

  (: this-one? (-> (With-meta meta (Def meta)) Boolean))
  (define (this-one? w)
    (eq? (def-name (get-data w)) s))
  
  (let ([w (filter this-one? (program-defs p))])
    (if (null? w)
        (error "Impossible! Undefined procedure" s)
        (let ([d (get-data (car w))])
          (values (def-args d) (def-pre d) (def-post d))))))
