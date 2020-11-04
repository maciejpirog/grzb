#lang typed/racket

(require "../core/expr.rkt")
(require "../core/while.rkt")
(require "../core/proc-call-graph.rkt")
(require "logic-internals.rkt")
(require "../utils/assoc.rkt")

(provide (struct-out proof-obligation) Proof-obligation gen-obligations with-axioms)

; Data structure to represent proof obligations

(struct (meta) proof-obligation
  ([meta : meta] [desc : Symbol] [f : Log-expr])
  #:transparent #:type-name Proof-obligation)

(: make-obligation (All (meta) (-> meta PO-type Log-expr Log-expr (proof-obligation meta))))
(define (make-obligation meta desc from to)
  (proof-obligation meta desc
    (log-impl from to)))

; Reify Boolean expressions as logic formulas

(: reify-bool (-> B-expr Log-expr))
(define (reify-bool b)
  (match b
    [(b-const x)  (log-const x)]
    [(b-var x)    (log-var x)]
    [(b-op s xs)  (log-op s (map reify-bool xs))]
    [(b-cmp s xs) (log-cmp s xs)]))

; Types of proof obligations

(define-type PO-type
  (U 'check 'user-defined 'while-postcondition 'while-body-precondition
     'while*-postcondition 'while*-body-precondition 'while*-variant-nonnegative
     'procedure-precondition 'procedure*-precondition 'dummy))

; Name of a variable that stores (internally in logic) the variant of
; the current procedure.
(: global-decr A-expr)
(define global-decr (a-var (gensym 'decr)))

; Calculate the weakest precondition together with proof obligations

(: weakest-precondition (All (meta) (-> (Program meta)
                                        (U Symbol Void)
                                        (Core meta)
                                        Log-expr
                                        (Pair Log-expr
                                              (Listof (Proof-obligation meta))))))
(define (weakest-precondition p current-proc c postcondition)

  ; Accumulator for proof obligations
  (: obs (Listof (Proof-obligation meta)))
  (define obs null)

  (: add! (-> (Proof-obligation meta) Void))
  (define (add! ob)
    (set! obs (cons ob obs)))

  ; Main loop
  (: wp (-> (Core meta) Log-expr Log-expr))
  (define (wp c f)
    (match (get-data c)

      [(skip)        f]

      [(comp l r)    (wp l (wp r f))]

      [(assign x e)  (subst x e f)]

      [(store x i e) (subst-store x i e f)]

      [(while i b d)
       (add! (make-obligation
               (get-meta c)
               'while-postcondition
               (log-and i (log-not (reify-bool b)))
               f))
       (add! (make-obligation
               (get-meta c)
               'while-body-precondition
               (log-and i (reify-bool b))
               (wp d i)))
       i]

      [(while* i v b d)
       (add! (make-obligation
               (get-meta c)
               'while*-postcondition
               (log-and i (log-not (reify-bool b)))
               f))
       (add! (let ([decr (a-var (gensym 'decr))])
               (make-obligation
                 (get-meta c)
                 'while*-body-precondition
                 (log-and (reify-bool b) i (log-= v decr))
                 (wp d (log-and          i (log-< v decr))))))
       (add! (make-obligation
               (get-meta c)
               'while*-variant-nonnegative
               (log-and i (reify-bool b))
               (log->= v (a-const 0))))
       i]

      [(if-stm b t e)
       (log-and (log-impl          (reify-bool b)  (wp t f))
                (log-impl (log-not (reify-bool b)) (wp e f)))]

      [(proc-call s as)
       (let* ([pd (get-procedure-def p s)]
              ; refresh all variables in the specification   
              [drename (zip-fresh (set-union (def-args pd)
                                             (log-free-vars (def-pre pd))
                                             (log-free-vars (def-post pd))))]
              [sub     (assoc->fun drename)]
              [dargs   (map    sub (def-args pd))]
              [dpre    (rename sub (def-pre  pd))]
              [dpost   (rename sub (def-post pd))]
              [odec    (def-dec pd)]
              [ddec    (if (void? odec)
                           (void)
                           (a-rename sub odec))]
              ; compute existentially and universally quantified variables
              [evars   (set-subtract
                          (set-union (log-free-vars dpre) (log-free-vars dpost))
                          dargs)]
              [avars   (remove-duplicates (proc-call-ref-vars as))]
              ; substitute actual for formal in specification
              [spre    (foldr subst-arg dpre  dargs as)]
              [spost   (foldr subst-arg dpost dargs as)]
              [sdec    (if (void? ddec)
                           (void)
                           (foldr a-subst-arg ddec dargs as))]
              ; partial correctness precondition
              [partial (make-quant 'exists evars
                         (log-and spre
                                  (log-quant 'forall avars (log-impl spost f))))])
              ; if both the caller and the callee are total, and the callee calls
              ; back the caller (maybe indirectly), check if the callee's variant
              ; strictly decreases
              (if (and (not (void? current-proc))
                       (not (void? sdec))
                       (calls? p s current-proc))
                  (log-and partial (log-< sdec global-decr))
                  partial))]

      [(annot g)
       (add! (make-obligation
               (get-meta c)
               'user-defined
               g
               f))
       g]

      [(dummy-po)
       (add! (make-obligation
               (get-meta c)
               'dummy
               f
               (log-const #t)))
       f]))

  ; body of weakest-precondition
  (let ([res (wp c postcondition)])
    (cons res obs)))

; Generating proof obligations for procedures

(: gen-obligations-proc (All (meta) (-> (Program meta)
                                        (With-meta meta (Def meta))
                                        (Listof (Proof-obligation meta)))))
(define (gen-obligations-proc p w)
  (match (get-data w)
    [(proc-def n a pre post b)
     (let ([obs (weakest-precondition p n b post)])
       (cons (make-obligation
               (get-meta w)
               'procedure-precondition
               pre
               (car obs))
             (cdr obs)))]
    [(proc*-def n a pre post v b)
     (let* ([obs  (weakest-precondition p n b post)])
       (cons (make-obligation
               (get-meta w)
               'procedure*-precondition
               (log-and pre
                        (log-= v global-decr))
               (log-and (car obs)
                        (log->= global-decr (a-const 0))))
             (cdr obs)))]))

; Generating proof obligations for entire programs

(: gen-obligations (All (meta) (-> (Program meta)
                                   (Listof (Proof-obligation meta)))))
(define (gen-obligations p)

  ; Explicit checks. I wanted this to be a separate procedure of type
  ; (All (meta) (-> (With-meta meta Check) (Proof-obligation meta)))
  ; but the type checker complains about map below (why?!) :(
  (: check->ob (-> (With-meta meta Check) (Proof-obligation meta)))
  (define (check->ob c)
    (match (get-data c)
      [(check g)
       (make-obligation
         (get-meta c)
         'check
         (log-const #t)
         (close-universally g))]))
  
  (append
    (map check->ob (program-checks p))
    (append-map (λ ([d : (With-meta meta (Def meta))])
                   (gen-obligations-proc p d))
                (program-defs p))
    (cdr (weakest-precondition p (void) (program-cmd p) (log-const #t)))))

; Adding axioms to obligations

(: with-axioms (All (meta) (-> (Listof (Proof-obligation meta))
                               (Listof Log-expr)
                               (Listof (Proof-obligation meta)))))
(define (with-axioms obs axs)
  (if (null? axs) obs
    (map (λ ([ob : (Proof-obligation meta)])
            (match ob
              [(proof-obligation m d f)
               (let ([new-f (log-op 'impl (append axs (list f)))])
                 (proof-obligation m d new-f))]))
         obs)))
