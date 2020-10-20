#lang typed/racket

(require "../core/expr.rkt")
(require "../core/while.rkt")
(require "logic-internals.rkt")

(provide (struct-out proof-obligation) Proof-obligation gen-obligations with-axioms)

; Proof obligations

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
     'while*-postcondition 'while*-body-precondition 'while*-variant-nonnegative))
     

; Calculate the weakest precondition together with proof obligations

(: weakest-precondition (All (meta) (-> (Core meta) Log-expr
                                        (Pair Log-expr
                                              (Listof (Proof-obligation meta))))))
(define (weakest-precondition c postcondition)

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
                (log-and i (reify-bool b) (log-= v decr))
                (wp d (log-and i          (log-< v decr))))))
       (add! (make-obligation
              (get-meta c)
              'while*-variant-nonnegative
              (log-and i (reify-bool b))
              (log->= v (a-const 0))))
       i]

      [(if-stm b t e)
       (log-and (log-impl          (reify-bool b)  (wp t f))
                (log-impl (log-not (reify-bool b)) (wp e f)))]

      [(annot g)
       (add! (make-obligation
              (get-meta c)
              'user-defined
              g
              f))
       g]))

  ; body of weakest-precondition
  (let ([res (wp c postcondition)])
    (cons res obs)))



; Generating proof-obligations

(: gen-obligations (All (meta) (-> (Listof (With-meta meta Check))
                                   (Core meta)
                                   (Listof (Proof-obligation meta)))))
(define (gen-obligations cs c)

  ; Explicit checks (I wanted this to be a separate procedure
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
        (from-axiom g))]))
  
  (append
    (map check->ob cs)
    (cdr (weakest-precondition c (log-const #t)))))

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
