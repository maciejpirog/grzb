#lang typed/racket

(require "../core/expr.rkt")
(require "../core/while.rkt")
(require "logic-internals.rkt")

(provide (struct-out proof-obligation) Proof-obligation gen-obligations with-axioms)

; Proof obligations

(struct (meta) proof-obligation
  ([meta : meta] [desc : Symbol] [f : Log-expr])
  #:transparent #:type-name Proof-obligation)

(: make-obligation (All (meta) (-> meta Symbol Log-expr Log-expr (proof-obligation meta))))
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

; Generating proof-obligations
(: gen-obligations (All (meta) (-> (Core meta) (Listof (proof-obligation meta)))))
(define (gen-obligations c)
  
  ; Last postcondition
  (: f Log-expr)
  (define f (log-const true))

  ; Main loop
  (: go (All (meta) (-> (Core meta) (Listof (proof-obligation meta)))))
  (define (go c)
    (match (core-data c)

      [(skip)
       null]

      [(comp l r)
       (let* ([robs (go r)]
              [lobs (go l)])
         (append lobs robs))]

      [(assign x e)
       (set! f (subst x e f))
       null]

      [(while i b e)
       (let* ([post (make-obligation
                      (core-meta c)
                      'while-postcondition
                      (log-and i (log-not (reify-bool b)))
                      f)]
              [dobs (begin (set! f i)
                           (go e))]
              [dpre (make-obligation
                      (core-meta c)
                      'while-body-precondition
                      (log-and i (reify-bool b))
                      f)])
         (set! f i)
         (list* post dpre dobs))]

      [(while* i d b e)
       (let* ([n (gensym 'decr)]
              [post (make-obligation
                      (core-meta c)
                      'while*-postcondition
                      (log-and i (log-not (reify-bool b)))
                      f)]
              [dobs (begin (set! f (log-and i (log-< d (a-var n))))
                           (go e))]
              [dpre (make-obligation
                      (core-meta c)
                      'while*-body-precondition
                      (log-and i (reify-bool b) (log-= d (a-var n)))
                      f)]
              [nneg (make-obligation
                    (core-meta c)
                    'while*-variant-nonnegative
                    (log-and i (reify-bool b))
                    (log->= d (a-const 0)))])
         (set! f i)
         (list* post dpre nneg dobs))]

      [(if-stm b t e)
       (let* ([old-f f]
              [tobs  (go t)]
              [t-f   f]
              [fobs  (begin (set! f old-f)
                            (go e))])
         (set! f (log-and (log-impl (reify-bool b) t-f)
                          (log-impl (log-not (reify-bool b)) f)))
         (append tobs fobs))]

      [(annot g)
       (let ([ob (make-obligation
                   (core-meta c)
                   'user-defined
                   g
                   f)])
         (set! f g)
         (list ob))]

      [(axiom f)
       null]

      [(check f)
       (list (proof-obligation (core-meta c)
                               'check
                               (from-axiom f)))]))
                                

  ; Invoke main loop
  (go c))

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

; Test

(: po-pretty-print (All (meta) (-> (Proof-obligation meta) Any)))
(define (po-pretty-print po)
  (log-pretty-print (proof-obligation-f po)))

(: pos-pretty-print (All (meta) (-> (Listof (Proof-obligation meta)) (Listof Any))))
(define (pos-pretty-print pos)
  (map (lambda ([x : (Proof-obligation meta)]) (po-pretty-print x)) pos))
