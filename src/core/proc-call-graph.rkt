#lang typed/racket

(require "while.rkt")

; Calculate if a procudere f calls a procedure g

(provide calls?)

(define-type Graph (Listof (Pairof Symbol (Listof Symbol))))

(: called-procs (All (meta) (-> (Core meta) (Listof Symbol))))
(define (called-procs c)
  (match (get-data c)
      [(comp l r) (set-union (called-procs l) (called-procs r))]
      [(while i b d) (called-procs d)]
      [(while* i v b d) (called-procs d)]
      [(if-stm b t e) (set-union (called-procs t) (called-procs e))]
      [(proc-call s as) (list s)]
      [_  null]))

(: make-graph (All (meta) (-> (Program meta) Graph)))
(define (make-graph p)

  (: make-edges (-> (With-meta meta (Def meta)) (Pairof Symbol (Listof Symbol))))
  (define (make-edges w)
    (cons (def-name (get-data w))
          (called-procs (def-body (get-data w)))))
  
  (map make-edges (program-defs p)))

(: calls? (All (meta) (-> (Program meta) Symbol Symbol Boolean)))
(define (calls? p f g)

  (: graph Graph)
  (define graph (make-graph p))

  (: visited (Listof Symbol))
  (define visited null)

  (: dfs (-> Symbol Boolean))
  (define (dfs current)
    (cond
      [(eq? current g) #t]
      [(member current visited) #f]
      [else (set! visited (cons current visited))
            (ormap dfs
                   (let ([r (assoc current graph)])
                     (if r r (error "Unknown procedure" current))))]))

  (dfs f))
        