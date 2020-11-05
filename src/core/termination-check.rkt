#lang typed/racket

(require "while.rkt")
(require "../utils/error.rkt")

(provide terminates)

; Check if a given program always terminate, i.e.,
; it uses "define*" and "while*", but it doesn't use "define" or "while"

(: err (All (meta) (-> meta String (Try (User-error meta) Void))))
(define (err m s)
  (raise-error (user-error m 'non-terminating s)))

(: core-terminates (All (meta) (-> (Core meta) (Try (User-error meta) Void))))
(define (core-terminates c)
  (match (get-data c)
    [(comp l r)        (combine2 (core-terminates l) (core-terminates r)
                                 (λ ([x : Void] [y : Void]) (return (void))))]
    [(while i b d)     (err (get-meta c) "Try \"while*\" instead of \"while\"")]
    [(while* i v b d)  (core-terminates d)]
    [(if-stm b t e)    (combine2 (core-terminates t) (core-terminates e)
                                 (λ ([x : Void] [y : Void]) (return (void))))]
    [_                 (return (void))]))

(: prog-terminates (All (meta) (-> (Program meta) (Try (User-error meta) (Listof Void)))))
(define (prog-terminates p)
  (apply combine
           (map (λ ([d : (With-meta meta (Def meta))])
                   (match (get-data d)
                     [(proc*-def n a p pp v b) (core-terminates b)]
                     [_ (err (get-meta d) "Try \"define*\" instead of \"define\"")]))
           (program-defs p))))

(: terminates (All (meta) (-> Boolean (Program meta) (Try (User-error meta) Void))))
(define (terminates b p)
  (if b (bind (prog-terminates p)
              (λ ([x : (Listof Void)])
                 (return (void))))
        (return (void))))