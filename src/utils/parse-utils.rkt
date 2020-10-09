#lang typed/racket

(require "error.rkt")

(provide (struct-out pos) Pos get-pos Parse-monad ouch!)

; Position

(struct pos
  ([file : Any]
   [line : Any]
   [col  : Any])
  #:transparent #:type-name Pos)

(: get-pos (-> (Syntaxof Any) Pos))
(define (get-pos s)
  (pos (syntax-source s)
       (syntax-line s)
       (syntax-column s)))

; Parse monad

(define-type (Parse-monad a)
  (Try (User-error Pos) a))

(: ouch! (All (a b) (-> (Syntaxof Any) String * (Parse-monad a))))
(define (ouch! p . xs)
  (raise-error
    (user-error (get-pos p) 'parse-error (apply string-append xs))))
