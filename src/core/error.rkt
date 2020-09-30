#lang typed/racket

(provide (all-defined-out))

(struct (meta) user-error
  ([pos  : meta]
   [type : Symbol]
   [body : Any])
  #:transparent #:type-name User-error)

; Monadic errors

(struct (e) errors
  ([errs : (Listof e)])
  #:transparent #:type-name Errors)

(struct (a) success
  ([val : a])
  #:transparent #:type-name Success)

(define-type (Try e a)
  (U (Errors e) (Success a)))

(: raise-error (All (e a) (-> e (Try e a))))
(define (raise-error e)
  (errors (list e)))

(: get-errors (All (e a) (-> (Try e a) (Listof e))))
(define (get-errors t)
  (match t
    [(success a) null]
    [(errors xs) xs]))

(: return (All (e a) (-> a (Try e a))))
(define (return a)
  (success a))

(: bind (All (e a b) (-> (Try e a) (-> a (Try e b)) (Try e b))))
(define (bind m f)
  (match m
    [(success a) (f a)]
    [(errors xs) (errors xs)]))

(: combine (All (e a) (-> (Try e a) * (Try e (Listof a)))))
(define (combine . xs)
  (if (null? xs)
      (success null)
      (match (apply combine (rest xs))
        [(errors ys)
         (errors (append (get-errors (first xs)) ys))]
        [(success ys)
         (if (success? (first xs))
             (success (cons (success-val (first xs)) ys))
             (errors (errors-errs (first xs))))])))

(: combine2 (All (e a b d) (-> (Try e a) (Try e b) (-> a b (Try e d))
                               (Try e d))))
(define (combine2 a b f)
  (if (and (success? a) (success? b))
      (f (success-val a) (success-val b))
      (errors (append (get-errors a) (get-errors b)))))

(: combine3 (All (e a b c d) (-> (Try e a) (Try e b) (Try e c) (-> a b c (Try e d))
                                 (Try e d))))
(define (combine3 a b c f)
  (if (and (success? a) (success? b) (success? c))
      (f (success-val a) (success-val b) (success-val c))
      (errors (append (get-errors a) (get-errors b) (get-errors c)))))
        