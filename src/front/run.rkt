#lang typed/racket

(require "../utils/error.rkt")
(require "../utils/parse-utils.rkt")
(require "../logic.rkt")
(require "../logic/proof-obligations.rkt")
(require "../logic/solver.rkt")
(require "../core/while.rkt")
(require "../core/parse-prog.rkt")

(provide run)

(define-type Exit-code
  (U 'file-error 'malformed-program 'logical-error))

(: exit-grzb (-> Exit-code Any))
(define (exit-grzb c)
  (exit (match c
          ['logical-error     1]
          ['malformed-program 2]
          ['file-error        3])))

(: run (-> Var-mode Boolean Any Any))
(define (run var-mode verbose-mode filename)

  (define (fnf)
    (printf "Error: Can't open file ~v~n" filename)
    (exit-grzb 'file-error))

  (: read-syntax* (-> Any Input-Port (Listof (Syntaxof Any))))
  (define (read-syntax* filename in)
    (let ([s (read-syntax filename in)])
      (if (eof-object? s) null
          (cons s (read-syntax* filename in)))))

  (with-handlers ([exn:fail:filesystem:errno? (λ (x) (fnf))]
                  [exn:fail:read? (λ ([x : exn:fail:read])
                                     (print-errors (make-read-errors x))
                                     (exit-grzb 'malformed-program))])
    (if (string? filename)
        (let* ([in (open-input-file (string->path filename))]
               [_  (port-count-lines! in)]
               [ss (read-syntax* filename in)]
               [_  (close-input-port in)])
          (if (null? ss)
              (begin (printf "Error: Missing program statement in ~v~n" filename)
                     (exit-grzb 'malformed-program))
              (match (parse-program ss)
                [(errors es) (print-errors es)
                             (exit-grzb 'malformed-program)]
                [(success p)
                 (let*-values
                   ([(obs)    (gen-obligations p)]
                    [(obsa)   (with-axioms obs (list-axioms p))]
                    [(_)      (if verbose-mode (print-obs obsa) false)]
                    [(r)      (discharge* var-mode obsa)])
                   (if (null? r)
                       (printf "ok~n")
                       (begin (print-errors r)
                              (exit-grzb 'logical-error))))])))
        (begin (printf "Error: Malformed file name~n")
               (exit-grzb 'file-error)))))

(define-type Proof-obligation-pos
  (Proof-obligation Pos))

(define-predicate proof-obligation-pos? Proof-obligation-pos)

(: print-ob (-> (Proof-obligation Pos) Void))
(define (print-ob ob)

  (define (desc s)
    (match s
      ['user-defined               "Explicit assertion"]
      ['while-postcondition        "Postcondition of while loop"]
      ['while-body-precondition    "Precondition of while loop body"]
      ['while*-postcondition       "Postcondition of while* loop"]
      ['while*-body-precondition   "Precondition of while* loop body"]
      ['while*-variant-nonnegative "Variant of while* nonnegative"]
      ['procedure-precondition     "Procedure precondition"]
      ['check                      "Explicit check"]))
  
  (match ob
    [(proof-obligation m d f)
     (printf "~a at (~a:~a)~n" (desc d) (pos-line m) (pos-col m))
     (print-formula f)]))
         
(: print-obs (-> (Listof (Proof-obligation Pos)) Void))
(define (print-obs obs)
  (if (null? obs)
      (void)
      (match (first obs)
        [(proof-obligation m d f)
         (printf "--- proof obligation ---~n")
         (print-ob (first obs))
         (printf "~n")
         (print-obs (rest obs))])))

(: print-errors (-> (Listof (User-error Pos)) Void))
(define (print-errors xs)
  (if (null? xs)
      (printf "")
      (match (first xs)
        [(user-error m d f)
         (printf "--- error ---~n")
         (match d
           ['parse-error
            (printf "Parse error at (~a:~a)~n" (pos-line m) (pos-col m))
            (printf "~a~n" f)]
           ['smt-dunno
            (printf "Z3 didn't manage to discharge obligation~n")
            (if (proof-obligation-pos? f)
                (print-ob f)
                false)]
           ['smt-counterexample
            (printf "Obligation cannot be discharged:~n")
            (if (and (pair? f) (proof-obligation-pos? (car f)) (string? (cdr f)))
                (begin
                  (print-ob (car f))
                  (printf "Counterexample:~n~a" (cdr f)))
                false)])
         (printf "~n")
         (print-errors (rest xs))])))

(: make-read-errors (-> exn:fail:read (Listof (User-error Pos))))
(define (make-read-errors r)
  (let ([srclocs (exn:fail:read-srclocs r)])
    (list
     (user-error
      (if (and (pair? srclocs) (srcloc? (first srclocs)))
          (pos "" (srcloc-line   (first srclocs))
                  (srcloc-column (first srclocs)))
          (pos "" #f #f))
      'parse-error
      (exn-message r)))))
