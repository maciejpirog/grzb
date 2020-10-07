#lang typed/racket

(require "../core.rkt")

(provide run)

(: run (-> Var-mode Boolean Any (U False Void)))
(define (run var-mode verbose-mode filename)

  (define (fnf)
    (printf "Error: Can't open file ~v~n" filename))

  (: read-syntax* (-> Any Input-Port (Listof (Syntaxof Any))))
  (define (read-syntax* filename in)
    (let ([s (read-syntax filename in)])
      (if (eof-object? s) null
          (cons s (read-syntax* filename in)))))

  (: my-map (All (meta) (-> (-> (Core meta) (Listof Log-expr))
                            (Listof (Core meta))
                            (Listof (Listof Log-expr)))))
  (define (my-map f xs) (map f xs)) ; no idea why type checker needs this :(
  
  (with-handlers ([exn:fail:filesystem:errno? (λ (x) (fnf))]
                  [exn:fail:read? (λ ([x : exn:fail:read])
                                     (print-errors (make-read-errors x)))])
    (if (string? filename)
        (let* ([in (open-input-file (string->path filename))]
               [_  (port-count-lines! in)]
               [ss (read-syntax* filename in)]
               [_  (close-input-port in)])
          (if (null? ss)
              (printf "Error: Missing program statement in ~v~n" filename)
              (match (parse-program ss)
                [(errors es) (print-errors es)]
                [(success ps)
                 (let*-values
                   ([(defs p) (split-at-right ps 1)]
                    [(obs)    (gen-obligations (car p))]
                    [(obsa)   (with-axioms obs (append* (my-map list-axioms ps)))]
                    [(_)      (if verbose-mode (print-obs obsa) false)]
                    [(r)      (discharge* var-mode obsa)])
                   (if (null? r)
                       (printf "ok~n")
                       (print-errors r)))])))
        (printf "Error: Malformed file name~n"))))

(define-type Proof-obligation-pos
  (Proof-obligation Pos))

(define-predicate proof-obligation-pos? Proof-obligation-pos)

(: print-ob (-> (Proof-obligation Pos) False))
(define (print-ob ob)

  (define (desc s)
    (match s
      ['user-defined               "Explicit assertion"]
      ['while-postcondition        "Postcondition of while loop"]
      ['while-body-precondition    "Precondition of while loop body"]
      ['while*-postcondition       "Postcondition of while* loop"]
      ['while*-body-precondition   "Precondition of while* loop body"]
      ['while*-variant-nonnegative "Variant of while* nonnegative"]))
  
  (match ob
    [(proof-obligation m d f)
     (printf "~a at (~a:~a)~n" (desc d) (pos-line m) (pos-col m))
     (match (extract-assumptions f)
       [(cons xs g)
        (map (λ ([h : Log-expr]) (printf "* ~a~n" (log-pretty-print h)))
             xs)
        (map (λ ([h : Log-expr]) (printf "=> ~a~n" (log-pretty-print h)))
             (split-and g))])
     false]))
         
(: print-obs (-> (Listof (Proof-obligation Pos)) False))
(define (print-obs obs)
  (if (null? obs)
      false
      (match (first obs)
        [(proof-obligation m d f)
         (printf "--- proof obligation ---~n")
         (print-ob (first obs))
         (printf "~n")
         (print-obs (rest obs))])))

(: print-errors (-> (Listof (User-error Pos)) (U False Void)))
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
