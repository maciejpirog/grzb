#lang typed/racket

(require racket/cmdline)

(require "../logic/solver.rkt")
(require "run.rkt")

(: verbose-mode (Parameter Boolean))
(define verbose-mode (make-parameter #f))

(: version-mode (Parameter Boolean))
(define version-mode (make-parameter #f))

(: termination-mode (Parameter Boolean))
(define termination-mode (make-parameter #f))

(: var-mode (Parameter Var-mode))
(define var-mode (make-parameter 'integer))

; (: file-to-read (Listof String))
(define file-to-read
  (command-line
   #:program
   "grzb"

   #:usage-help
   "Verify the program in <input-file>"
   
   #:once-each
   [("--verbose" "-v") "Show all generated proof obligations"
                       (verbose-mode #t)]

   #:once-each
   [("--termination" "-t") "Make sure that the program terminates"
                           (termination-mode #t)]
   
   #:once-any
   [("--mode-integer") "Type of variables is Integer (default)"
                       (var-mode 'integer)]
   [("--mode-real")    "Type of variables is Real"
                       (var-mode 'real)]

   #:once-each
   [("--version" "-V") "Show version"
                       (version-mode #t)]

   #:args ([input-file false])
   
   input-file))

(if (version-mode)
    (printf "grzb version 0.1.1~n")
    (if file-to-read
        (run (var-mode) (termination-mode) (verbose-mode) file-to-read)
        (printf "grzb: expects an <input-file> on the command line~n")))
