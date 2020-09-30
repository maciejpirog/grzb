(setq grzb-output-font-lock-keywords
      (let* (
            (x-headers '("--- proof obligation ---"))
            (x-errors '("--- error ---"))
           ;(x-counterexample '("Counterexample:"
           ;                    "Obligation cannot be discharged"
           ;                    "Parse error"
           ;                    "Z3 didn't manage to discharge obligation"))
            (x-keywords-regexp "^ok$")
            (x-headers-regexp (regexp-opt x-headers))
	    (x-errors-regexp (regexp-opt x-errors))
	    (x-list-regexp "^\\(* \\|=> \\)")
	   ;(x-counterexample-regexp (regexp-opt x-counterexample))
	    (x-ce-var-regexp "^\\([A-za-z0-9!@#$%^&*<>'-]*\\) -> ")
	   ;(x-ce-arr-regexp "^[A-za-z0-9!@#$%^&*<>'-]* \\(->\\) ")
            )

       `(
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-headers-regexp . font-lock-keyword-face)
          (,x-errors-regexp . font-lock-warning-face)
          (,x-list-regexp . font-lock-builtin-face)
	 ;(,x-counterexample-regexp . font-lock-variable-name-face)
         ;(,x-ce-var-regexp (1 font-lock-variable-name-face))
         ;(,x-ce-arr-regexp (1 font-lock-builtin-face))
          )))

(define-derived-mode grzb-output-mode compilation-mode ;; compilation-shell-minor-mode
  "grzb output mode"
  "Mode for colouring grzb output"

(setq font-lock-defaults '((grzb-output-font-lock-keywords))))
