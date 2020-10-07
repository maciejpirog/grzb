(setq grzb-output-font-lock-keywords
      (let* (
            (x-headers '("--- proof obligation ---" " grzb "))
            (x-errors '("--- error ---"))
            (x-keywords-regexp "^ok$")
            (x-headers-regexp (regexp-opt x-headers))
	    (x-errors-regexp (regexp-opt x-errors))
	    (x-list-regexp "^\\(* \\|=> \\)")
	    (x-ce-var-regexp "^\\([A-za-z0-9!@#$%^&*<>'-]*\\) -> ")
            )

       `(
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-headers-regexp . font-lock-keyword-face)
          (,x-errors-regexp . font-lock-warning-face)
          (,x-list-regexp . font-lock-builtin-face)
          )))

(define-derived-mode grzb-output-mode compilation-mode
  "grzb output mode"
  "Mode for colouring grzb output"

(setq font-lock-defaults '((grzb-output-font-lock-keywords))))
