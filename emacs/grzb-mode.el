

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq while-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("begin" "if" "while" "assert" "axiom"))
            (x-operators '(":="))
	    
            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            ;;(x-types-regexp "{[^}]*}")
	    (x-types-regexp "{\\([^}]*\\)}")
	    (x-types2-regexp "([ ]*\\(assert\\|while\\|axiom\\)[ ]*\\([A-za-z0-9!@#$%^&*<>'-]*\\)")
	    (x-varname-regexp "(\\([a-zA-Z0-9'_-]*\\) :=")
            (x-comment-regexp ";.*")
            (x-operators-regexp (regexp-opt x-operators))
        )

        `(
	  (,x-comment-regexp . font-lock-comment-face)
	  (,x-keywords-regexp . font-lock-keyword-face)
          ;;(,x-types-regexp . font-lock-doc-face)
	  (,x-types-regexp (0 font-lock-doc-face))
	  (,x-types2-regexp (2 font-lock-doc-face))
	  (,x-varname-regexp (1 font-lock-variable-name-face))
          ;;(,x-constants-regexp . font-lock-type-face)
	  
          (,x-operators-regexp . font-lock-builtin-face)
	  ;;,x-functions-regexp . (1 font-lock-function-name-face))
          
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))


;; variable mode

;(make-local-variable 'var-mode)
(setq var-mode 'integer)

;;;###autoload
(define-derived-mode grzb-mode prog-mode "grzb mode"
  "Major mode for editing While files to be used in grzb"

;; code for syntax highlighting
(setq font-lock-defaults '((while-font-lock-keywords))))

;; evaluating buffer
(defcustom grzb-path-to-bin nil
  "Path to the grzb interpreter's directory"
  :group 'grzb :type 'directory)

(defun grzb-eval-buffer (&optional BUFFER)
  "Verify the current buffer's file"
  (interactive)
  (save-buffer)
  (let ((grzb-command
         (if (or (null grzb-path-to-bin)
                 (string= grzb-path-to-bin ""))
             (locate-file "grzb" exec-path)
           (locate-file "grzb" (list grzb-path-to-bin))))
        (file-name
         (buffer-file-name BUFFER)))
    (when (null grzb-command)
      (error "grzb executable not found! Make sure it is in exec-path or that `grzb-path-to-bin' is set appropriately."))
    (let ((olddir default-directory)
	  (vmode (if (eq var-mode 'integer) "--mode-integer"
		   (if (eq var-mode 'real) "--mode-real" "--help"))))
      (cd (substring grzb-command 0 -5))
      (compile (concat grzb-command " " vmode " " file-name) 'grzb-output-mode)
      (minibuffer-message olddir)
      (cd olddir))))

(defun grzb-verbose-eval-buffer (&optional BUFFER)
  "Verify the current buffer's file in the verbose mode"
  (interactive)
  (save-buffer)
  (let ((grzb-command
         (if (or (null grzb-path-to-bin)
                 (string= grzb-path-to-bin ""))
             (locate-file "grzb" exec-path)
           (locate-file "grzb" (list grzb-path-to-bin))))
        (file-name
         (buffer-file-name BUFFER)))
    (when (null grzb-command)
      (error "grzb executable not found! Make sure it is in exec-path or that `grzb-path-to-bin' is set appropriately."))
    (let ((olddir default-directory)
	  (vmode (if (eq var-mode 'integer) "--mode-integer"
		   (if (eq var-mode 'real) "--mode-real" "--help"))))
      (cd (substring grzb-command 0 -5))
      (compile (concat grzb-command " -v " vmode " "file-name) 'grzb-output-mode)
      (minibuffer-message olddir)
      (cd olddir))))

(setq grzb-mode-map (make-sparse-keymap))
(define-key grzb-mode-map "\C-c\C-c" 'grzb-eval-buffer)
(define-key grzb-mode-map "\C-c\C-p" 'grzb-verbose-eval-buffer)

;; menus
(define-key-after
  global-map
  [menu-bar mymenu]
  (cons "grzb" (make-sparse-keymap "grzb mode"))
  'tools )

;; var mode menu

(defun grzb-var-mode-integer ()
  "Set variable mode to `integer'."
  (interactive)
  (setq var-mode 'integer))
					;(bubbles))

(defun grzb-var-mode-real ()
  "Set variable mode to `real'."
  (interactive)
  (setq var-mode 'real))
  ;(bubbles))

(define-key global-map
  [menu-bar mymenu var-mode-real];menu [grzb-set-var-mode-real]
      (list 'menu-item "Real Mode" 'grzb-var-mode-real
            :button '(:radio . (eq var-mode 'real))))
(define-key global-map
  [menu-bar mymenu var-mode-integer] ;menu [grzb-set-var-mode-integer]
      (list 'menu-item "Integer Mode" 'grzb-var-mode-integer
            :button '(:radio . (eq var-mode 'integer))))

(define-key
  global-map
  [menu-bar mymenu sep]
  '("--single-line"))

;; berify menu

;; Creating a menu item, under the menu by the id “[menu-bar mymenu]”

(define-key
  global-map
  [menu-bar mymenu vver]
  '("Show Proof Obligations and Verify" . grzb-verbose-eval-buffer))

(define-key
  global-map
  [menu-bar mymenu ver]
  '("Verify" . grzb-eval-buffer))

;; 

(add-to-list 'auto-mode-alist '("\\.while\\'" . grzb-mode))

;; add the mode to the `features' list
(provide 'grzb-mode)
