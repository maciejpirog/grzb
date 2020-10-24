

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq while-font-lock-keywords
      (let* (
            ;; define several category of keywords
            ;(x-keywords '("begin" "skip" "if" "while" "while\\*" "assert" "axiom"))
            (x-operators '(":="))
	    
            ;; generate regex string for each category of keywords
	    ;(x-keywords-regexp (regexp-opt x-keywords 'words))
	    (x-keywords-regexp "([ ]*\\(define\\|begin\\|skip\\|if\\|while\\*\\|while\\|assert\\|axiom\\|check\\|dummy-po\\)\\( \\|$\\|)\\)")
	    ;;(x-types-regexp "{[^}]*}")
	    (x-types-regexp "{\\([^}]*\\)}")
	    (x-types2-regexp "([ ]*\\(assert\\|while\\*\\|while\\|axiom\\|check\\)[ ]+\\([A-za-z0-9!@#$%^&*<>'-]*\\)")
	    (x-varname-regexp "(\\([a-zA-Z0-9'_-]*\\) :=")
	    (x-varname2-regexp "((\\([a-zA-Z0-9'_-]*\\) . ")
	    (x-fundef-regexp "(define[ ]+(\\([A-za-z0-9!@#$%^&*<>'=+-]+\\)")
	    (x-funcall-convention-regexp "([ ]*\\(ref \\|val \\)")
            (x-comment-regexp ";.*")
            (x-operators-regexp (regexp-opt x-operators))
        )

        `(
	  (,x-comment-regexp . font-lock-comment-face)
					;(,x-keywords-regexp . font-lock-keyword-face)
	  (,x-keywords-regexp (1 font-lock-keyword-face))
          ;;(,x-types-regexp . font-lock-doc-face)
	  (,x-types-regexp (0 font-lock-string-face))
	  (,x-types2-regexp (2 font-lock-string-face))
	  (,x-varname-regexp (1 font-lock-variable-name-face))
	  (,x-varname2-regexp (1 font-lock-variable-name-face))
          (,x-funcall-convention-regexp (1 font-lock-builtin-face))
	  (,x-fundef-regexp (1 font-lock-function-name-face))
	 
          ;;(,x-constants-regexp . font-lock-type-face)
	  
          (,x-operators-regexp . font-lock-builtin-face)
	  ;;,x-functions-regexp . (1 font-lock-function-name-face))
          
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))


;; variable mode

;(make-local-variable 'var-mode)
(setq var-mode 'integer)

(defun grzb-var-mode-integer ()
  "Set variable mode to `integer'."
  (interactive)
  (setq var-mode 'integer))

(defun grzb-var-mode-real ()
  "Set variable mode to `real'."
  (interactive)
  (setq var-mode 'real))

;; evaluating buffer
(defcustom grzb-path-to-bin nil
  "Path to the grzb interpreter's directory"
  :group 'grzb :type 'directory)

(defun grzb-verify-buffer (&optional BUFFER)
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

(defun grzb-verbose-verify-buffer (&optional BUFFER)
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

(defun grzb-docker-verify-buffer (&optional BUFFER)
  "Fetch grzb from Docker and Verify the current buffer's file"
  (interactive)
  (save-buffer)
  (let ((grzb-command (concat "docker run --rm -t -v "
			      (file-name-directory (buffer-file-name BUFFER))
			      ":/home maciejpirog/grzb:latest"))
	(file-name
         (file-name-nondirectory (buffer-file-name BUFFER))))
    (when (null grzb-command)
      (error "grzb executable not found! Make sure it is in exec-path or that `grzb-path-to-bin' is set appropriately."))
    (let ((vmode (if (eq var-mode 'integer) "--mode-integer"
		   (if (eq var-mode 'real) "--mode-real" "--help"))))
      (compile (concat grzb-command " " vmode " " file-name) 'grzb-output-mode)
      )))

(defun grzb-docker-verbose-verify-buffer (&optional BUFFER)
  "Fetch grzb from Docker and Verify the current buffer's file"
  (interactive)
  (save-buffer)
  (let ((grzb-command (concat "docker run --rm -t -v "
			      (file-name-directory (buffer-file-name BUFFER))
			      ":/home maciejpirog/grzb:latest"))
	(file-name
         (file-name-nondirectory (buffer-file-name BUFFER))))
    (when (null grzb-command)
      (error "grzb executable not found! Make sure it is in exec-path or that `grzb-path-to-bin' is set appropriately."))
    (let ((vmode (if (eq var-mode 'integer) "--mode-integer"
		   (if (eq var-mode 'real) "--mode-real" "--help"))))
      (compile (concat grzb-command " -v " vmode " " file-name) 'grzb-output-mode)
      )))

(defvar grzb-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'grzb-verify-buffer)
    (define-key map "\C-c\C-p" #'grzb-verbose-verify-buffer)
    (define-key map "\C-c\C-d\C-c" #'grzb-docker-verify-buffer)
    (define-key map "\C-c\C-d\C-p" #'grzb-docker-verbose-verify-buffer)
    (define-key map [menu-bar grzb-mode] (cons "grzb" menu-map))

    (define-key menu-map [grzbrm]
      (list 'menu-item "Real Mode" 'grzb-var-mode-real
            :button '(:radio . (eq var-mode 'real))))
    
    (define-key menu-map [grzbim]
      (list 'menu-item "Integer Mode" 'grzb-var-mode-integer
            :button '(:radio . (eq var-mode 'integer))))

    (define-key menu-map [grzbs2] '("--single-line"))

    (define-key menu-map [grzbdvv]
      '(menu-item "Show POs and Verify Using Docker Image" grzb-docker-verbose-verify-buffer
                  :help "Send the file to Grzb for verification and show the proof obligations"))
    
    (define-key menu-map [grzbdv]
      '(menu-item "Verify Using Docker Image" grzb-docker-verify-buffer
                  :help "Send the file to Grzb for verification"))

    (define-key menu-map [grzbs1] '("--single-line"))

    (define-key menu-map [grzbvv]
      '(menu-item "Show POs and Verify" grzb-verbose-verify-buffer
                  :help "Send the file to Grzb for verification and show the proof obligations"))

    (define-key menu-map [grzbv]
      '(menu-item "Verify" grzb-verify-buffer
                  :help "Send the file to Grzb for verification"))

    map))

;;;###autoload
(define-derived-mode grzb-mode prog-mode "grzb mode"
  "Major mode for editing While files to be used in grzb
\\{grzb-mode-map}
"
;; code for syntax highlighting
(setq font-lock-defaults '((while-font-lock-keywords))))

;; 

(add-to-list 'auto-mode-alist '("\\.imp\\'" . grzb-mode))

;; add the mode to the `features' list
(provide 'grzb-mode)
