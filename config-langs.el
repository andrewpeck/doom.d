;;------------------------------------------------------------------------------
;; Mode aliases
;;------------------------------------------------------------------------------

;; enable syntax highlighting for vimrc files
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))   ; vimrc
(add-to-list 'auto-mode-alist '("\\.xdc\\'"          . vivado-mode))  ; tcl mode for xdc files
(add-to-list 'auto-mode-alist '("\\.ltx\\'"          . json-mode))    ; json mode for ltx files
(add-to-list 'auto-mode-alist '("\\.ino\\'"          . cpp-mode))     ; cpp mode for arduino files
(add-to-list 'auto-mode-alist '("\\.bb\\'"           . clojure-mode)) ; babashka

;;------------------------------------------------------------------------------
;; Awk
;;------------------------------------------------------------------------------

(add-hook! 'awk-mode-hook
  (setq-local  comment-start "# "))

;; redefine the awk checker to have no-ext enabled on the linter "--lint=no-ext"
(after! flycheck
  (flycheck-define-checker awk-gawk
    "GNU awk's built-in --lint checker."
    :command ("gawk"
              ;; Avoid code execution.  See https://github.com/w0rp/ale/pull/1411
              "--source" "'BEGIN{exit} END{exit 1}'"
              "-f" source
              "--lint=no-ext"
              "/dev/null")
    :standard-input nil
    :error-patterns
    ((warning line-start
              "gawk: "
              (file-name) ":" line ":" (optional column ":")
              (message (one-or-more not-newline)
                       (optional "\n"
                                 (one-or-more not-newline)
                                 " ^ "
                                 (one-or-more not-newline)))
              line-end))
    :error-filter flycheck-awk-gawk-error-filter
    :modes awk-mode))

;;------------------------------------------------------------------------------
;; Elisp
;;------------------------------------------------------------------------------

;;  set the tab width for emacs lisp mode to 4 for compatibility with emacs libs
(add-hook! 'emacs-lisp-mode-hook
  (setq-local tab-width 4))

;; remember ielm history
;; global copy of the buffer-local variable
(after! ielm
  (defvar ielm-comint-input-ring nil)

  (defun set-ielm-comint-input-ring ()
    ;; create a buffer-local binding of kill-buffer-hook
    (make-local-variable 'kill-buffer-hook)
    ;; save the value of comint-input-ring when this buffer is killed
    (add-hook 'kill-buffer-hook #'save-ielm-comint-input-ring)
    ;; restore saved value (if available)
    (when ielm-comint-input-ring
      (message "Restoring comint-input-ring...")
      (setq comint-input-ring ielm-comint-input-ring)))

  (defun save-ielm-comint-input-ring ()
    (message "Saving comint-input-ring...")
    (setq ielm-comint-input-ring comint-input-ring))

  (add-hook 'inferior-emacs-lisp-mode-hook #'set-ielm-comint-input-ring))

;;--------------------------------------------------------------------------------
;; Verilog
;;--------------------------------------------------------------------------------

(add-hook! 'verilog-mode-hook
  (setq-local comment-multi-line t))

(after! verilog
  (setq verilog-align-ifelse t
        verilog-auto-delete-trailing-whitespace t
        verilog-auto-inst-param-value t
        verilog-indent-lists nil ;; Fix the dumb indentation inside of port lists
        verilog-auto-inst-vector nil
        verilog-auto-lineup (quote all)
        verilog-auto-newline nil
        verilog-auto-save-policy nil
        verilog-auto-template-warn-unused t
        verilog-highlight-grouping-keywords t
        verilog-highlight-modules t
        verilog-case-indent 2
        verilog-cexp-indent 2
        verilog-indent-level 2
        verilog-indent-level-behavioral 2
        verilog-indent-level-declaration 2
        verilog-indent-level-module 2
        verilog-tab-to-comment nil)

(defun verilog-align-ports ()
  (interactive)
  (er/expand-region 2)
  (align-paren (region-beginning) (region-end)))


;;------------------------------------------------------------------------------
;; VHDL Mode
;;------------------------------------------------------------------------------

;; vhdl mode will wrap comments after some # of characters
(after! vhdl-mode
  (setq vhdl-end-comment-column 200
        vhdl-prompt-for-comments nil)


  (setq flycheck-ghdl-language-standard "08")

  (require 'er-basic-expansions)

  (defun vhdl-unsigned->slv ()
    "Convert a VHDL unsigned to standard logic vector."
    (interactive)
    (when (not (region-active-p))
      (er/mark-symbol))
    (let ((sig (buffer-substring-no-properties (mark) (point))))
      (delete-region (mark) (point))
      (insert (format  "std_logic_vector(%s)" sig))
      (backward-char 1))
    (when (functionp 'evil-insert)
      (evil-insert 0)))

  (defun vhdl-int->slv ()
    "Convert a VHDL integer to standard logic vector."
    (interactive)
    (when (not (region-active-p))
      (er/mark-symbol))
    (let ((sig (buffer-substring-no-properties (mark) (point))))
      (delete-region (mark) (point))
      (insert (format  "std_logic_vector(to_unsigned(%s, ))" sig))
      (backward-char 2))
    (when (functionp 'evil-insert)
      (evil-insert 0)))

  (defun vhdl-slv->int ()
    "Convert a VHDL standard logic vector to integer."
    (interactive)
    (when (not (region-active-p))
      (er/mark-symbol))
    (let ((sig (buffer-substring-no-properties (mark) (point))))
      (delete-region (mark) (point))
      (insert (format  "to_integer(unsigned(%s))" sig))))

  (defun vhdl-slv->unsigned ()
    "Convert a VHDL standard logic vector to unsigned."
    (interactive)
    (when (not (region-active-p))
      (er/mark-symbol))
    (let ((sig (buffer-substring-no-properties (mark) (point))))
      (delete-region (mark) (point))
      (insert (format  "unsigned(%s)" sig))
      (backward-char 1)))

  (defun vhdl-self-op (op)
    (let ((sym (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                   (symbol-at-point))))
      (save-excursion
        (when sym
          (forward-line)
          (open-line 1)
          (indent-for-tab-command)
          (insert (format "%s <= %s %s 1;" sym sym op))))))

  (defun vhdl-i++ ()
    "Insert a vhdl i++ for either the current selection or symbol at point."
    (interactive)
    (vhdl-self-op "+"))

  (defun vhdl-i-- ()
    "Insert a vhdl i-- for either the current selection or symbol at point."
    (interactive)
    (vhdl-self-op "-")))

;;------------------------------------------------------------------------------
;; Tcl
;;------------------------------------------------------------------------------

(add-hook! 'tcl-mode-hook
  (setq-local smartparens-mode t
              auto-fill-mode nil))

;; (dolist (key vivado-builtin-list)
;;   (add-to-list 'tcl-builtin-list key))
;; (dolist (key vivado-keyword-list)
;;   (add-to-list 'tcl-keyword-list key))
;; (dolist (key vivado-constant-list)
;;   (add-to-list 'tcl-constant-list key))

;; make $ not part of a symbol in tcl-mode
(after! tcl
  (setq tcl-help-directory-list '("/usr/share/doc/tclx"))
  (add-hook! 'tcl-mode-hook (setq-local smartparens-mode t))
  (modify-syntax-entry ?$ "'" tcl-mode-syntax-table))

;;------------------------------------------------------------------------------
;; Common Lisp
;;------------------------------------------------------------------------------

(after! slime
  (setq inferior-lisp-program "sbcl")
  (setq org-babel-lisp-eval-fn 'slime-eval))

;;------------------------------------------------------------------------------
;; Clojure
;;------------------------------------------------------------------------------

(use-package! flycheck-clj-kondo :defer-incrementally t)

;; cider-edit-jack-in-command
(setq org-babel-clojure-backend "cider")
(setq cider-save-file-on-load t)

;;-----------------------------------------------------------------------------------------
;; Markdown
;;------------------------------------------------------------------------------

;; Make Mode GFM by default
;; (after! gfm
;;   (setq initial-major-mode 'gfm-mode))

;;-----------------------------------------------------------------------------------------
;; C mode
;;-----------------------------------------------------------------------------------------

;; For example, if you prefer double slashes // instead of slash-stars /* ... */
;; in c-mode, insert below code into your ~/.emacs:
(add-hook! 'c-mode-common-hook
           ;; Preferred comment style
           (setq comment-start "// " comment-end ""))

;;------------------------------------------------------------------------------
;; SCAD
;;------------------------------------------------------------------------------

(add-hook! 'scad-mode-hook
  (add-hook 'write-contents-functions
            #'re-indent-buffer nil t))

;;------------------------------------------------------------------------------
;; Emacs
;;------------------------------------------------------------------------------

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (add-hook 'write-contents-functions
;;                       #'re-indent-buffer nil t)))

;;-----------------------------------------------------------------------------------------
;; XML
;;------------------------------------------------------------------------------

(after! nxml
  (setq nxml-child-indent 2
        nxml-attribute-indent 2)
  (add-hook! 'nxml-mode-hook
    (visual-fill-column-mode -1))
  (defun nxml-pretty-format ()
    (interactive)
    (save-excursion
      (shell-command-on-region (point-min) (point-max)
                               "xmllint --format -" (buffer-name) t)
      (nxml-mode)
      (indent-region begin end))))

;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

(setq python-shell--interpreter "python3")
