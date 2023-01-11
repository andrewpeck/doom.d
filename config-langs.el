;;------------------------------------------------------------------------------
;; Awk
;;------------------------------------------------------------------------------

(add-hook 'awk-mode-hook (lambda ()
                           (progn (make-variable-buffer-local 'comment-start)
                                  (setq comment-start "# "))))

;; redefine the awk checker to have no-ext enabled on the linter "--lint=no-ext"
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
  :modes awk-mode)

;;------------------------------------------------------------------------------
;; Elisp
;;------------------------------------------------------------------------------

;;  set the tab width for emacs lisp mode to 4 for compatibility with emacs libs
(add-hook! emacs-lisp-mode-hook
  (progn (make-variable-buffer-local tab-width)
         (setq tab-width 4)))

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

;;------------------------------------------------------------------------------
;; VHDL
;;------------------------------------------------------------------------------

(defun vhdl-slv->unsigned ()
  (interactive)
  (er/mark-symbol)
  (let ((sig (buffer-substring-no-properties (mark) (point))))
    (delete-region (mark) (point))
    (insert (format  "unsigned(%s)" sig))
    (backward-char 1)))

(defun vhdl-unsigned->slv ()
  (interactive)
  (er/mark-symbol)
  (let ((sig (buffer-substring-no-properties (mark) (point))))
    (delete-region (mark) (point))
    (insert (format  "std_logic_vector(%s)" sig))
    (backward-char 1)))

(defun vhdl-int->slv ()
  (interactive)
  (er/mark-symbol)
  (let ((sig (buffer-substring-no-properties (mark) (point))))
    (delete-region (mark) (point))
    (insert (format  "std_logic_vector(to_unsigned(%s, ))" sig))
    (backward-char 2)))

(defun vhdl-slv->int ()
  (interactive)
  (er/mark-symbol)
  (let ((sig (buffer-substring-no-properties (mark) (point))))
    (delete-region (mark) (point))
    (insert (format  "to_integer(unsigned(%s))" sig))))

;;--------------------------------------------------------------------------------
;; Verilog
;;--------------------------------------------------------------------------------

(setq
 verilog-align-ifelse t
 verilog-auto-delete-trailing-whitespace t
 verilog-auto-inst-param-value t
 verilog-auto-inst-vector nil
 verilog-auto-lineup (quote all)
 verilog-auto-newline nil
 verilog-auto-save-policy nil
 verilog-auto-template-warn-unused t
 verilog-case-indent 3
 verilog-cexp-indent 3
 verilog-highlight-grouping-keywords t
 verilog-highlight-modules t
 verilog-indent-level 3
 verilog-indent-level-behavioral 3
 verilog-indent-level-declaration 3
 verilog-indent-level-module 3
 verilog-tab-to-comment t)

;;------------------------------------------------------------------------------
;; Tcl
;;------------------------------------------------------------------------------

;; make $ not part of a symbol in tcl-mode
(after! tcl
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
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Preferred comment style
            (setq comment-start "// " comment-end "")))
;;------------------------------------------------------------------------------
;; SCAD
;;------------------------------------------------------------------------------

(add-hook 'scad-mode-hook
          (lambda ()
            (add-hook 'write-contents-functions
                      #'re-indent-buffer nil t)))

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
  (add-hook
   'nxml-mode-hook
   (setq nxml-child-indent 2 nxml-attribute-indent 2)))

;; (add-hook 'nxml-mode-hook (lambda () (visual-fill-column-mode -1)))
;; (defun nxml-pretty-format ()
;;   (interactive)
;;   (save-excursion
;;     (shell-command-on-region (point-min) (point-max)
;;     "xmllint --format -" (buffer-name) t)
;;     (nxml-mode)
;;     (indent-region begin end)))

;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

(setq python-shell--interpreter  "python3")

;;------------------------------------------------------------------------------
;; VHDL Mode
;;------------------------------------------------------------------------------

;; vhdl mode will wrap comments after some # of characters
(after! vhdl
  (setq vhdl-end-comment-column 200
        vhdl-prompt-for-comments nil
        auto-fill-mode nil))
