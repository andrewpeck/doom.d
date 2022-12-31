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
