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
