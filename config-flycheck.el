;;; ../Sync/dotfiles/doom.d/config-flycheck.el -*- lexical-binding: t; -*-

(after! flycheck

  ;;------------------------------------------------------------------------------
  ;; Prose lint
  ;; https://unconj.ca/blog/linting-prose-in-emacs.html
  ;;------------------------------------------------------------------------------
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint"
              ;;
              ;;            (option-flag "--external-sources" flycheck-shellcheck-follow-sources)
              "--config" (eval (expand-file-name "~/.doom.d/proselint.rc"))
              source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message) line-end))
    :modes (text-mode latex-mode markdown-mode gfm-mode org-mode))

  (add-to-list 'flycheck-checkers 'proselint)

  ;;------------------------------------------------------------------------------
  ;; 1
  ;;------------------------------------------------------------------------------

  (setq flycheck-temp-prefix ".flycheck")

  (setq flycheck-markdown-markdownlint-cli-config
        (concat doom-private-dir "markdownlint-config.yml"))

  (setq flycheck-yamllintrc
        (concat doom-private-dir "yamllintrc.yml"))

  (setq flycheck-flake8rc
        (concat doom-private-dir "flake8.rc"))

  (setq flycheck-pylintrc
        (concat doom-private-dir "pylint.rc"))

  ;; (flycheck-add-next-checker 'python-flake8 'python-pylint)

  (flycheck-def-config-file-var flycheck-vhdl-vsg-config vhdl-vsg ".vsgrc")

  (flycheck-define-checker vhdl-vsg
    "VHDL Style Guide (VSG) provides coding style guide enforcement for VHDL code.
 https://vhdl-style-guide.readthedocs.io/en/latest/index.html"
    :command ("vsg" "-f"
              (config-file "-c" flycheck-vhdl-vsg-config)
              source)
    ;; https://vhdl-style-guide.readthedocs.io/en/latest/formatting_terminal_output.html
    ;; use syntastic format?
    :error-patterns
    ((error line-start (zero-or-more blank) (one-or-more (not blank)) (one-or-more blank) "|" (one-or-more blank)
            "Error"
            (one-or-more blank) "|" (one-or-more blank) line (one-or-more blank) "|" (one-or-more blank) (message) eol)
     (warning line-start (zero-or-more blank) (one-or-more (not blank)) (one-or-more blank) "|" (one-or-more blank)
              "Warning"
              (one-or-more blank) "|" (one-or-more blank) line (one-or-more blank) "|" (one-or-more blank) (message) eol))

    :modes vhdl-mode)


  ;; (flycheck-define-checker tcl-nagelfar
  ;;   "An extensible tcl syntax checker See URL `http://nagelfar.sourceforge.net/'."
  ;;   :command ("nagelfar" "-H" source)
  ;;   :error-patterns
  ;;   ;; foo.tcl: 29: E Wrong number of arguments (4) to "set"
  ;;   ;; foo.tcl: 29: W Expr without braces
  ;;   ((info    line-start (file-name) ": " line ": N " (message) line-end)
  ;;    (warning line-start (file-name) ": " line ": W " (message) line-end)
  ;;    (error   line-start (file-name) ": " line ": E " (message) line-end))
  ;;   :modes tcl-mode)
  ;; )

  ;; architecture_010          | Error      |        643 | Add *architecture* keyword.


  ;; (flycheck-define-checker vhdl-tool
  ;;   "A VHDL syntax checker, type checker and linter using VHDL-Tool.
  ;;         See URL `http://vhdltool.com'."
  ;;   :command ("vhdl-tool" "client" "lint" "--compact" "--stdin" "-f" source
  ;;             )
  ;;   :standard-input t
  ;;   :error-patterns
  ;;   ((warning line-start (file-name) ":" line ":" column ":w:" (message) line-end)
  ;;    (error line-start (file-name) ":" line ":" column ":e:" (message) line-end))
  ;;   :modes (vhdl-mode))
  ;;
  ;; (add-to-list 'flycheck-checkers 'vhdl-tool)
  )
