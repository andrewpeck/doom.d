;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Flycheck
;;------------------------------------------------------------------------------

(use-package! flycheck
  :defer-incrementally t

  :config

  (setq-default flycheck-ghdl-language-standard "08")

  (setq flycheck-temp-prefix ".flycheck"
        flycheck-check-syntax-automatically '(save idle-buffer-switch mode-enabled)
        flycheck-markdown-markdownlint-cli-config (concat doom-user-dir "markdownlint-config.yml")
        flycheck-yamllintrc (concat doom-user-dir "yamllintrc.yml")
        flycheck-flake8rc (concat doom-user-dir "flake8.rc")
        flycheck-pylintrc (concat doom-user-dir "pylint.rc"))

  ;; (flycheck-add-next-checker 'python-flake8 'python-pylint)

  ;;------------------------------------------------------------------------------
  ;; Verilator Modifications
  ;;------------------------------------------------------------------------------

  ;; add --timing opt to verilator
  (flycheck-define-checker verilog-verilator
    "A Verilog syntax checker using the Verilator Verilog HDL simulator.

    See URL `https://www.veripool.org/wiki/verilator'."
    :command ("verilator" "--timing" "--lint-only" "-Wall" "--quiet-exit"
              (option-list "-I" flycheck-verilator-include-path)
              source)
    :error-patterns
    ((warning line-start "%Warning"
              (? "-" (id (+ (any "0-9A-Z_")))) ": "
              (? (file-name) ":" line ":" (? column ":") " ")
              (message) line-end)
     (error line-start "%Error"
            (? "-" (id (+ (any "0-9A-Z_")))) ": "
            (? (file-name) ":" line ":" (? column ":") " ")
            (message) line-end))
    :modes verilog-mode)

  ;;------------------------------------------------------------------------------
  ;; Tcl Nagelfar
  ;; modified from the original to add filters and change options
  ;;------------------------------------------------------------------------------

  (flycheck-define-checker tcl-nagelfar
    "An extensible tcl syntax checker
See URL `http://nagelfar.sourceforge.net/'."
    :command ("nagelfar" "-Wunusedvar" "-filter" "*Unknown command*" "-H" source)
    :error-patterns
    ;; foo.tcl: 29: E Wrong number of arguments (4) to "set"
    ;; foo.tcl: 29: W Expr without braces
    ((info    line-start (file-name) ": " line ": N " (message) line-end)
     (warning line-start (file-name) ": " line ": W " (message) line-end)
     (error   line-start (file-name) ": " line ": E " (message) line-end))
    :modes tcl-mode)

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

  ;; (add-to-list 'flycheck-checkers 'proselint)
  (setq-default flycheck-disabled-checkers '(proselint))

  ;;------------------------------------------------------------------------------
  ;; HOG
  ;;------------------------------------------------------------------------------


  (flycheck-define-checker
      hog-src-checker
    "Checker for Hog source files"

    :command ("emacs" (eval flycheck-emacs-args)
              "--load" (eval (file-name-sans-extension (locate-library "hog")))
              "--visit" source-inplace
              "-f" "hog-check-src-file")

    :error-patterns
    ((error line-start "Error:" line (one-or-more blank) (message) line-end)
     (info line-start "Info:" line (one-or-more blank) (message) line-end)
     (warning line-start "Warning:" (one-or-more blank) line (message) line-end))
    :modes (hog-src-mode))

  (add-to-list 'flycheck-checkers 'hog-src-checker)

  ;;------------------------------------------------------------------------------
  ;; VHDL
  ;;------------------------------------------------------------------------------

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


  ;;------------------------------------------------------------------------------
  ;; AWK
  ;;------------------------------------------------------------------------------
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
  ;; SCAD
  ;;------------------------------------------------------------------------------

  (flycheck-define-checker openscad
    "Runs openscad"
    :command ("openscad"
              (eval (concat "-o" (flycheck-temp-dir-system) "/scad-tmp.png"))
              source-inplace)
    :error-patterns
    ;; different versions of scad produce slightly different error messages... uhg
    ((error line-start "ERROR:" (message) " " (file-name)  ", line " line line-end)
     (error line-start "ERROR:" (message) "\"" (file-name) "\", line " line ": syntax error" line-end))
    :modes (scad-mode))
  (add-to-list 'flycheck-checkers 'openscad))
