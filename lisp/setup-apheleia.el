;;------------------------------------------------------------------------------
;; Apheleia
;;------------------------------------------------------------------------------

(use-package apheleia
  :config

  ;;; turn on apheleia
  (add-hook 'python-base-mode-hook #'apheleia-mode)

  ;;; turn on apheleia
  (add-hook 'verilog-mode-hook #'apheleia-mode)

  ;; don't want to apply autoformatter for files with conflict markers in them
  ;; just add some simple advice to look for conflict markers and skip the
  ;; formatter, issuing a message along the way.
  (advice-add #'apheleia-format-buffer :before-until
              (defun file-has-conflict-markers (&rest _)
                (let ((is-conflict (save-excursion
                                     (goto-char (point-min))
                                     (re-search-forward "^<<<<<<< [A-z]+$" nil t))))
                  (if is-conflict
                      (message "File has conflict markers; not formatting.")
                    (message "Formatting buffer..."))

                  is-conflict)))

  (add-to-list 'apheleia-formatters '(sysarch-sv-format "sysarch-sv-format" "--stdout" "-"))
  (add-to-list 'apheleia-mode-alist '(verilog-ts-mode . sysarch-sv-format))

  (add-to-list 'apheleia-formatters '(docstrfmt "docstrfmt"))
  (add-to-list 'apheleia-mode-alist '(rst-mode . docstrfmt))

  (add-to-list 'apheleia-formatters '(autopep8 "autopep8" "-"))
  (add-to-list 'apheleia-formatters '(python-mode isort "isort"  "-ca" "--stdout" "-"))

  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))

  (add-to-list 'apheleia-mode-alist '(python-mode . autopep8))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . autopep8)))
