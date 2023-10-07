;;------------------------------------------------------------------------------
;; Eglot
;;------------------------------------------------------------------------------

(after! eglot

  (setq eglot-workspace-configuration
        '((pyright
           (plugins
            (mccabe (enabled . t)) ; Remove this if you want mccabe.
            (pycodestyle (enabled . nil))
            (flake8 (enabled . t))))))

  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))

                                        ; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=61373
  ;; pyright generates html :(
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=61373
  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             ))
  (setq eglot-prefer-plaintext t)
  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil)

  (dolist (provider '(:hoverProvider :documentHighlightProvider))
    (add-to-list 'eglot-ignored-server-capabilities provider))

  (add-to-list 'eglot-server-programs
               '(vhdl-mode . ("ghdl-ls"))))
