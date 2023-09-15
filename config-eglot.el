;;------------------------------------------------------------------------------
;; Eglot
;;------------------------------------------------------------------------------

(after! eglot
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
