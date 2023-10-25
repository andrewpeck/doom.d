;;------------------------------------------------------------------------------
;; Eglot
;;------------------------------------------------------------------------------

(with-eval-after-load 'eglot

  (setq eglot-prefer-plaintext t
        eglot-autoshutdown t
        ;; eglot-events-buffer-size 0
        ;; eldoc-echo-area-prefer-doc-buffer nil
        ;; eldoc-echo-area-use-multiline-p t
        )


  ;; (add-hook! python-mode-hook
  ;;   (setq eglot-workspace-configuration
  ;;         '((pyright
  ;;            (plugins
  ;;             (mccabe (enabled . t))    ; Remove this if you want mccabe.
  ;;             (pycodestyle (enabled . nil))
  ;;             (flake8 (enabled . t)))))))

  ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  ;; (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))

                                        ; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=61373
  ;; pyright generates html :(
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=61373

  ;; (dolist (provider '(:hoverProvider :documentHighlightProvider))
  ;;   (add-to-list 'eglot-ignored-server-capabilities provider))


  ;; (add-to-list 'eglot-workspace-configuration
  ;;              '(:svlangserver (:settings (:systemverilog.launchConfiguration: "verilator -sv -Wall --lint-only",
  ;;                                          :systemverilog.formatCommand: "verible-verilog-format"))))

  ;; (add-to-list 'eglot-server-programs
  ;;              '(verilog-mode . ("svls")))

  ;; (add-hook! verilog-mode-hook
  ;;   (setq eglot-workspace-configuration
  ;;         `((:systemverilog
  ;;            (:includeIndexing '["**/*.{sv,svh}"])
  ;;            (:excludeIndexing '["test/**/*.{sv,svh}"])
  ;;            (:defines nil)
  ;;            (:launchConfiguration "verilator -sv --lint-only -Wall")
  ;;            (:lintOnUnsaved t)
  ;;            (:formatCommand "verible-verilog-format")
  ;;            (:disableCompletionProvider nil)
  ;;            (:disableHoverProvider nil)
  ;;            (:disableSignatureHelpProvider nil)
  ;;            (:disableLinting nil)))))

  (add-to-list 'eglot-server-programs
               '(vhdl-mode . ("ghdl-ls")))
  )
