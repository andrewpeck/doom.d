;;------------------------------------------------------------------------------
;; LSP
;;------------------------------------------------------------------------------

(when (and (modulep! :tools lsp)
           (not (modulep! :tools lsp +eglot)))

  (use-package lsp-mode
    :config

    (setopt lsp-file-watch-threshold 2000)
    (setopt lsp-warn-no-matched-clients nil)

    (setopt lsp-enable-symbol-highlighting nil
            lsp-vhdl-server 'hdl-checker
            ;; lsp-vhdl-server-path "~/.local/bin/hdl_checker"; only needed if hdl_checker is not already on the PATH
            )

    (add-to-list 'lsp-file-watch-ignored (expand-file-name "~/.pyenv"))
    (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
    (add-to-list 'lsp-disabled-clients 'svlangserver)
    (add-to-list 'lsp-disabled-clients 'lsp-verilog-verible)

    ;; (add-hook 'verilog-mode-hook 'lsp)
    ;; (add-hook 'vhdl-mode-hook 'lsp)

    ;; https://github.com/doomemacs/doomemacs/issues/7491
    (setq lsp-auto-register-remote-clients nil) ; ?? does not seem to work
    )

  ;;------------------------------------------------------------------------------
  ;; Booster
  ;;------------------------------------------------------------------------------


  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?) ;; for check lsp-server-present?
               (not (remote-host? default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection)) ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))

  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;;------------------------------------------------------------------------------
;; Eglot
;;------------------------------------------------------------------------------

(when (modulep! :tools lsp +eglot)
      (use-package! eglot

        :init

        (setq eglot-managed-mode-hook
              (list (lambda () (eldoc-mode -1))))

        :config
 
        (add-to-list 'eglot-server-programs
                     '((python-mode python-ts-mode)
                       "basedpyright-langserver" "--stdio"))

        (defun emacs-lsp-booster-install ()
          (interactive)
          (compile "cargo install --git https://github.com/blahgeek/emacs-lsp-booster"))

        (unless (executable-find "emacs-lsp-booster")
          (error "emacs-lsp booster not found! install with emacs-lsp-booster-install"))

        (setq eglot-prefer-plaintext nil
              eglot-autoshutdown t
              help-at-pt-display-when-idle t)

        ;; (add-to-list 'eglot-server-programs '(python-mode . ("ruff-lsp")))
        ;; (add-to-list 'eglot-server-programs '(python-ts-mode . ("ruff-lsp")))
        ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
        ;; (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))

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
                     '(vhdl-mode . ("ghdl-ls"))))

      (use-package! eglot-booster
        :after eglot
        :init
        (cl-remprop 'buffer-local-value 'byte-obsolete-generalized-variable)
        :config
        (setq eglot-booster-no-remote-boost t)))
