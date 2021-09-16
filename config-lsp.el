;; -*- lexical-binding: t; -*-

;; LSP
;;------------------------------------------------------------------------------

(after! lsp-ui
  (setq-default lsp-headerline-breadcrumb-enable t
                lsp-ui-doc-enable t
                lsp-ui-doc-show-with-mouse t
                lsp-ui-doc-show-with-cursor nil
                lsp-ui-doc-max-width 150
                lsp-ui-sideline-show-hover nil
                lsp-ui-sideline-enable nil
                lsp-ui-doc-delay 0.2))

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'vhdl-mode-hook #'lsp)
(add-hook 'vhdl-mode-hook #'lsp-ui-mode)

(add-hook 'python-mode-hook #'lsp-mode)
(add-hook 'python-mode-hook #'lsp-ui-mode)


(after! lsp-mode

  ;; vhdl-tool, hdl-checker vhdl-ls ghdl-ls
  (setq lsp-vhdl-server 'vhdl-tool)
  (cl-case lsp-vhdl-server
    ('vhdl-tool (setq lsp-vhdl-server-path (executable-find "vhdl-tool")))
    ('ghdl-ls (setq lsp-vhdl-server-path (executable-find "ghdl-ls")))
    ('vhdl-ls (setq lsp-vhdl-server-path (executable-find "vhdl_ls"))))

  (setq lsp-progress-via-spinner nil
        ccls-sem-highlight-method nil
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        +format-with-lsp nil)

  ) ;; after! lsp


;; (require 'lsp)
;; (add-hook 'vhdl-mode-hook #'lsp)
;; (add-to-list 'lsp-language-id-configuration '(vhdl-mode . "vhdl"))
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection '("ghdl-ls"))
;;                   :major-modes '(vhdl-mode)
;;                   :priority -1
;;                   :server-id 'lsp-vhdl-mode))

;; (setq lsp-log-io nil
;;       lsp-auto-configure t
;;       lsp-auto-guess-root t
;;       lsp-completion-enable t
;;       lsp-enable-xref t
;;       lsp-prefer-flymake nil
;;       lsp-use-native-json t
;;       lsp-enable-indentation t
;;       lsp-response-timeout 10
;;       lsp-restart 'auto-restart
;;       lsp-keep-workspace-alive t
;;       lsp-eldoc-render-all nil
;;       lsp-enable-snippet nil
;;       lsp-enable-folding t
;;       )

;; ;; lsp-ui gives us the blue documentation boxes and the sidebar info
;; (setq lsp-ui-sideline-ignore-duplicate t
;;       lsp-ui-sideline-delay 0.5
;;       lsp-ui-sideline-show-symbol t
;;       lsp-ui-sideline-show-hover t
;;       lsp-ui-sideline-show-diagnostics t
;;       lsp-ui-sideline-show-code-actions t
;;       lsp-ui-peek-always-show t
;;       lsp-ui-doc-use-childframe t)

;; (lsp-ui-flycheck-enable t)
;; (lsp-ui-sideline-enable t)
;; (lsp-ui-imenu-enable t)
;; (lsp-lens-mode t)
;; (lsp-ui-peek-enable t)
;; (lsp-ui-doc-enable t)

;;   ;;; company lsp
;; ;; install LSP company backend for LSP-driven completion
;; (setq company-lsp-cache-candidates t
;;       company-lsp-enable-recompletion t
;;       company-lsp-enable-snippet t
;;       company-lsp-async t)

;; (setq lsp-vhdl-server 'ghdl-ls
;;       lsp-vhdl-server-path (executable-find "ghdl-ls")
;;       lsp-vhdl--params nil)

;; ;; (flycheck-define-checker vhdl-tool
;; ;;   "A VHDL syntax checker, type checker and linter using VHDL-Tool.
;; ;;         See URL `http://vhdltool.com'."
;; ;;   :command ("vhdl-tool" "client" "lint" "--compact" "--stdin" "-f" source
;; ;;             )
;; ;;   :standard-input t
;; ;;   :error-patterns
;; ;;   ((warning line-start (file-name) ":" line ":" column ":w:" (message) line-end)
;; ;;    (error line-start (file-name) ":" line ":" column ":e:" (message) line-end))
;; ;;   :modes (vhdl-mode))
;; ;;
;; ;; (add-to-list 'flycheck-checkers 'vhdl-tool)
