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
                lsp-ui-doc-delay 0.1))

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'vhdl-mode-hook #'lsp)
(add-hook 'vhdl-mode-hook #'lsp-ui-mode)

(after! lsp
  (setq lsp-progress-via-spinner nil)
  (setq  ccls-sem-highlight-method nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-indentation nil)
  (setq +format-with-lsp nil)

  (add-hook 'python-mode-hook #'lsp-mode)
  (add-hook 'python-mode-hook #'lsp-ui-mode)

  ;; VHDL Tool
  (setq lsp-vhdl-server 'vhdl-tool)
  (setq lsp-vhdl-server-path "vhdl-tool")
  ;; (setq lsp-vhdl-server 'vhdl-ls)
  ;; (setq lsp-vhdl-server-path "~/bin/vhdl_ls")
  )


;; HDL Checker
;;(setq lsp-vhdl-server 'hdl-checker)

;; VHDL Ls (Rust HDL)
;;(setq lsp-vhdl-server 'vhdl-ls)
;;(setq lsp-vhdl-server-path "~/rust_hdl/target/release/vhdl_ls")

;;(use-package lsp-mode
;;         :config
;;         (add-hook 'vhdl-mode-hook 'lsp))

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
