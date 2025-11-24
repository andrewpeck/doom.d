;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; LSP
;;------------------------------------------------------------------------------

(use-package lsp-pyright
  :when (modulep! :tools lsp -eglot)
  :custom (lsp-pyright-langserver-command "basedpyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package lsp-mode
  :when (modulep! :tools lsp -eglot)

  :custom

  (lsp-completion-provider nil)
  (lsp-auto-register-remote-clients nil) ; ?? does not seem to work ; https://github.com/doomemacs/doomemacs/issues/7491
  (lsp-file-watch-threshold 2000)
  (lsp-warn-no-matched-clients nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-vhdl-server 'hdl-checker)

  :config

  (add-to-list 'lsp-file-watch-ignored (expand-file-name "~/.pyenv"))
  (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
  (add-to-list 'lsp-disabled-clients 'svlangserver)
  (add-to-list 'lsp-disabled-clients 'lsp-verilog-verible))

;;------------------------------------------------------------------------------
;; Eglot
;;------------------------------------------------------------------------------

(use-package! eglot

  :if (modulep! :tools lsp +eglot)

  :config

  (setq eglot-managed-mode-hook (list (lambda () (eldoc-mode -1)))
        eglot-events-buffer-config '(:size 2000000 :format full)
        eglot-prefer-plaintext nil
        eglot-autoshutdown t)

  (add-hook 'eglot-managed-mode-hook 'eglot-inlay-hints-mode t)

  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio"))

  (defun emacs-lsp-booster-install ()
    "Install LSP booster with cargo."
    (interactive)
    (compile "cargo install --git https://github.com/blahgeek/emacs-lsp-booster"))

  (unless (executable-find "emacs-lsp-booster")
    (error "emacs-lsp booster not found! install with emacs-lsp-booster-install"))

  (add-to-list 'eglot-server-programs
               '(vhdl-mode . ("ghdl-ls"))))

;;------------------------------------------------------------------------------
;; Eglot Booster
;;------------------------------------------------------------------------------

(use-package! eglot-booster
  :after eglot
  :custom
  (eglot-booster-no-remote-boost t)
  (eglot-booster-io-only t)
  :init
  (cl-remprop 'buffer-local-value 'byte-obsolete-generalized-variable)
  :config
  (eglot-booster-mode))
