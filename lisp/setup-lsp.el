;; -*- lexical-binding: t; -*-

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
