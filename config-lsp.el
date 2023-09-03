;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; LSP Mode
;;------------------------------------------------------------------------------

(setq lsp-enable-file-watchers nil)

;; vhdl-tool, hdl-checker vhdl-ls ghdl-ls
(setq lsp-vhdl-server 'ghdl-ls)
(let ((exe (cl-case lsp-vhdl-server
             ('hdl-checker "hdl_checker")
             ('vhdl-tool "vhdl-tool")
             ('ghdl-ls "ghdl-ls")
             ('vhdl-ls "vhdl_ls"))))
  (setq lsp-vhdl-server-path (executable-find exe)))

(setq lsp-progress-via-spinner nil
      ccls-sem-highlight-method nil
      lsp-enable-on-type-formatting nil
      lsp-enable-indentation nil
      +format-with-lsp nil)

;; https://github.com/chipsalliance/verible/blob/master/verilog/tools/ls/README.md
(add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))

;; (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("svls"))
;;    :major-modes '(verilog-mode)
;;    :priority -1))
;; (require 'lsp-mode)

;; (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))

(setq lsp-client-packages '(ccls))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("verible-verilog-ls --rules_config_search true"))
                  :major-modes '(verilog-mode)
                  :priority -1))

(add-hook 'verilog-mode-hook 'lsp)
(add-hook 'vhdl-mode-hook 'lsp)
