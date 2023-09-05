;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; LSP Mode
;;------------------------------------------------------------------------------

(after! lsp-mode
  (setq lsp-enable-file-watchers nil
        lsp-progress-via-spinner nil
        ccls-sem-highlight-method nil
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        +format-with-lsp nil)

  ;; lsp-terraform was crashing lsp mode for some reason (tbd)
  (delete 'lsp-terraform lsp-client-packages))

;;------------------------------------------------------------------------------
;; VHDL
;;------------------------------------------------------------------------------
;; vhdl-tool, hdl-checker vhdl-ls ghdl-ls

(after! lsp-mode
  (setq lsp-vhdl-server 'ghdl-ls)
  (let ((exe (cl-case lsp-vhdl-server
               (hdl-checker "hdl_checker")
               (vhdl-tool "vhdl-tool")
               (ghdl-ls "ghdl-ls")
               (vhdl-ls "vhdl_ls"))))
    (setq lsp-vhdl-server-path (executable-find exe))))

(add-hook! 'vhdl-mode-hook #'lsp)

;;------------------------------------------------------------------------------
;; Verilog
;;------------------------------------------------------------------------------

(after! lsp-mode
  ;; https://github.com/chipsalliance/verible/blob/master/verilog/tools/ls/README.md
  (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))

  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-stdio-connection '("svls"))
  ;;   :major-modes '(verilog-mode)
  ;;   :priority -1))

  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-stdio-connection
  ;;                    '("verible-verilog-ls" "--rules_config_search" "true"))
  ;;   :major-modes '(verilog-mode)
  ;;   :priority -1))
  )

(after! lsp-mode
  (setq lsp-clients-svlangserver-launchConfiguration "/tools/verilator -sv --lint-only -Wall"
        lsp-clients-svlangserver-formatCommand "/tools/verible-verilog-format"))

(add-hook! 'verilog-mode-hook (lsp))
