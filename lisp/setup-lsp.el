;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; LSP
;;------------------------------------------------------------------------------

(use-package lsp-mode
  :when (modulep! :tools lsp -eglot)

  :custom

  (lsp-auto-register-remote-clients nil) ; ?? does not seem to work ; https://github.com/doomemacs/doomemacs/issues/7491
  (lsp-file-watch-threshold 2000)
  (lsp-warn-no-matched-clients nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-vhdl-server 'hdl-checker)
  ;; (lsp-vhdl-server-path "~/.local/bin/hdl_checker"); only needed if hdl_checker is not already on the PATH

  :config

  (add-to-list 'lsp-file-watch-ignored (expand-file-name "~/.pyenv"))
  (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
  (add-to-list 'lsp-disabled-clients 'svlangserver)
  (add-to-list 'lsp-disabled-clients 'lsp-verilog-verible)

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

(use-package! eglot

  :if (modulep! :tools lsp +eglot)

  :custom

  (eglot-managed-mode-hook (list (lambda () (eldoc-mode -1))))
  (eglot-events-buffer-config '(:size 2000000 :format full))
  (eglot-prefer-plaintext nil)
  (eglot-autoshutdown t)

  :config

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
