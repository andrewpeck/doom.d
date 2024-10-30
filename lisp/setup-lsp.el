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
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection)) ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))

  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))
