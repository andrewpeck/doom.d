(use-package! treesit

  :config

  (defun treesit-explore-start ()
    (interactive)

    (add-hook 'post-command-hook #'treesit--explorer-post-command 0 t)

    (treesit-explore-mode t)

    (treesit--explorer-refresh)

    ;; this should happen automatically, something is wrong, buffer local hooks or something?
    ;; some other hook overwriting it?
    (treesit--explorer-post-command))

  (define-derived-mode verilog-ts-mode verilog-mode "Verilog"
    "A mode for Verilog."
    (when (treesit-ready-p 'verilog)
      (treesit-parser-create 'verilog)
      (treesit-major-mode-setup))))
