;; -*- lexical-binding: t; -*-

(use-package! treesit

  :config

  (set-tree-sitter! 'verilog-mode 'verilog-ts-mode
    '((verilog :url "https://github.com/tree-sitter/tree-sitter-verilog"
       :commit "227d277b6a1a5e2bf818d6206935722a7503de08")))

  (setq treesit-font-lock-level 4)

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

;;------------------------------------------------------------------------------
;; Treesitter
;;------------------------------------------------------------------------------

(comment
 (use-package treesit-auto
   :commands (global-treesit-auto-mode
              treesit-auto-install-all
              treesit-auto-add-to-auto-mode-alist)
   :custom
   (treesit-font-lock-level 5)
   (treesit-auto-install 'prompt)
   :config
   (delete 'janet treesit-auto-langs)
   (delete 'markdown treesit-auto-langs)
   (delete 'latex treesit-auto-langs)
   (treesit-auto-add-to-auto-mode-alist 'all)
   (global-treesit-auto-mode)))
