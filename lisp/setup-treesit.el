;; -*- lexical-binding: t; -*-

(use-package! treesit

  :config

  (add-to-list 'treesit-language-source-alist '(verilog "https://github.com/gmlarumbe/tree-sitter-systemverilog" nil nil nil nil))
  (add-to-list 'treesit-language-source-alist '(systemverilog "https://github.com/gmlarumbe/tree-sitter-systemverilog" nil nil nil nil))

  (add-hook 'markdown-mode-hook #'markdown-ts-mode)

  (set-tree-sitter! 'verilog-mode 'verilog-ts-mode
    '((verilog :url "https://github.com/tree-sitter/tree-sitter-verilog"
       :commit "227d277b6a1a5e2bf818d6206935722a7503de08")))

  (setopt treesit-font-lock-level 4)

  (defun treesit-explore-start ()
    (interactive)
    (add-hook 'post-command-hook #'treesit--explorer-post-command 0 t)
    (treesit-explore-mode t)
    (treesit--explorer-refresh)
    ;; this should happen automatically, something is wrong, buffer local hooks or something?
    ;; some other hook overwriting it?
    (treesit--explorer-post-command)))

(define-derived-mode verilog-ts-mode verilog-mode "Verilog"
  "A mode for Verilog."
  (when (treesit-ready-p 'verilog)
    (treesit-parser-create 'verilog)
    (treesit-major-mode-setup)))

(define-derived-mode vhdl-ts-mode vhdl-mode "Vhdl"
  "A mode for Vhdl."
  (when (treesit-ready-p 'vhdl)
    (treesit-parser-create 'vhdl)
    (treesit-major-mode-setup)))

;;------------------------------------------------------------------------------
;; Python Highlighting
;;------------------------------------------------------------------------------

(use-package! treesit

  :config

  (defface python-dict-key-face
    '((t :inherit font-lock-property-use-face))
    "Face for Python dictionary string keys.")

  (defun python--apply-dict-key-overlays ()
    (when (treesit-parser-list nil 'python)
      (remove-overlays (point-min) (point-max) 'python-dict-key t)
      (dolist (cell (treesit-query-capture
                     (treesit-buffer-root-node 'python)
                     '((pair key: (string) @k))))
        (let ((ov (make-overlay (treesit-node-start (cdr cell))
                                (treesit-node-end   (cdr cell)))))
          (overlay-put ov 'face 'python-dict-key-face)
          (overlay-put ov 'python-dict-key t)
          (overlay-put ov 'priority 100)))))   ; beats eglot text properties

  (add-hook 'python-ts-mode-hook
            (lambda ()
              ;; Run after eglot applies semantic tokens
              (add-hook 'eglot-managed-mode-hook #'python--apply-dict-key-overlays nil t)
              ;; Re-run on buffer changes (debounced via idle timer)
              (add-hook 'after-change-functions
                        (lambda (&rest _)
                          (run-with-idle-timer 0.3 nil #'python--apply-dict-key-overlays))
                        nil t))))
