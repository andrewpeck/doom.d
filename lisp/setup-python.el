;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; pyenv mode
;;------------------------------------------------------------------------------

(use-package! pyenv-mode
  :config
  ;; damn pyenv-mode sets C-c C-s and it shows up everywhere (e.g. in latex)
  (define-key pyenv-mode-map (kbd "C-c C-s") nil))

;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

(use-package! python
  :functions (python--do-isort)
  :defer-incrementally t
  :init

  (defun advice/close-lookup-maybe (_id &rest _arg)
    "Repeated invotations of +lookup/documentation will simply toggle the
help instead of keeping it open."
    (let ((buffers (seq-filter (lambda (buf)
                                 (or (string-prefix-p "*eglot-help" (buffer-name buf))
                                     (string-prefix-p "*helpful " (buffer-name buf))))
                               (buffer-list))))
      (when buffers
        (mapc #'kill-buffer buffers))

      buffers))

  (advice-add #'+lookup/documentation :before-until
              #'advice/close-lookup-maybe)

  ;; Initialize LSP unless the python file is remote
  (defun +python-init-lsp-mode-maybe-h ()
    "Initialize LSP unless the python file is remote."
    (when (fboundp 'lsp)
      (unless (and (buffer-file-name)
                   (remote-host? (buffer-file-name)))
        (call-interactively #'lsp))))
  (add-hook! 'python-mode-local-vars-hook #'+python-init-lsp-mode-maybe-h)

  (remove-hook! 'python-mode-local-vars-hook #'tree-sitter!)
  (add-hook! 'python-mode-local-vars-hook
    (defun +python-init-tree-sitter-mode-maybe-h ()
      (unless (and (buffer-file-name)
                   (remote-host? (buffer-file-name)))
        (tree-sitter!))))

  ;; modify the hook found in doom;
  ;; activating anaconda on tramp buffers is slow as hell
  (remove-hook! 'python-mode-local-vars-hook
    #'+python-init-anaconda-mode-maybe-h)
  (add-hook! 'python-mode-local-vars-hook :append
    (defun +python-init-anaconda-mode-maybe-h ()
      "Enable `anaconda-mode' if `lsp-mode' is absent and
`python-shell-interpreter' is present and we aren't on a tramp buffer."
      (unless (or (and (buffer-file-name) (remote-host? (buffer-file-name)))
                  (bound-and-true-p lsp-mode)
                  (bound-and-true-p eglot--managed-mode)
                  (bound-and-true-p lsp--buffer-deferred)
                  (not (executable-find python-shell-interpreter t)))
        (anaconda-mode +1))))

  (defun my/check-python-tooling ()
    (unless (executable-find "ruff")
      (warn "ruff not found! please install it"))
    (unless (executable-find "pyright")
      (warn "pyright not found! please install it"))
    (unless (executable-find "mypy")
      (warn "mypy not found! please install it")))

  (add-hook 'python-base-mode-hook #'my/check-python-tooling)

  :config

  (advice-add 'run-python :around
              (lambda (orig-fun &rest args)
                (let ((current (selected-window)))
                  (apply orig-fun args)
                  (select-window current))))

  ;; This option influences run-python when called without a prefix
  ;; argument.  If buffer or project, create a Python shell
  ;; dedicated to the current buffer or its project (if one is found).
  (setq python-shell-dedicated 'buffer)

  (defvar py-isort-options '("--line-length" "300") "List of arguments to apply in `python-sort-imports")
  (setopt python-flymake-command '("flake8" "-"))

  (defun python-sort-imports ()
    "Sort Python imports in the current buffer."
    (interactive)
    (if (apply #'python--do-isort py-isort-options)
        (message "Sorted imports")
      (message "(No changes in Python imports needed)"))))

;;------------------------------------------------------------------------------
;; Emacs Pet
;;------------------------------------------------------------------------------

(use-package pet
  :init
  (add-hook 'python-base-mode-hook 'pet-mode -10)

  (add-hook 'python-base-mode-hook
            (lambda ()
              (pet-mode)

              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))

              (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter
                          lsp-pyright-venv-path python-shell-virtualenv-root)

              (setq-local dap-python-executable python-shell-interpreter)

              (setq-local python-pytest-executable (pet-executable-find "pytest"))

              (pet-flycheck-setup)
              (flycheck-mode))))


;;------------------------------------------------------------------------------
;; Jupyter Code Cells
;;------------------------------------------------------------------------------

(use-package code-cells
  :config
  (map! :localleader :map code-cells-mode-map
        (:prefix-map ("e" . "eval")
         :desc "Eval code cell." "c" #'code-cells-eval
         :desc "Eval code cells above." "C" #'code-cells-eval-above)))
