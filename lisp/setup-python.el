;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

(use-package! python
  :commands (python--do-isort)
  :init

  (add-hook 'python-base-mode-hook (lambda () (eldoc-mode nil)))

  (add-hook 'python-base-mode-hook
            (lambda ()
              (when-let* ((name (buffer-file-name)))
                (when (string=  (file-name-extension name) "ipynb")
                  (flycheck-mode 0)))))

  (add-hook 'python-base-mode-hook #'uv-mode-auto-activate-hook)

  (defun my/check-python-tooling ()
    "Check if python tooling is installed."
    (interactive)
    (unless (executable-find "ruff")
      (warn "ruff not found! please install it"))
    (unless (or (executable-find "basedpyright")
                (executable-find "pyright"))
      (warn "pyright/basedpyright not found! please install it"))
    (unless (executable-find "mypy")
      (warn "mypy not found! please install it")))

  (add-hook 'python-base-mode-hook 'my/check-python-tooling)

  ;; for some reason the magit+mark-stale-buffers-h hook reverts the formatting
  ;; used by code-cells.el. It makes it so that any time you switch away (via
  ;; doom-switch-frame-hook) or open Magit code cells reverts from a python file
  ;; back into a JSON file. I'm not sure how to fix it yet but this is an easy workaround.
  (defun +magit-mark-stale-buffers-h () nil)
  ;; (remove-hook 'doom-switch-frame-hook '+magit-mark-stale-buffers-h)

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
;; Jupyter Code Cells
;;------------------------------------------------------------------------------

(use-package code-cells
  :config
  (map! :localleader :map code-cells-mode-map
        (:prefix-map ("e" . "eval")
         :desc "Eval code cell." "c" #'code-cells-eval
         :desc "Eval code cells above." "C" #'code-cells-eval-above)))
