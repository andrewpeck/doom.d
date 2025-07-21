;; -*- lexical-binding: t; -*-

(use-package buffer-env
  :init
  (add-hook 'hack-local-variables-hook #'buffer-env-update)
  (add-hook 'comint-mode-hook #'buffer-env-update)
  :custom
  (buffer-env-script-name '(".venv/bin/activate" ".env" ".envrc")))

;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

(use-package! python

  :defer t

  :commands (python--do-isort)

  :custom

  ;; This option influences run-python when called without a prefix
  ;; argument.  If buffer or project, create a Python shell
  ;; dedicated to the current buffer or its project (if one is found).
  (python-shell-dedicated 'buffer)

  (python-flymake-command '("flake8" "-"))

  (map! :localleader :map python-base-mode-map
        (:prefix-map ("t" . "test")
         :desc "Test File" "f" #'python-pytest-file))

  :init

  (comment
   (add-hook 'python-base-mode-hook #'apheleia-mode))

  (add-hook 'python-mode-hook #'python-ts-mode)

  (add-hook 'python-base-mode-hook
            (defun hook/disable-eldoc-mode () (eldoc-mode nil)))

  (add-hook 'python-base-mode-hook
            (defun hook/disable-flycheck-for-ipynb ()
              (when-let* ((name (buffer-file-name)))
                (when (string=  (file-name-extension name) "ipynb")
                  (flycheck-mode 0)))))

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

  :config

  (my/check-python-tooling)

  ;; for some reason the magit+mark-stale-buffers-h hook reverts the formatting
  ;; used by code-cells.el. It makes it so that any time you switch away (via
  ;; doom-switch-frame-hook) or open Magit code cells reverts from a python file
  ;; back into a JSON file. I'm not sure how to fix it yet but this is an easy workaround.
  (defun +magit-mark-stale-buffers-h () nil)

  (advice-add 'run-python :around
              (lambda (orig-fun &rest args)
                (let ((current (selected-window)))
                  (apply orig-fun args)
                  (select-window current))))

  (defvar py-isort-options '("--line-length" "300" "-sl")
    "List of arguments to apply in `python-sort-imports")

  (defun python-sort-imports ()
    "Sort Python imports in the current buffer."
    (interactive)

    (when (= 1 (call-process "python" nil nil nil "-m" "isort"))
      (when (getenv "VIRTUAL_ENV")
        (message "Installing isort into virtual environment")
        (call-process "uv" nil nil nil "pip" "install" "isort")))

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
