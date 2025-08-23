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
  :init

  ;;; turn on apheleia
  (comment (add-hook 'python-base-mode-hook #'apheleia-mode))

  ;;; invoke python ts mode
  (add-hook 'python-mode-hook #'python-ts-mode)

  ;;; python + eglot
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)

  (add-hook 'flycheck-mode-hook
            (defun hook/configure-python-checkers ()
              (when (or (equal major-mode 'python-ts-mode)
                        (equal major-mode 'python-mode))
                (setq flycheck--automatically-enabled-checkers
                      (remove nil
                              (list (when (executable-find "ruff") 'python-ruff)
                                    (when (executable-find "flake8") 'python-flake8)
                                    (when (executable-find "mypy") 'python-mypy))))
                (setq-local flycheck-disabled-checkers '(python-pylint)))))

  (flycheck-add-next-checker 'python-flake8 (cons t 'python-ruff))
  (flycheck-add-next-checker 'python-ruff (cons t 'python-mypy))

  ;; (flycheck-add-next-checker 'python-ruff (cons t 'python-pyright))

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

  (defun uv-install (packages)
    "Use uv pip to install a list of PACKAGES or a single package.

e.g. (uv-install \\='(\"mypy\" \"flake8\"))

or for a single package (uv-install \"mypy\")
"
    (let ((pkg-str
           (cond
            ((stringp packages) packages)
            ((listp packages) (string-join packages " "))
            (t (error "Unrecognized input to uv-install.")))))
      (compile (concat "uv pip install " pkg-str))))

  (defun my/setup-python-tooling ()
    (interactive)

    (unless (getenv "VIRTUAL_ENV")
      (when (yes-or-no-p "Virtual environment not active. Do you want to create want at project root?")
        (when (shell-command (concat "uv venv --python 3.12 " (doom-project-root) ".venv"))
          (buffer-env-update))))

    (when (getenv "VIRTUAL_ENV")
      (uv-install '("mypy" "flake8")))

    (my/check-python-tooling))

  :config

  (define-key python-ts-mode-map (kbd "C-c C-e") nil)

  (map! :localleader :map (python-mode-map python-ts-mode-map)
        (:prefix ("t" . "test")
         :desc "Test File" "f" #'python-pytest-file))

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
    (if (apheleia-format-buffer 'ruff-isort)
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
