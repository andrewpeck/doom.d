;; -*- lexical-binding: t; -*-

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
  (add-hook 'python-base-mode-hook #'apheleia-mode)

  ;;; invoke python ts mode
  (add-hook 'python-mode-hook #'python-ts-mode)

  ;;; python + eglot
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)

  (add-hook 'python-base-mode-hook
            (defun hook/disable-eldoc-mode () (eldoc-mode nil)))

  (add-hook 'python-base-mode-hook
            (defun hook/disable-flycheck-for-ipynb ()
              (when-let* ((name (buffer-file-name)))
                (when (string=  (file-name-extension name) "ipynb")
                  (flycheck-mode 0)))))

  (defun uv-tool (packages)
    "Use uv tool to install a list of PACKAGES or a single package.

e.g. (uv-install \\='(\"mypy\" \"flake8\"))

or for a single package (uv-install \"mypy\")
"
    (let ((pkg-str
           (cond
            ((stringp packages) packages)
            ((listp packages) (string-join packages " "))
            (t (error "Unrecognized input to uv-tool.")))))
      (compile (concat "uv tool install " pkg-str))))

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

  (defun my/setup-python-venv ()
    "Set up a python venv at project root."
    (interactive)

    (buffer-env-update)

    (unless (getenv "VIRTUAL_ENV")
      (when (yes-or-no-p "Virtual environment not active. Do you want to create at venv project root?")
        (when (shell-command (concat "uv venv --python 3.12 " (doom-project-root) ".venv"))

          (let* ((file (concat (doom-project-root) ".venv/bin/activate"))
                 (hash (with-temp-buffer
                         (insert-file-contents-literally file)
                         (secure-hash 'sha256 (current-buffer)))))
            (or (member (cons file hash) buffer-env-safe-files)
                (customize-save-variable
                       'buffer-env-safe-files
                       (push (cons file hash) buffer-env-safe-files))))

          (buffer-env-update))))

    (when (getenv "VIRTUAL_ENV")
      (unless (executable-find "mypy")
        (uv-install '("mypy")))
      (unless (executable-find "flake8")
        (uv-install '("flake8")))
      (unless (executable-find "ty")
        (uv-install '("ty")))
      (unless (executable-find "rass")
        (uv-install '("rassumfrassum")))))

  :config

  ;;------------------------------------------------------------------------------
  ;; Keybindings
  ;;------------------------------------------------------------------------------


  (defun ruff-fmt ()
    "Format the current buffer with Ruff."
    (interactive)
    (apheleia-format-buffer 'ruff))

  (map! :map python-base-mode-map :localleader "ts" #'python-string-toggle)
  (map! :map python-base-mode-map :localleader "bf" #'ruff-fmt)

  (map! :localleader :map (python-mode-map python-ts-mode-map)
        (:prefix ("t" . "test")
         :desc "Test File" "f" #'python-pytest-file))

  ;;------------------------------------------------------------------------------
  ;; Hacks
  ;;------------------------------------------------------------------------------

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

  ;;------------------------------------------------------------------------------
  ;; Sorting
  ;;------------------------------------------------------------------------------

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
         :desc "Eval code cells above." "C" #'code-cells-eval-above))


  (defun jupyter-install-nbdime ()
    (unless (executable-find "nbdime")
      (uv-tool "nbdime")
      (shell-command "nbdime config-git --enable --global"))))
