;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------

(use-package! pyenv-mode
  :config
  ;; damn pyenv-mode sets C-c C-s and it shows up everywhere (e.g. in latex)
  (define-key pyenv-mode-map (kbd "C-c C-s") nil))

(use-package! python
  :defer-incrementally t
  :init

  ;; Initialize LSP unless the python file is remote
  (defun +python-init-lsp-mode-maybe-h ()
    "Initialize LSP unless the python file is remote."
    (unless (and (buffer-file-name)
                 (file-remote-p (buffer-file-name)))
      (call-interactively #'lsp)))
  (add-hook! 'python-mode-local-vars-hook #'+python-init-lsp-mode-maybe-h)

  (remove-hook! 'python-mode-local-vars-hook #'tree-sitter!)
  (add-hook! 'python-mode-local-vars-hook
    (defun +python-init-tree-sitter-mode-maybe-h ()
      (unless (and (buffer-file-name)
                   (file-remote-p (buffer-file-name)))
        (tree-sitter!))))

  ;; modify the hook found in doom;
  ;; activating anaconda on tramp buffers is slow as hell
  (remove-hook! 'python-mode-local-vars-hook
    #'+python-init-anaconda-mode-maybe-h)
  (add-hook! 'python-mode-local-vars-hook :append
    (defun +python-init-anaconda-mode-maybe-h ()
      "Enable `anaconda-mode' if `lsp-mode' is absent and
`python-shell-interpreter' is present and we aren't on a tramp buffer."
      (unless (or (and (buffer-file-name) (file-remote-p (buffer-file-name)))
                  (bound-and-true-p lsp-mode)
                  (bound-and-true-p eglot--managed-mode)
                  (bound-and-true-p lsp--buffer-deferred)
                  (not (executable-find python-shell-interpreter t)))
        (anaconda-mode +1))))

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

  ;; (defun save-window (&rest args)
  ;;   (lambda (orig-fun &rest args)
  ;;     (let ((current (selected-window)))
  ;;       (apply orig-fun args)
  ;;       (select-window current)))
  ;;   )

  ;;   (advice-add 'python-shell-send-buffer :around
  ;;               (lambda (orig-fun &rest args)

  ;;                 (call-interactively #'run-python)
  ;;                 ;; (call-interactively #'run-python)

  ;;                   (apply orig-fun args)
  ;; ))

  ;; (defun run-python-unless (&rest _)
  ;;     "Run python (unless it is already running)"
  ;;     (interactive)

  ;;     (unless (python-shell-get-buffer)
  ;;       (python-shell-make-comint
  ;;        (python-shell-calculate-command)
  ;;        (python-shell-get-process-name t) t)

  ;; ))


  ;; (advice-add 'python-shell-send-buffer
  ;;             :before
  ;;             (lambda (&rest _)
  ;;               (call-interactively #'run-python)))

  ;; (advice-add 'python-shell-send-region
  ;;             :before
  ;;             (lambda (&rest _)
  ;;               (interactive)
  ;;               (call-interactively #'run-python)))

  (setq python-shell--interpreter "python3"
        python-flymake-command '("flake8" "-")
        py-isort-options '("--line-length" "300"))

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
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)

  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))

              (pet-flycheck-setup)

              (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter
                          lsp-pyright-venv-path python-shell-virtualenv-root)

              ;; (setq-local dap-python-executable python-shell-interpreter)

              )))


;;------------------------------------------------------------------------------
;; Jupyter Code Cells
;;------------------------------------------------------------------------------

(use-package! code-cells
  :defer t
  :mode ("\\.ipynb\\'")
  :config
  (map! :localleader
        :map code-cells-mode-map
        :prefix "m"
        (:prefix ("e" . "eval")
                 "c" #'code-cells-eval
                 "C" #'code-cells-eval-above)))
