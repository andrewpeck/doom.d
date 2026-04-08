;; -*- lexical-binding: t; -*-

(defun +lsp-shutdown ()
  (interactive)
  (let ((inhibit-message t))
    (flycheck-eglot-mode -1)
    (eglot-inlay-hints-mode -1)
    (when-let* ((timer eglot--outstanding-inlay-regions-timer))
      (cancel-timer timer))
    (when-let* ((current-server (eglot-current-server)))
      (ignore-errors (eglot-shutdown current-server))
      (let ((inhibit-message nil))
        (message "Shut down `%s' language server"
                 (nth 1 (eglot--server-info current-server)))))))

(defun +lsp-startup ()
  (interactive)
  (when (+lsp-should-start-p)
    (require 'eglot)
    (run-with-idle-timer 1 nil #'eglot-ensure)))

(defun +lsp-should-start-p (&rest _)
  (let ((should-start
         (and (eglot--lookup-mode major-mode)
              (or (not (derived-mode-p 'python-ts-mode 'python-ts-mode))
                  (and (require 'buffer-env)
                       (buffer-env-update)
                       buffer-env-active)))))
    (unless should-start
      (message "No virtual environment found. Not starting LSP.")) should-start))

(defun +lsp-restart ()
  (interactive)
  (+lsp-shutdown)
  (+lsp-startup))

(map! :leader
      :after eglot
      :desc "LSP"
      (:prefix ("l" . "LSP")
               "l" #'+lsp-startup
               "r" #'+lsp-restart
               "d" #'+lsp-shutdown))

(use-package! eglot

  :if (modulep! :tools lsp +eglot)

  :init

  ;; Don't auto-start eglot for Python without a venv. Doom's lsp! hook calls
  ;; eglot-ensure automatically; this intercepts it before the server spawns.
  (advice-add #'!lsp :override '+lsp-startup)

  :config

  (advice-add 'eglot--message :override
              (lambda (format &rest args)
                (let ((msg (apply #'eglot--format format args)))
                  (message
                   (cond
                    ((string-match-p "not watching some directories" msg) nil)
                    ((string-match-p (regexp-quote "Reached 'e") msg) "Connected to LSP.")
                    (t msg))))))

  (advice-add 'jsonrpc--message :override
              (lambda (format &rest args)
                (let ((msg (apply #'format format args)))
                  (message
                   (cond
                    ((string-match-p (regexp-quote "Server exited with status") msg) "Disconnected from LSP.")
                    (t msg))))))

  (defun eglot-describe-session ()
    (interactive)
    (message (format "%s" (eglot--server-info (eglot-current-server)))))

  (setopt eglot-managed-mode-hook (list (lambda () (eldoc-mode -1)))
          eglot-events-buffer-config '(:size 0 :format full)
          ;; eglot-events-buffer-config '(:size 2000000 :format full)
          eglot-sync-connect nil
          eglot-max-file-watches 20
          ;; don't tell server of changes before Emacs's been idle for this many seconds:
          ;; increase from 0.5 s to reduce chatter
          eglot-send-changes-idle-time 1
          eglot-prefer-plaintext nil
          eglot-autoshutdown t
          ;; If non-nil, allow watching files outside project root.
          eglot-watch-files-outside-project-root nil)

  (add-hook 'eglot-managed-mode-hook 'eglot-inlay-hints-mode t)

  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio"))

  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "ty" "server"))

  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "rass" "python"))

  (unless (executable-find "emacs-lsp-booster")
    (warn "emacs-lsp booster not found! install with emacs-lsp-booster-install"))

  (add-to-list 'eglot-server-programs
               '(vhdl-mode . ("ghdl-ls"))))

;;------------------------------------------------------------------------------
;; Eglot Booster
;;------------------------------------------------------------------------------

(use-package! eglot-booster
  :after eglot
  :custom
  (eglot-booster-no-remote-boost t)
  (eglot-booster-io-only t)
  :init
  (cl-remprop 'buffer-local-value 'byte-obsolete-generalized-variable)
  :config
  (eglot-booster-mode))
