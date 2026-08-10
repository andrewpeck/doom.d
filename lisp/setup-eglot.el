;; -*- lexical-binding: t; -*-

(defun +lsp-shutdown ()
  (interactive)
  (require 'eglot)
  (let ((inhibit-message t))
    (when (bound-and-true-p flycheck-eglot-mode)
      (flycheck-eglot-mode -1))
    (when (bound-and-true-p eglot-inlay-hints-mode)
      (eglot-inlay-hints-mode -1))
    (when-let* ((timer (bound-and-true-p eglot--outstanding-inlay-regions-timer)))
      (cancel-timer timer))
    (when-let* ((current-server (eglot-current-server)))
      (ignore-errors (eglot-shutdown current-server))
      (let ((inhibit-message nil))
        (message "Shut down `%s' language server"
                 (plist-get (eglot--server-info current-server) :name))))))

(defun +lsp-startup ()
  (interactive)
  (require 'eglot)
  (when (+lsp-should-start-p)
    ;; NOTE: `eglot-ensure' captures `current-buffer' when it is *called*, and
    ;; idle timers fire in whatever buffer happens to be current then, so the
    ;; buffer has to be closed over explicitly.
    (let ((buffer (current-buffer)))
      (run-with-idle-timer
       1 nil
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (eglot-ensure))))))))

(defun +lsp-should-start-p (&rest _)
  "Non-nil if eglot should be started in the current buffer.
Python buffers additionally require an active virtualenv, which
`buffer-env' is asked to locate."
  (and (eglot--lookup-mode major-mode)
       (or (not (derived-mode-p 'python-mode 'python-ts-mode))
           (and (require 'buffer-env nil t)
                (progn (buffer-env-update)
                       (bound-and-true-p buffer-env-active)))
           ;; Only complain about the venv; a mode with no registered server
           ;; just isn't an LSP mode and shouldn't produce a message.
           (ignore (message "No virtual environment found. Not starting LSP.")))))

(defun +lsp-restart ()
  (interactive)
  (+lsp-shutdown)
  (+lsp-startup))

;; NOTE: deliberately not `:after eglot' -- `+lsp-startup' is how eglot gets
;; loaded in the first place, so the binding has to exist beforehand.
(map! :leader
      (:prefix ("l" . "LSP")
               "l" #'+lsp-startup
               "r" #'+lsp-restart
               "d" #'+lsp-shutdown))

(use-package! eglot

  :if (modulep! :tools lsp +eglot)

  :init

  ;; Don't auto-start eglot for Python without a venv. Doom's `lsp!' hook calls
  ;; eglot-ensure automatically; this intercepts it before the server spawns.
  (advice-add #'lsp! :override #'+lsp-startup)

  :config

  (defun +eglot--message-a (format &rest args)
    "Override for `eglot--message' that drops the prefix and known noise."
    (let ((msg (apply #'eglot--format format args)))
      ;; `eglot-max-file-watches' is capped below, so big projects legitimately
      ;; hit the limit; don't nag about it.
      (unless (string-match-p "not watching some directories" msg)
        (message "%s" msg))))

  (advice-add 'eglot--message :override #'+eglot--message-a)

  (defun +jsonrpc--message-a (format &rest args)
    "Override for `jsonrpc--message' that drops the prefix and shortens exits."
    (let ((msg (apply #'format format args)))
      (message "%s" (if (string-match-p "Server exited with status" msg)
                        "Disconnected from LSP."
                      msg))))

  (advice-add 'jsonrpc--message :override #'+jsonrpc--message-a)

  ;; Eglot only offers the symbol at point as the minibuffer *default* (M-n);
  ;; pre-fill it as the initial input instead so it can just be edited.
  (defun my/eglot-rename-prefill-symbol (fn &rest args)
    "Around advice for FN to seed `eglot-rename' with the symbol at point."
    (cl-letf* ((read-from-minibuffer (symbol-function 'read-from-minibuffer))
               ((symbol-function 'read-from-minibuffer)
                (lambda (prompt &optional initial keymap read hist default &rest rest)
                  (apply read-from-minibuffer prompt
                         (or initial (if (consp default) (car default) default))
                         keymap read hist default rest))))
      (apply fn args)))

  (advice-add 'eglot--rename-interactive :around #'my/eglot-rename-prefill-symbol)

  (defun eglot-describe-session ()
    (interactive)
    (message "%s" (eglot--server-info (eglot-current-server))))

  ;; NOTE: `eglot-managed-mode-hook' runs when eglot *stops* managing a buffer
  ;; too, so everything on it has to check `eglot-managed-p' first.

  (defun hook/eglot-disable-eldoc ()
    "Silence eldoc under eglot; `eldoc-box' is summoned on demand instead."
    (when (eglot-managed-p)
      (eldoc-mode -1)))

  (add-hook 'eglot-managed-mode-hook #'hook/eglot-disable-eldoc)

  (defun hook/eglot-inlay-hints ()
    (when (eglot-managed-p)
      (eglot-inlay-hints-mode 1)))

  (add-hook 'eglot-managed-mode-hook #'hook/eglot-inlay-hints t)

  ;; NOTE: `eglot-autoshutdown' and `eglot-events-buffer-config' are already set
  ;; by Doom's :tools lsp module; the latter must not be overwritten wholesale
  ;; or `set-debug-var!' loses its handle on it.
  (setopt eglot-sync-connect nil
          ;; The 10000 default costs real time on large repos. 1000 still covers
          ;; ordinary projects; when it is exceeded eglot registers no watches
          ;; at all and warns (suppressed in `+eglot--message-a' above).
          eglot-max-file-watches 1000
          ;; don't tell server of changes before Emacs's been idle for this many
          ;; seconds: increase from 0.5 s to reduce chatter
          eglot-send-changes-idle-time 1
          ;; If non-nil, allow watching files outside project root.
          eglot-watch-files-outside-project-root nil)

  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("rass" "python")))

  (add-to-list 'eglot-server-programs
               '(vhdl-mode . ("ghdl-ls"))))

;;------------------------------------------------------------------------------
;; Eldoc-Box
;;------------------------------------------------------------------------------

(defun my/eldoc-box-help-at-mouse (event)
  "Move point to mouse EVENT and show fresh eldoc-box docs there."
  (interactive "e")
  (let* ((posn (event-start event))
         (win  (posn-window posn))
         (pos  (posn-point posn)))
    (when (and (window-live-p win)
               (integer-or-marker-p pos))
      (select-window win)
      (with-current-buffer (window-buffer win)
        (goto-char pos)
        ;; Clear stale ElDoc state and ask providers again.
        (when (fboundp 'eldoc)
          (eldoc))
        ;; Give async providers/Eglot/LSP a moment to update the doc buffer.
        (run-at-time
         0.05 nil
         (lambda (buf win pos)
           (when (and (buffer-live-p buf)
                      (window-live-p win))
             (select-window win)
             (with-current-buffer buf
               (goto-char pos)
               (eldoc-box-help-at-point))))
         (current-buffer) win pos)))))

;; NOTE: no `eldoc-box-*-mode' is enabled on purpose -- eldoc is off under eglot
;; (see `hook/eglot-disable-eldoc'), and `eldoc-box-help-at-point' works without
;; it, so docs are strictly on demand via the binding below.
(use-package! eldoc-box
  :bind
  ("C-<down-mouse-1>" . my/eldoc-box-help-at-mouse))
