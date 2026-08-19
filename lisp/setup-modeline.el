;; -*- lexical-binding: t; -*-

;; https://www.emacswiki.org/emacs/ModeLineConfiguration
;; https://www.emacswiki.org/emacs/PercentConstruct

(use-package nyan-mode
  :config
  (setq nyan-animate-nyancat t
        nyan-animation-frame-interval 0.1
        nyan-bar-length 16
        nyan-wavy-trail t))

(defvar modeline-show-flycheck-names nil)

(defvar-local modeline--flycheck-status ""
  "Cached mode line string describing the current flycheck state.
Recomputed by `modeline-flycheck-update-status', never in the mode line
itself.")

(after! flycheck
  (defsubst modeline-flycheck-state ()
    ""
    (if-let* ((status (flycheck-count-errors flycheck-current-errors)))
        (let-alist status
          (concat (propertize (format "%s" (or .error "0") ) 'face '(:inherit error))
                  "·"
                  (propertize (format "%s" (or .warning "0")) 'face '(:inherit warning))))
      "")) ;; no errors or warnings

  (defsubst my-flycheck-mode-line-status-text ()
    "Get a text describing STATUS for use in the mode line.
STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
    (pcase flycheck-last-status-change
      ('not-checked "")
      ('no-checker  "")
      ('running     "󰔟")
      ('errored     "")
      ('interrupted "")
      ('suspicious  "")
      ('finished     (modeline-flycheck-state))))

  ;; PERF: `flycheck-count-errors' walks the whole error list. Calling it from
  ;; the mode line's :eval re-counted every error on every redisplay, for every
  ;; window (up to `flycheck-checker-error-threshold' = 1000 errors). Compute it
  ;; only when flycheck reports a change.
  (defun modeline-flycheck-update-status (&rest _)
    "Refresh `modeline--flycheck-status' for the current buffer."
    (setq modeline--flycheck-status (my-flycheck-mode-line-status-text))
    (force-mode-line-update))

  (add-hook 'flycheck-status-changed-functions #'modeline-flycheck-update-status)
  (add-hook 'flycheck-after-syntax-check-hook #'modeline-flycheck-update-status))

(after! vc-git
  (defsubst advice/vc-mode-line-transform (tstr)
    ;; Start with e.g. Git:master
    ;; strip off Git to yield :master
    ;; take first character to get -
    (let* ((tstr (replace-regexp-in-string "Git" "" tstr))
           (first-char (substring tstr 0 1))
           (modified (string= first-char ":"))
           (face (if modified 'diff-removed 'diff-added))
           (tstr (substring tstr 1 nil))
           (icon (propertize "󰊢" 'face `(:foreground ,(face-attribute face :foreground)))))
      (concat icon " " tstr)))

  ;; https://emacs.stackexchange.com/questions/10955/customize-vc-mode-appearance-in-mode-line
  (advice-add #'vc-git-mode-line-string :filter-return #'advice/vc-mode-line-transform))

;;------------------------------------------------------------------------------
;; Keeping the branch fresh
;;------------------------------------------------------------------------------

;; `vc-mode' -- the branch shown above -- is only recomputed by
;; `vc-refresh-state', which normally runs on `find-file-hook' and on revert.
;; Switching branches without changing a file's contents therefore leaves the
;; mode line stale until the buffer is re-visited.
;;
;; `auto-revert-check-vc-info' would fix that by refreshing on every poll, but
;; it does so for *every* file buffer on every tick (~7ms of git each). Refresh
;; just the buffer being looked at instead, debounced so that cycling through
;; buffers (e.g. `buffer-flip') doesn't spawn a git process per buffer.

(defvar my/vc-refresh-timer nil
  "Idle timer used to debounce `my/vc-refresh-state-soon'.")

(defun my/vc-refresh-state-soon (&rest _)
  "Refresh `vc-mode' for the current buffer once Emacs goes idle."
  (when (timerp my/vc-refresh-timer)
    (cancel-timer my/vc-refresh-timer))
  (setq my/vc-refresh-timer
        (run-with-idle-timer
         0.3 nil
         (lambda (buf)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               ;; the advice in setup-tramp.el keeps this off remote buffers
               (when buffer-file-name
                 (vc-refresh-state)))))
         (current-buffer))))

(add-hook 'doom-switch-buffer-hook #'my/vc-refresh-state-soon)
(add-hook 'doom-switch-window-hook #'my/vc-refresh-state-soon)

(defun my/vc-refresh-visible-buffers (&rest _)
  "Refresh `vc-mode' in every visible file buffer.
Used after magit operations, which can change the branch without changing
any file on disk."
  (dolist (win (window-list))
    (with-current-buffer (window-buffer win)
      (when buffer-file-name
        (vc-refresh-state)))))

(after! magit
  (add-hook 'magit-post-refresh-hook #'my/vc-refresh-visible-buffers))

;;------------------------------------------------------------------------------
;; Segments
;;------------------------------------------------------------------------------

;; PERF: every `(:eval FORM)' in `mode-line-format' is handed to `eval', so FORM
;; is *macroexpanded from scratch* on every redisplay, for every window. A CPU
;; profile showed `cconv-make-interpreted-closure', `macroexpand-all' and
;; `internal--build-bindings' (from `and-let*') running under
;; `redisplay_internal' for exactly this reason. Keeping each `:eval' down to a
;; single funcall moves all of that to load time.
;;
;; Corollary: never put a literal `(lambda ...)' inside an `:eval' form -- it is
;; rebuilt and re-macroexpanded every redisplay. Use a named function instead.

(defvar-local modeline--remote-host-cache nil
  "Cons of (DIRECTORY . HOST) caching `remote-host?' for the mode line.")

(defun modeline-remote-host ()
  "Return `remote-host?' of `default-directory', cached per buffer.
`file-remote-p' consults every entry of `file-name-handler-alist', which is too
much work to repeat for each window on each redisplay."
  (let ((dir default-directory))
    (unless (equal (car modeline--remote-host-cache) dir)
      (setq modeline--remote-host-cache (cons dir (remote-host? dir))))
    (cdr modeline--remote-host-cache)))

(defun modeline-remote ()
  (when-let* ((host (modeline-remote-host)))
    (concat (propertize host 'face '(:inherit warning)) ":")))

(defvar modeline-buffer-id (propertized-buffer-identification "%b")
  "Pre-propertized buffer name.
The properties never change, so cons the string once rather than per redisplay.")

(defun modeline-vc ()
  (and (not (modeline-remote-host))
       vc-mode
       (concat " (" (string-trim vc-mode) ")")))

;; PERF: read which-func's own cache (refreshed on an idle timer by
;; `which-func-update') rather than calling `which-function' -- i.e. imenu -- on
;; every redisplay.
(defun modeline-which-func ()
  (and (bound-and-true-p which-func-mode)
       (let ((fn (gethash (selected-window) which-func-table)))
         (and fn
              ;; escape % so it isn't read as a mode line spec
              (concat "[" (propertize (string-replace "%" "%%" fn)
                                      'face 'which-func
                                      'local-map which-func-keymap
                                      'mouse-face 'mode-line-highlight)
                      "]")))))

(defun modeline-nyan ()
  (and (bound-and-true-p nyan-mode)
       (concat " " (nyan-create))))

(defun modeline-macro ()
  (and (or defining-kbd-macro executing-kbd-macro)
       (concat " (MACRO " (char-to-string evil-this-macro) ")")))

;;------------------------------------------------------------------------------
;; venv
;;------------------------------------------------------------------------------

(defun modeline-venv-help-echo (window _object _pos)
  "Resolve the venv tooltip on hover, keeping `abbreviate-file-name' off the
redisplay path."
  (with-current-buffer (window-buffer window)
    (if (bound-and-true-p buffer-env-active)
        (abbreviate-file-name buffer-env-active)
      "")))

(defvar modeline--venv-indicator
  (propertize " " 'help-echo #'modeline-venv-help-echo))

(defvar modeline--no-venv-indicator
  (propertize "No venv " 'face 'error))

(defun modeline-venv ()
  (cond ((bound-and-true-p buffer-env-active) modeline--venv-indicator)
        ((memq major-mode '(python-ts-mode python-mode)) modeline--no-venv-indicator)))

;;------------------------------------------------------------------------------
;; lsp
;;------------------------------------------------------------------------------

(defun modeline-lsp-help-echo (window _object _pos)
  "Format the (large) server-info plist only when the tooltip is shown."
  (with-current-buffer (window-buffer window)
    (format "%s" (eglot--server-info (eglot-current-server)))))

(defvar-local modeline--lsp-segment nil
  "Cached mode line LSP indicator.
Recomputed by `modeline-lsp-update-segment' when eglot attaches or detaches,
never in the mode line itself -- `eglot--server-info' walks an eieio object.")

(defun modeline-lsp-update-segment ()
  "Refresh `modeline--lsp-segment' for the current buffer."
  (setq modeline--lsp-segment
        (and (fboundp #'eglot-managed-p)
             (eglot-managed-p)
             (let* ((lsp-server-info (eglot--server-info (eglot-current-server)))
                    (lsp-server-name (plist-get lsp-server-info :name))
                    (icon (pcase lsp-server-name
                            ("basedpyright" "󱔎 " )
                            ("ty" " ")
                            ("rustanalyzer" " ")
                            ("rass" " ")
                            ("pyrefly-lsp" "🦋 ")
                            ("ty+pyrefly-lsp" " ")
                            ("slang-server" " ")
                            (_ " "))))
               (propertize icon 'help-echo #'modeline-lsp-help-echo))))
  (force-mode-line-update))

(add-hook 'eglot-managed-mode-hook #'modeline-lsp-update-segment)

;;------------------------------------------------------------------------------
;; flycheck / position
;;------------------------------------------------------------------------------

(defun modeline-flycheck ()
  (and (bound-and-true-p flycheck-mode)
       (bound-and-true-p flycheck-enabled-checkers)
       (if modeline-show-flycheck-names
           (format "(%s) %s "
                   (string-join (mapcar #'symbol-name flycheck-enabled-checkers) " ")
                   modeline--flycheck-status)
         (concat modeline--flycheck-status " "))))

(defun modeline-position ()
  (if (eq major-mode 'pdf-view-mode)
      (format "%s / %s " (pdf-view-current-page) (pdf-cache-number-of-pages))
    ;; constant string -- redisplay expands the %-constructs itself
    "(L%l C%c %p) "))

;;------------------------------------------------------------------------------
;; Format
;;------------------------------------------------------------------------------

(setq-default mode-line-format
              '(
                ;;LEFT
                ;; mode-line-front-space
                "%e"
                evil-mode-line-tag
                mode-line-mule-info
                "%* "

                (:eval (modeline-remote))

                modeline-buffer-id

                ;; git
                (:eval (modeline-vc))

                " " (:eval (modeline-which-func))

                (:eval (modeline-nyan))

                (:eval (modeline-macro))

                ;; RIGHT PAD
                mode-line-format-right-align

                ;; RIGHT

                ;; venv
                (:eval (modeline-venv))

                ;; lsp
                modeline--lsp-segment

                ;; flycheck
                (:eval (modeline-flycheck))

                ;; position
                (:eval (modeline-position))))

(which-function-mode 1)
