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

(setq-default mode-line-format
              '(
                ;;LEFT
                ;; mode-line-front-space
                "%e"
                evil-mode-line-tag
                mode-line-mule-info
                "%* "

                (:eval (and-let* ((host (remote-host? default-directory)))
                         (concat (propertize host 'face '(:inherit warning)) ":")))

                (:eval (propertized-buffer-identification "%b"))

                ;; git
                (:eval (and-let* ((m (and (not (remote-host? default-directory)) vc-mode)))
                         (concat " (" (string-trim m) ")")))


                " " which-func-format

                (:eval (and nyan-mode
                            (concat " " (nyan-create))))
                
                (:eval (and (or defining-kbd-macro executing-kbd-macro)
                            (concat " (MACRO " (char-to-string evil-this-macro) ")")))

                ;; RIGHT PAD
                mode-line-format-right-align

                ;; RIGHT

                ;; venv
                (:eval
                 (or (and buffer-env-active
                          (propertize " " 'help-echo (abbreviate-file-name buffer-env-active) ))
                     (and (or (eq major-mode 'python-ts-mode)
                              (eq major-mode 'python-mode))
                          (propertize  "No venv " 'face 'error))))

                ;; lsp
                (:eval (and (fboundp #'eglot-managed-p)
                            (eglot-managed-p)
                            (let* ((lsp-server-info (eglot--server-info (eglot-current-server)))
                                   (lsp-server-name (nth 1 lsp-server-info))
                                   (icon (pcase lsp-server-name
                                           ("basedpyright" "󱔎 " )
                                           ("ty" " ")
                                           ("rustanalyzer" " ")
                                           ("rass" " ")
                                           ("pyrefly-lsp" "🦋 ")
                                           ("ty+pyrefly-lsp" " ")
                                           ("slang-server" " ")
                                           (_ " "))))
                              (propertize icon 'help-echo (format "%s" lsp-server-info)))))

                ;; flycheck
                (:eval (and flycheck-mode flycheck-enabled-checkers
                            (let ((status (my-flycheck-mode-line-status-text)))

                              (if modeline-show-flycheck-names
                                  (let ((checkers (string-join (mapcar 'symbol-name flycheck-enabled-checkers) " ")))
                                    (format "(%s) %s " checkers status))
                                (concat status " ")))))

                ;; position
                (:eval (let ((page (pcase major-mode
                                     ('pdf-view-mode (format "%s / %s" (pdf-view-current-page) (pdf-cache-number-of-pages)))
                                     (_  "(L%l C%c %p)"))))
                         (concat page " ")))))

(which-function-mode 1)
