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
      ('finished     (modeline-flycheck-state)))))

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
                         (concat " (" (string-trim m) ")" " ")))

                (:eval (and nyan-mode
                            (concat " " (nyan-create))))
                
                (:eval (and (or defining-kbd-macro executing-kbd-macro)
                            (concat " (MACRO " (char-to-string evil-this-macro) ")")))

                ;; RIGHT PAD
                mode-line-format-right-align

                ;; RIGHT

                ;; lsp
                (:eval (and (fboundp #'eglot-managed-p)
                            (eglot-managed-p)
                            (let* ((lsp-server-info (eglot--server-info (eglot-current-server)))
                                   (lsp-server-name (nth 1 lsp-server-info))
                                   (icon (pcase lsp-server-name
                                           ("pyrefly-lsp" " " )
                                           ("basedpyright" " " )
                                           ("ty" " ")
                                           ("rustanalyzer" " ")
                                           (_ (concat server "  ")))))
                              (propertize icon 'help-echo (format "%s" lsp-server-info)))))

                ;; venv
                (:eval
                 (and buffer-env-active
                      (propertize " " 'help-echo (abbreviate-file-name buffer-env-active) )))

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
