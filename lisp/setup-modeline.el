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

(defsubst modeline-flycheck-state ()
  ""
  (string-trim
   (let-alist (flycheck-count-errors flycheck-current-errors)
     (if (not (or .error .warning))
         ;; no errors or warnings
         ""
       ;; else
       (concat
        (propertize (format "%s" (or .error "0") ) 'face '(:inherit error))
        "·"
        (propertize (format "%s" (or .warning "0")) 'face '(:inherit warning)))))))

(defsubst my-flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.
STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (pcase (or status flycheck-last-status-change)
    (`not-checked "")
    (`no-checker  "")
    (`running     "󰔟")
    (`errored     "")
    (`interrupted "")
    (`suspicious  "")
    (`finished     (modeline-flycheck-state))))

(after! vc-git
  (defsubst advice/vc-mode-line-transform (tstr)
    ;; Start with e.g. Git:master
    ;; strip off Git to yield :master
    ;; take first character to get -
    (let* ((tstr (replace-regexp-in-string "Git" "" tstr))
           (first-char (substring tstr 0 1))
           (modified (string= first-char ":"))
           (face (if modified 'diff-removed 'diff-added)))
      (substring (propertize tstr 'face `(:foreground ,(face-attribute face :foreground))) 1 nil)))

  ;; https://emacs.stackexchange.com/questions/10955/customize-vc-mode-appearance-in-mode-line
  (advice-add #'vc-git-mode-line-string :filter-return #'advice/vc-mode-line-transform))

(setq eglot-menu-string "⌁")
(custom-set-faces '(eglot-mode-line ((t))))

(setq mode-line-format
      '(
        ;;LEFT
        "%e"
        evil-mode-line-tag
        mode-line-mule-info
        "%* "

        (:eval (when-let* ((host (remote-host? default-directory)))
                 (concat (propertize host 'face '(:inherit warning)) ":")))

        (:eval (propertized-buffer-identification "%b"))

        (:eval (when nyan-mode
                 (concat " " (nyan-create))))

        ;; RIGHT PAD
        mode-line-format-right-align

        ;; RIGHT

        (:eval
         (string-join 
          (remq nil 
                (list
                 (when (or defining-kbd-macro executing-kbd-macro)
                   (concat "MACRO(" (char-to-string evil-this-macro) ") ⋅"))

                 (and flycheck-mode flycheck-enabled-checkers
                      (concat (when modeline-show-flycheck-names
                                (concat
                                 "("
                                 (string-join (mapcar 'symbol-name flycheck-enabled-checkers) " ") " "))

                              (my-flycheck-mode-line-status-text)

                              (when modeline-show-flycheck-names ")")))

                 (pcase major-mode
                   ('pdf-view-mode (format "%s / %s" (pdf-view-current-page) (pdf-cache-number-of-pages)))
                   (_  "(L%l C%c %p)"))

                 (when (not (remote-host? default-directory))
                   (when-let ((m vc-mode))
                     (concat "(" (string-trim m) ")")))

                 ;; (format "%s" (if (listp mode-name) (car mode-name) mode-name))

                 (when buffer-env-active "")

                 ;; replace (eglot--mode-line-format)
                 (when (and (fboundp #'eglot-managed-p)
                            (eglot-managed-p)) "")))
          " "))

        ;; END
        " " mode-line-end-spaces))
