;; -*- lexical-binding: t; -*-

;; https://www.emacswiki.org/emacs/ModeLineConfiguration
;; https://www.emacswiki.org/emacs/PercentConstruct

(display-battery-mode)
(setq battery-mode-line-format "%b%p%% · ")

(setq nyan-animate-nyancat t
      nyan-animation-frame-interval 0.1
      nyan-bar-length 16
      nyan-wavy-trail t)

(defsubst simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(defsubst modeline-flycheck-state ()
  ""
  (let-alist (flycheck-count-errors flycheck-current-errors)
    (if (not (or .error .warning))
        ;; no errors or warnings
        ""
      ;; else
      (concat
       (propertize (format "%s" (or .error "0") ) 'face '(:inherit error))
       "·"
       (propertize (format "%s" (or .warning "0")) 'face '(:inherit warning))))))

(defsubst my-flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.
STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (concat
   (pcase (or status flycheck-last-status-change)
     (`not-checked "")
     (`no-checker  "")
     (`running     "󰔟")
     (`errored     "")
     (`interrupted "")
     (`suspicious  "")
     (`finished     (modeline-flycheck-state)))
   " "))

(after! vc-git
  (defsubst advice/vc-mode-line-transform (tstr)
    (let* ((tstr (replace-regexp-in-string "Git" "" tstr))
           (first-char (substring tstr 0 1)))
      (cond ((string= ":" first-char) ;;; Modified
             (replace-regexp-in-string "^:"
                                       (propertize "󰊢 " 'face `(:foreground ,(face-attribute 'diff-removed :foreground)))
                                       tstr))
            ((string= "-" first-char) ;; No change
             (replace-regexp-in-string "^-" (propertize "󰊢 " 'face `(:foreground ,(face-attribute 'diff-added :foreground))) tstr))
            (t tstr))))
  ;; https://emacs.stackexchange.com/questions/10955/customize-vc-mode-appearance-in-mode-line
  (advice-add #'vc-git-mode-line-string :filter-return #'advice/vc-mode-line-transform))

(setq eglot-menu-string "⌁")
(custom-set-faces '(eglot-mode-line ((t))))

(setq-default mode-line-format

              '((:eval (simple-mode-line-render

                        ;; Left.
                        (list "%e"
                              evil-mode-line-tag
                              mode-line-mule-info
                              "%* "
                              (when-let* ((host (remote-host? default-directory)))
                                (concat (propertize host 'face '(:inherit warning)) ":"))

                              (propertized-buffer-identification "%b")

                              (when nyan-mode
                                (concat "  " (nyan-create))))

                        ;; Right.
                        (list ""

                         (when (or defining-kbd-macro executing-kbd-macro)
                           (concat "MACRO(" (char-to-string evil-this-macro) ") ⋅ "))

                         (and flycheck-mode flycheck-enabled-checkers
                              (concat "("
                                      (string-join
                                       (mapcar 'symbol-name flycheck-enabled-checkers) " ") ") "))

                         ;; mode-line-misc-info
                         ;; global-mode-string
                         ;; '("" battery-mode-line-string)

                         (pcase major-mode
                           ('pdf-view-mode (format "%s / %s" (pdf-view-current-page) (pdf-cache-number-of-pages)))
                           (_  "(L%l C%c %p)"))

                         (when (not (remote-host? default-directory))
                           (when-let ((m (vc-mode)))
                             (concat " (" (string-trim m) ") ")))

                         ;; (format "%s" (if (listp mode-name) (car mode-name) mode-name))

                         (when buffer-env-active " ")

                         ;; replace (eglot--mode-line-format)
                         (when (and (fboundp #'eglot-managed-p)
                                    (eglot-managed-p)) " ")

                         (when flycheck-mode
                           (concat " "
                                   (replace-regexp-in-string
                                    "FlyC" ""
                                    (my-flycheck-mode-line-status-text)))))))))
