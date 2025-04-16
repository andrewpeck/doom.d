;; -*- lexical-binding: t; -*-

;; https://www.emacswiki.org/emacs/ModeLineConfiguration
;; https://www.emacswiki.org/emacs/PercentConstruct

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(defun my-flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.
STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (concat
   (pcase (or status flycheck-last-status-change)
     (`not-checked " ")
     (`no-checker " ")                ;✗
     (`running " ")
     (`errored " ")                   ; ‼
     (`interrupted ". ")               ;✓
     (`suspicious "? ")
     (`finished (let-alist (flycheck-count-errors flycheck-current-errors)
                  (if (or .error .warning)
                      (concat
                       (propertize (format "%s" (or .error "0") ) 'face '(:inherit error))
                       "·" (propertize (format "%s" (or .warning "0")) 'face '(:inherit warning))) " "))))  " "))

(after! vc-git
  (defun advice/vc-mode-line-transform (tstr)
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
                      (let ((host (remote-host? default-directory)))
                        (if host
                            (concat (propertize host 'face '(:inherit warning)) ":") nil))

                      (propertized-buffer-identification "%b"))

                ;; Right.
                (list (if (or defining-kbd-macro executing-kbd-macro)
                          (concat "MACRO(" (char-to-string evil-this-macro) ") ⋅ ") "")

                      ;; replace (eglot--mode-line-format)
                      (when (and (fboundp #'eglot-managed-p)
                                 (eglot-managed-p)) "🕷")

                      " L%l·C%c·%p"

                      (if (and (not (remote-host? default-directory)) vc-mode)
                          (concat " ⋅" vc-mode " ⋅ ") " ⋅ ")

                      (format "%s" (if (listp mode-name) (car mode-name) mode-name))

                      (replace-regexp-in-string "FlyC" ""
                                                (concat " ⋅ " (my-flycheck-mode-line-status-text))) " ")))))
