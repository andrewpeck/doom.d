(after! minibuffer-line-mode
  (minibuffer-line-mode 1)
  (setq minibuffer-line-format
        '(""
          (:eval system-name)
          " | "
          (:eval
           (format-time-string "%F %R")))))

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

(setq mode-line-buffer-identification
      (propertized-buffer-identification "%b"))

(setq flycheck-mode-line '(:eval (replace-regexp-in-string
                                  "FlyC" ""
                                  (flycheck-mode-line-status-text))))

'(:eval (propertize (concat "\t[" mode-name "] %l:%i\t") 'face '(:foreground "black" :height 0.9 :weight normal)
                    'help-echo (buffer-file-name)))

(defun flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.
STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (pcase (or status flycheck-last-status-change)
    (`not-checked "∄")
    (`no-checker "✗")
    (`running "⟳")
    (`errored "‼")
    (`finished
     (let-alist (flycheck-count-errors flycheck-current-errors)
       (if (or .error .warning)
           (concat
            "" (propertize (format "%s" (or .error 0) ) 'face '(:inherit error))
            "|" (propertize (format "%s" (or .warning 0)) 'face '(:inherit warning)))
         "✓")))
    (`interrupted ".")
    (`suspicious "?")))

(setq mode-line-format
      '((:eval (simple-mode-line-render
                ;; Left.
                `("%e"
                  evil-mode-line-tag
                  mode-line-mule-info
                  "%* "
                  mode-line-buffer-identification)

                ;; Right.
                `(,(if (or defining-kbd-macro executing-kbd-macro)
                       (concat "MACRO(" (char-to-string evil-this-macro) ") · ") "")

                  "L%l⸱C%c⸱%p"

                  ,(if vc-mode
                     (concat " ·" vc-mode " · ") " · ")

                  ,(format "%s" (if (listp mode-name) (car mode-name) mode-name))
                  " · "

                  flycheck-mode-line " ")))))

(setq-default mode-line-format mode-line-format)
