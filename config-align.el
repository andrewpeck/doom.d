;; -*- lexical-binding: t; -*-

;;; Alignment
;;------------------------------------------------------------------------------

(after! align
  ;; Alignment functions
  ;; (defun align-to-colon (begin end)
  ;;   "Align region to colon (:) signs"
  ;;   (interactive "r")
  ;;   (align-regexp begin end
  ;;                 (rx (group (zero-or-more (syntax whitespace))) ":") 1 1 ))
  ;;
  ;; (defun align-to-comma (begin end)
  ;;   "Align region to comma  signs"
  ;;   (interactive "r")
  ;;   (align-regexp begin end
  ;;                 (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 t ))
  ;;

  (defun align-& (start end)
    "Align columns by ampersand"
    (interactive "r")
    (align-regexp start end
                  "\\(\\s-*\\)&" 1 1 t))

  (defun align-= (start end)
    "Align columns by ampersand"
    (interactive "r")
    (align-regexp start end
                  "\\(\\s-*\\)=" 1 1 t))

  ;;
  ;; ;; http://pragmaticemacs.com/emacs/aligning-text/
  ;; (defun bjm/align-comma (start end)
  ;;   "Align columns by ampersand"
  ;;   (interactive "r")
  ;;   (align-regexp start end
  ;;                 "\\(\\s-*\\),\\(\\s-*\\)" 1 1 t))
  ;;
  ;; (defun align-to-equals (begin end)
  ;;   "Align region to equal signs"
  ;;   (interactive "r")
  ;;   (align-regexp begin end
  ;;                 (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 ))
  ;;
  ;; (defun align-to-hash (begin end)
  ;;   "Align region to hash ( => ) signs"
  ;;   (interactive "r")
  ;;   (align-regexp begin end
  ;;                 (rx (group (zero-or-more (syntax whitespace))) "=>") 1 1 ))
  ;;
  ;; ; (defun align-regexp (beg end regexp &optional group spacing repeat)
  ;; ;; work with this
  ;; (defun align-to-comma-before (begin end)
  ;;   "Align region to equal signs"
  ;;   (interactive "r")
  ;;   (align-regexp begin end
  ;;                 (rx (group (zero-or-more (syntax whitespace))) ",") 1 1 t))
  ;;
  ;; ;; https://www.reddit.com/r/emacs/comments/6pak1o/configuration_for_alignment_commands/
  ;; (defun align-whitespace (start end)
  ;;   "Align columns by whitespace"
  ;;   (interactive "r")
  ;;   (align-regexp start end
  ;;                 "\\(\\s-*\\)\\s-" 1 0 t))

  (defun align-whitespace (start end)
    "Align columns by whitespace"
    (interactive "r")
    (align-regexp start end
                  "\\(\\s-*\\)\\s-" 1 0 t))

;;;###autoload
  ;;(defun ap/align (start end x)
  ;;  "Align"
  ;;  (interactive
  ;;   (let ((string (read-string "Foo: " nil 'my-history)))
  ;;     ;;(list (region-beginning) (region-end) string)
  ;;     (align-regexp (region-beginning) (region-end) "\\(\\s-*\\) &" 1 1 t)
  ;;
  ;;     ))

  ;;(interactive
  ;; (let ((string (read-string "Align regexp: ")))
  ;;   ;;(print start)
  ;;   ;;(print end)
  ;;   (print x)
  ;;   ;;(align-regexp start end "\\(\\s-*\\)                   &") 1 1 t))
  ;;   )
  ;; )
  ;; )

  ;;(evil-ex-define-cmd "Tab[ular]" 'ap/align)

  (defun align-ampersand (start end)
    "Align columns by ampersand"
    (interactive "r")
    (align-regexp start end
                  "\\(\\s-*\\)&" 1 1 t))

  (defun align-quote-space (start end)
    "Align columns by quote and space"
    (interactive "r")
    (align-regexp start end
                  "\\(\\s-*\\).*\\s-\"" 1 0 t))

  (defun align-equals (start end)
    "Align columns by equals sign"
    (interactive "r")
    (align-regexp start end
                  "\\(\\s-*\\)=" 1 0 t))

  (defun align-comma (start end)
    "Align columns by comma"
    (interactive "r")
    (align-regexp start end
                  "\\(\\s-*\\)," 1 1 t))

  (defun align-dot (start end)
    "Align columns by dot"
    (interactive "r")
    (align-regexp start end
                  "\\(\\s-*\\)\\\." 1 1 t))
  )
