;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Rainbow Delimeters
;;------------------------------------------------------------------------------

(after! rainbow-delimiters-mode
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;------------------------------------------------------------------------------
;; True/False Faces
;;------------------------------------------------------------------------------

(defface my/highlight-true-face
  '((t :foreground "#2a2" :weight bold))
  "Highlight face for true"
  :group 'basic-faces)

(defface my/highlight-false-face
  '((t :foreground "#f22" :weight bold))
  "Highlight face for false"
  :group 'basic-faces)

(defun my/highlight-true ()
  "Use hi-lock to highlight specific words"
  (hi-lock-face-buffer "\\b\\(true\\|True\\)\\b" 'my/highlight-true-face))

(defun my/highlight-false ()
  "Use hi-lock to highlight specific words"
  (hi-lock-face-buffer "\\b\\(false\\|False\\)\\b" 'my/highlight-false-face))

(add-hook 'vhdl-mode-hook #'my/highlight-false)
(add-hook 'vhdl-mode-hook #'my/highlight-true)

(custom-set-faces
 '(writegood-weasels-face ((t (:underline (:color "#888888" :style wave))))))

;; override some annoying defaults of doom-gruvbox
(custom-theme-set-faces! 'doom-gruvbox
  ;; the default function name face is the same as the string face and looks awful for lisp
  '(font-lock-function-name-face :foreground "#d3869b")
  '(whitespace-tab :background "#282828")
  '(org-todo :weight bold :foreground "orange red")
  '(org-table :weight normal :foreground "lightblue")
  '(org-link :weight normal :underline "lightpink" :foreground "light pink")
  '(org-level-1 :height 1.5 :weight bold   :foreground "#b8bb26")
  '(org-level-2 :height 1.2 :weight bold   :foreground "lightblue")
  '(org-level-3 :height 1.1 :weight normal :foreground "#d3869b")
  '(org-level-4 :height 1.1 :weight normal :foreground "#83a598"))

(custom-theme-set-faces! 'doom-spacegrey
  '(org-todo :weight bold :foreground "orange red")
  '(org-table :weight normal :foreground "lightblue")
  '(org-link :weight normal :underline "lightpink" :foreground "light pink")
  '(org-level-1 :height 1.5 :weight bold   :foreground "#c0c5ce")
  '(org-level-2 :height 1.2 :weight bold   :foreground "lightblue")
  '(org-level-3 :height 1.1 :weight normal :foreground "#d3869b")
  '(org-level-4 :height 1.1 :weight normal :foreground "#83a598"))

(custom-theme-set-faces! 'hima
  '(org-level-1 :height 1.0 :weight bold)
  '(org-level-2 :height 1.0 :weight bold)
  '(org-level-3 :height 1.0 :weight bold)
  '(org-level-4 :height 1.0 :weight bold))

(custom-theme-set-faces! 'solarized-zenburn
  '(font-lock-doc-face :inherit font-lock-comment-face)
  '(font-lock-string-face :inherit error)
  '(font-lock-keyword-face :foreground "#7F9F7F")
  '(font-lock-constant-face :foreground "#7F9F7F")
  '(font-lock-variable-name-face :foreground "#8CD0D3" :weight normal)
  '(flyspell-incorrect :underline (:style wave :color "#CC9393") )
  '(flycheck-error :underline (:style wave :color "#552424") )
  '(flycheck-info :underline (:style wave :color "#75898a") )
  '(highlight-quoted-symbol :inherit font-lock-variable-name-face :slant italic)
  '(font-latex-italic-face :inherit default :slant italic)
  '(font-latex-bold-face :inherit default :weight bold))

(custom-theme-set-faces! 'doom-zenburn
  '(font-latex-italic-face :inherit default :slant italic)
  '(font-latex-bold-face :inherit default :weight bold)
  )

(custom-theme-set-faces! 'hima
  '(font-latex-italic-face :inherit default :slant italic)
  '(font-latex-bold-face :inherit default :weight bold)
  '(font-lock-type-face :inherit default :family "Courier New" )
  )
