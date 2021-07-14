;; -*- lexical-binding: t; -*-

;; override some annoying defaults of doom-gruvbox
(custom-theme-set-faces! 'doom-gruvbox
  '(org-todo :weight bold :foreground "orange red")
  '(org-table :weight normal :foreground "lightblue")
  '(org-link :weight normal :underline "lightpink" :foreground "light pink")
  '(org-level-1 :height 1.5 :weight bold   :foreground "#b8bb26")
  '(org-level-2 :height 1.2 :weight bold   :foreground "lightblue")
  '(org-level-3 :height 1.1 :weight normal :foreground "#d3869b")
  '(org-level-4 :height 1.1 :weight normal :foreground "#83a598"))


(defun ap/toggle-theme ()
  (interactive)
  (if (eq doom-theme 'summerfruit)
      (progn
        (setq highlight-indent-guides-auto-enabled nil)
        (setq highlight-indent-guides-responsive "stack")
        (setq doom-theme 'doom-gruvbox)
        (load-theme 'doom-gruvbox t)
        (set-face-foreground 'highlight-indent-guides-character-face "#375c3c644822"))

    (progn
      (setq highlight-indent-guides-auto-enabled nil)
      (setq highlight-indent-guides-responsive "stack")
      (setq doom-theme 'summerfruit)
      (load-theme 'summerfruit t)
      (set-face-foreground 'highlight-indent-guides-character-face "#efefef"))))

(map! :leader :desc "Toggle Themes" "t t" #'ap/toggle-theme)
