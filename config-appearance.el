;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;;; Appearance
;;------------------------------------------------------------------------------

(after! highlight-indent-guides
  ;;(setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-responsive nil
        highlight-indent-guides-method 'bitmap))

;; All the icons
(use-package! all-the-icons
  :defer-incrementally t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (all-the-icons-ibuffer-mode 1))

(after! ivy
  (all-the-icons-ivy-setup))

;; Theme
(add-to-list 'load-path "~/.doom.d/themes/")
(add-to-list 'custom-theme-load-path "~/.doom.d/themes/")

(cond
 ((string= (system-name) "strange") (setq doom-theme 'doom-material))
 ((string= (system-name) "pepper")  (setq doom-theme 'doom-gruvbox))
 ((string= (system-name) "larry")   (setq doom-theme 'doom-spacegrey))
 (t (setq doom-theme 'doom-one)))

;;------------------------------------------------------------------------------
;;; FONT
;;------------------------------------------------------------------------------

;; (setq my-font "Inconsolata")
;; (setq my-font "JetBrains Mono")
;; (setq my-font "IBM Plex Mono")
;; (setq my-font "Fira Code")
(setq my-font "Roboto Mono")

(setq doom-font (font-spec :family my-font :size 15)
      doom-big-font (font-spec :family my-font :size 14)
      doom-variable-pitch-font (font-spec :family my-font  :size 17)
      ;;doom-variable-pitch-font (font-spec :family "Comic Sans MS"   :size 17)
      doom-serif-font (font-spec :family my-font :weight 'light))

;;------------------------------------------------------------------------------
;; Syntax Highlighting
;;------------------------------------------------------------------------------

;; enable syntax highlighting for vimrc files
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

(add-to-list 'auto-mode-alist '("\\.xdc\\'" . vivado-mode))  ; tcl mode for xdc files
(add-to-list 'auto-mode-alist '("\\.ltx\\'" . json-mode))    ; json mode for ltx files
(add-to-list 'auto-mode-alist '("\\.ino\\'" . cpp-mode))     ; cpp mode for arduino files

;;------------------------------------------------------------------------------
;;; Rainbow Delimeters
;;------------------------------------------------------------------------------

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;------------------------------------------------------------------------------
;;; True/False Faces
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
(add-hook 'python-mode-hook #'my/highlight-false)
(add-hook 'python-mode-hook #'my/highlight-true)


(custom-set-faces
 '(writegood-weasels-face ((t (:underline (:color "#888888" :style wave))))))

;; override some annoying defaults of doom-gruvbox
(custom-theme-set-faces! 'doom-gruvbox
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
