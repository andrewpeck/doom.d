;;; ../.dotfiles/doom.d/config-appearance.el -*- lexical-binding: t; -*-

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
(setq my-font "JetBrains Mono")
;; (setq my-font "IBM Plex Mono")

(setq doom-font (font-spec :family my-font :size 14)
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
