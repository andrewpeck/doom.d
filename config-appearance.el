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
 ((not (display-graphic-p)) (setq doom-theme 'monochrome-solarized))
 ((string= (system-name) "strange") (setq doom-theme 'doom-spacegray))
 ((string= (system-name) "pepper")  (setq doom-theme 'doom-zenburn))
 ((string= (system-name) "larry")   (setq doom-theme 'doom-zenburn))
 (t (setq doom-theme 'doom-one)))

;;------------------------------------------------------------------------------
;;; FONT
;;------------------------------------------------------------------------------


(defun font-exists-p (font)
  "Check if FONT exists"
  (if (functionp 'doom-font-exists-p)
      (doom-font-exists-p font)
      (ignore-errors
        (if (null (x-list-fonts font))
            nil t))))

(setq font-list
      '(("Source Code Pro" . 16)
        ("Hack" . 15)
        ("JetBrains Mono" . 14)
        ("IBM Plex Mono" . 14)
        ("Inconsolata" . 14)
        ("Fira Code" . 14)
        ("Roboto Mono" . 14)))

(cl-dolist (my-font font-list)
  (when (font-exists-p (car my-font))
    (progn
      (setq doom-font (font-spec :family (car my-font) :size (cdr my-font) :weight 'regular)
            doom-big-font (font-spec :family (car my-font) :size (+ 4 (cdr my-font)))
            doom-variable-pitch-font (font-spec :family (car my-font)  :size (cdr my-font))
            doom-serif-font (font-spec :family (car my-font) :weight 'light))
      (cl-return t))))

;;------------------------------------------------------------------------------
;; Syntax Highlighting
;;------------------------------------------------------------------------------

;; enable syntax highlighting for vimrc files
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))   ; vimrc
(add-to-list 'auto-mode-alist '("\\.xdc\\'"          . vivado-mode))  ; tcl mode for xdc files
(add-to-list 'auto-mode-alist '("\\.ltx\\'"          . json-mode))    ; json mode for ltx files
(add-to-list 'auto-mode-alist '("\\.ino\\'"          . cpp-mode))     ; cpp mode for arduino files

;;------------------------------------------------------------------------------
;; Rainbow Delimeters
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
  '(highlight-quoted-symbol :inherit font-lock-variable-name-face :slant italic))
