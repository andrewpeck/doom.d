;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;;; Appearance
;;------------------------------------------------------------------------------

(setq display-line-numbers nil)

;; Start emacs in full screen by default
(add-to-list 'default-frame-alist
             '(fullscreen . maximized))

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

;; Theme
(add-to-list 'load-path "~/.doom.d/themes/")
(add-to-list 'custom-theme-load-path "~/.doom.d/themes/")

(setq doom-theme
      (if (not (display-graphic-p))
          'monochrome-solarized
        (pcase (system-name)
          ("pepper"  'doom-zenburn)
          ("larry"   'doom-zenburn)
          ("strange" 'doom-spacegray)
          (_         'doom-one))))

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
      '(("Roboto Mono" . 16)
        ("Consolas" . 17)
        ("Source Code Pro" . 16)
        ("Hack" . 15)
        ("IBM Plex Mono" . 16)
        ("JetBrains Mono" . 16)
        ("Inconsolata" . 14)
        ("Fira Code" . 14)))

(cl-dolist (my-font font-list)
  (when (font-exists-p (car my-font))
    (progn
      (setq doom-font (font-spec :family (car my-font) :size (cdr my-font) :weight 'regular)
            doom-big-font (font-spec :family (car my-font) :size (+ 4 (cdr my-font)))
                                        ;doom-variable-pitch-font (font-spec :family "Comic Sans" :size 16)
            doom-serif-font (font-spec :family (car my-font) :weight 'light))
      (cl-return t))))

;;
(use-package! delight
  :defer-incrementally t
  :config
  (delight '+org-pretty-mode         " 🌻"          "org")
  (delight 'better-jumper-local-mode ""             "better-jumper")
  (delight 'company-mode             ""             "company")
  (delight 'dtrt-indent-mode         ""             "dtrt-indent")
  (delight 'eldoc-mode               ""             "eldoc")
  (delight 'evil-escape-mode         ""             "evil-escape")
  (delight 'evil-goggles-mode        ""             "evil-goggles")
  (delight 'evil-org-mode            ""             "evil-org")
  (delight 'evil-snipe-local-mode    ""             "evil-snipe")
  (delight 'evil-traces-mode         ""             "evil-traces")
  (delight 'evil-traces-mode         ""             "evil-traces")
  (delight 'gcmh-mode                ""             "gcmh")
  (delight 'hi-lock-mode             ""             "hi-lock")
  (delight 'lispy-mode               ""             "lispy")
  (delight 'lispyville-mode          ""             "lispyville")
  (delight 'org-indent-mode          ""             "org-indent")
  (delight 'outline-minor-mode       ""             "outline")
  (delight 'persp-mode               "view"         "perspective-mode")
  (delight 'projectile-mode          ""             "projectile")
  (delight 'subword-mode             ""             "subword")
  (delight 'undo-tree-mode           ""             "undo-tree")
  (delight 'which-key-mode           ""             "which-key")
  (delight 'whitespace-mode          ""             "whitespace")
  (delight 'ws-butler-mode           ""             "ws-butler")
  (delight 'yas-minor-mode           ""             "yasnippet")
  (delight 'evil-markdown-mode       ""             "evil-markdown")
  (delight 'format-all-mode          ""             "format-all")
  (delight 'markdown-mode            "Md"           "markdown")
  (delight 'poly-markdown-mode       " Pm"          "poly-markdown")
  (delight 'magit-gitflow-mode       ""             "magit-gitflow")
  (delight 'git-gutter-mode          ""             "git-gutter")
  (delight 'rainbow-mode             " 🌈"          "rainbow-mode")
  ;; (delight 'emacs-lisp-mode          " λ"           "emacs-lisp-mode")
  ;; (delight 'a-mode                   ""             "a")
  ;; (delight 'a-mode                   ""             "a")
  ;; (delight 'a-mode                   ""             "a")
  ;; (delight 'a-mode                   ""             "a")
  ;; (delight 'a-mode                   ""             "a")
  ;; (delight 'a-mode                   ""             "a")
  )
