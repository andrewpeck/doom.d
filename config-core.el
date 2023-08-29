;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;;; Appearance
;;------------------------------------------------------------------------------

;; don't show line numbers by default
(setq display-line-numbers nil)
(remove-hook! 'prog-mode-hook

  #'display-line-numbers-mode)
;; Start emacs in full screen by default
(add-to-list 'default-frame-alist
             '(fullscreen . maximized))

(after! highlight-indent-guides
  ;;(setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-responsive nil
        highlight-indent-guides-method 'bitmap))

;; All the icons
(after! all-the-icons
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (all-the-icons-ibuffer-mode 1))

;; Theme
(add-to-list 'custom-theme-load-path (expand-file-name "~/.doom.d/themes/"))

;;------------------------------------------------------------------------------
;; Automatic theme setting
;;------------------------------------------------------------------------------

;; doom-material, doom-manegarm, doom-one, doom-spacegray, doom-material
;; doom-gruvbox, doom-oceanic-next, doom-tomorrow-night

(setq dark-mode 'dark) ;; 'auto 'dark 'light

(defun ap/get-dark-theme ()
  (if (not (display-graphic-p)) 'doom-gruvbox
    (pcase (downcase (system-name))
      ;; doom-laserwave doom-one doom-gruvbox
      ("pepper"      'doom-laserwave)
      ("larry"       'doom-oceanic-next)
      ("strange"     'doom-spacegray)
      ("apeck-len01" 'doom-spacegray)
      (_             'doom-one))))

(defun ap/get-light-theme ()
  (if (not (display-graphic-p)) 'summerfruit
    (pcase (downcase (system-name))
      ("apeck-len01"  'doom-one-light)
      ("pepper"       'doom-one-light)
      ("larry"        'summerfruit)
      ("strange"      'summerfruit)
      (_              'summerfruit))))

(defun synchronize-theme ()
  (interactive)
  (when (equal dark-mode 'auto)
    (let* ((hour  (string-to-number (format-time-string "%H")))
           (darkp (not  (member hour (number-sequence 7 16)))))
      (if darkp
          (set-dark-mode)
        (set-light-mode)))))

(defun set-dark-mode ()
  "Set the color scheme to be dark."
  (interactive)
  (setq dark-mode 'dark)
  (let ((theme (ap/get-dark-theme)))
    (when (not (equal theme (car custom-enabled-themes)))
      (progn (setq doom-theme theme)
             (load-theme theme)))))

(defun set-light-mode ()
  "Set the color scheme to be light."
  (interactive)
  (setq dark-mode 'light)
  (let ((theme (ap/get-light-theme)))
    (when (not (equal theme (car custom-enabled-themes)))
      (progn (setq doom-theme theme)
             (load-theme theme)))))

(defun set-auto-dark-mode ()
  "Set the color scheme to follow the day cycle (roughly)."
  (interactive)
  (setq dark-mode 'auto)
  (synchronize-theme)
  (setq dark-mode 'auto))

(defun toggle-dark-mode ()
  "Toggle dark mode."
  (interactive)

  (pcase dark-mode

    ;; auto -> dark
    ('auto (set-dark-mode))

    ;; dark -> light
    ('dark (set-light-mode))

    ;; light -> auto
    ('light (set-auto-dark-mode))

    (_ (error "Invalid dark mode!")))

  (message (format "Setting theme mode to %s (%s)"
                   (symbol-name dark-mode)
                   (symbol-name doom-theme))))

(when (or (not (boundp 'theme-timer))
          (not theme-timer))
  ;; (cancel-function-timers #'synchronize-theme)
  (setq theme-timer
        (run-with-timer 0 3600 'synchronize-theme)))

;; https://gml.noaa.gov/grad/solcalc/solareqns.PDF
;; https://en.wikipedia.org/wiki/Sunrise_equation

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

;; M-x describe-font
(setq font-list
      '(("Comic Code"      . 16)
        ("Consolas"        . 20)
        ("Source Code Pro" . 16)
        ("JetBrains Mono"  . 16)
        ("Terminus"        . 16)
        ("Hack"            . 14)
        ("Ubuntu Mono"     . 21)
        ("Roboto Mono"     . 14)
        ("Fira Code"       . 14)
        ("Inconsolata"     . 18)
        ("IBM Plex Mono"   . 16)))

(setq variable-pitch-font-list
      '(("Comic Code" . 16)
        ("Fira Code"  . 17)
        ("Cantarell"  . 18)
        ("Calibri"    . 18)
        ("Arial"      . 17)))

(cl-dolist (my-font font-list)
  (when (font-exists-p (car my-font))
    (progn
      (setq doom-font (font-spec :family (car my-font) :size (cdr my-font) :weight 'regular)
            doom-big-font (font-spec :family (car my-font) :size (+ 4 (cdr my-font)))
            doom-serif-font (font-spec :family (car my-font) :weight 'light))
      (cl-return t))))

(cl-dolist (my-font variable-pitch-font-list)
  (when (font-exists-p (car my-font))
    (progn
      (setq doom-variable-pitch-font
            (font-spec :family (car my-font) :size (cdr my-font)))
      (cl-return t))))

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
