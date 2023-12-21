;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;;; Appearance
;;------------------------------------------------------------------------------

;; don't show line numbers by default
(setq display-line-numbers t)

(add-hook! 'org-mode-hook
           #'display-line-numbers-mode)

;; Start emacs in full screen by default
(add-to-list 'default-frame-alist
             '(fullscreen . maximized))

(after! highlight-indent-guides
  ;;(setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-responsive nil
        highlight-indent-guides-method 'bitmap))

;; All the icons
;; (after! all-the-icons
;;   (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;;   (all-the-icons-ibuffer-mode 1))

;; Theme
(add-to-list 'custom-theme-load-path
             (expand-file-name "~/.doom.d/themes/"))

;;------------------------------------------------------------------------------
;; Automatic theme setting
;;------------------------------------------------------------------------------

;; doom-material, doom-manegarm, doom-one, doom-spacegray, doom-material
;; doom-gruvbox, doom-oceanic-next, doom-tomorrow-night

(defun ap/get-dark-theme ()
  (if (not (or (daemonp)
               (display-graphic-p)))
      'doom-gruvbox
    (pcase (downcase (system-name))
      ;; doom-laserwave doom-one doom-gruvbox
      ("pepper"      'doom-laserwave)
      ("larry"       'doom-spacegrey)
      ("strange"     'doom-spacegrey)
      ("apeck-len01" 'doom-spacegrey)
      (_             'doom-spacegrey))))

(defun ap/get-light-theme ()
  (if (not (display-graphic-p)) 'summerfruit
    (pcase (downcase (system-name))
      ("apeck-len01"  'hima)
      ("pepper"       'doom-one-light)
      ("larry"        'summerfruit)
      ("strange"      'summerfruit)
      (_              'summerfruit))))

(defun synchronize-theme ()
  (interactive)
  (pcase dark-mode
    ('auto (let* ((hour  (string-to-number (format-time-string "%H")))
                  (darkp (not  (member hour (number-sequence 7 15)))))
             (if darkp
                 (set-dark-mode)
               (set-light-mode))))
    ('dark (set-dark-mode))
    ('light (set-light-mode))))

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

(when (not (boundp 'dark-mode))
  (setq dark-mode 'auto)) ;; 'auto 'dark 'light

(synchronize-theme)

;; https://gml.noaa.gov/grad/solcalc/solareqns.PDF
;; https://en.wikipedia.org/wiki/Sunrise_equation

;;------------------------------------------------------------------------------
;;; FONT
;;------------------------------------------------------------------------------

(defun font-exists? (font)
  "Check if FONT exists"
  (if (functionp 'doom-font-exists-p)
      (doom-font-exists-p font)
    (ignore-errors
      (if (null (x-list-fonts font))
          nil t))))

(defun hd? ()
  (> (display-pixel-width) 1920))

;; M-x describe-font
(defun font-list ()
  ""
  `(("Julia Mono"       . ,(if (hd?) 21 16))
    ("Inconsolata"      . ,(if (hd?) 19 18))
    ("Comic Code"       . ,(if (hd?) 22 19))
    ("Roboto Mono"      . ,(if (hd?) 22 19))
    ("Hack"             . ,(if (hd?) 19 15))
    ("Noto Mono"        . ,(if (hd?) 19 15))
    ("DejaVu Sans Mono" . ,(if (hd?) 19 17))
    ("Consolas"         . ,(if (hd?) 20 19))
    ("Source Code Pro"  . ,(if (hd?) 16 19))
    ("JetBrains Mono"   . ,(if (hd?) 16 19))
    ("Terminus"         . ,(if (hd?) 16 19))
    ("Fira Code"        . ,(if (hd?) 14 19))
    ("IBM Plex Mono"    . ,(if (hd?) 16 19))
    ("Ubuntu Mono"      . ,(if (hd?) 21 19))))

(defun variable-pitch-font-list ()
  ""
  '(("Comic Code" . 16)
    ("Fira Code"  . 17)
    ("Cantarell"  . 18)
    ("Calibri"    . 18)
    ("Arial"      . 17)))

(defun ap/update-font-list ()

  (cl-dolist (my-font (font-list))
    (when (font-exists? (car my-font))
      (progn
        (setq doom-font (font-spec :family (car my-font) :size (cdr my-font) :weight 'regular)
              doom-big-font (font-spec :family (car my-font) :size (+ 4 (cdr my-font)))
              doom-serif-font (font-spec :family (car my-font) :weight 'light))
        (cl-return t))))

  (cl-dolist (my-font (variable-pitch-font-list))
    (when (font-exists? (car my-font))
      (progn
        (setq doom-variable-pitch-font
              (font-spec :family (car my-font) :size (cdr my-font)))
        (cl-return t)))))

(ap/update-font-list)

;; synchronize font periodically since the damn monitor changes so much
(when (or (not (boundp 'font-timer))
          (not font-timer))
  (setq font-timer
        (run-with-timer 0 60
                        (lambda () (progn (ap/update-font-list) (doom/reload-font))))))

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
