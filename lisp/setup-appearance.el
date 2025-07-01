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

;;------------------------------------------------------------------------------
;; PDF View Mode
;;------------------------------------------------------------------------------

(after! pdf-view
  (defun pdf-view-midnight-update-colors ()
    "Sync pdf view midnight colors to currently selected theme."
    (interactive)
    (setq pdf-view-midnight-colors
          `(,(face-attribute 'default :foreground) .
            ,(face-attribute 'default :background)))
    (dolist (buf (buffer-list (current-buffer)))
      (with-current-buffer buf
        (when pdf-view-midnight-minor-mode
          (pdf-view-midnight-minor-mode 1)))))

  (add-hook 'doom-load-theme-hook #'pdf-view-midnight-update-colors))

;;------------------------------------------------------------------------------
;;; Appearance
;;------------------------------------------------------------------------------

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

(defun my/disable-all-active-themes ()
  "Disable all currently active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(advice-add 'load-theme :before (lambda (&rest _) (my/disable-all-active-themes)))

(defun ap/get-dark-theme ()
  (if (not (or (daemonp)
               (display-graphic-p)))
      'modus-vivendi-tinted
    (pcase (downcase (system-name))
      ;; doom-laserwave doom-one doom-gruvbox
      ("pepper"      'doom-laserwave)
      ("larry"       'doom-spacegrey)
      ("strange"     'doom-spacegrey)
      ("apeck-len01" 'doom-gruvbox)
      (_             'doom-spacegrey))))

(defun ap/get-light-theme ()
  (if (not (display-graphic-p)) 'summerfruit
    (pcase (downcase (system-name))
      ("apeck-len01"  'standard-light)
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

    ;; ;; auto -> dark
    ;; ('auto (set-dark-mode))

    ;; dark -> light
    ('dark (set-light-mode))

    ;; light -> auto
    ('light (set-dark-mode))

    (_ (error "Invalid dark mode!")))

  (message (format "Setting theme mode to %s (%s)"
                   (symbol-name dark-mode)
                   (symbol-name doom-theme))))

;; (when (or (not (boundp 'theme-timer))
;;           (not theme-timer))
;;   ;; (cancel-function-timers #'synchronize-theme)
;;   (setq theme-timer
;;         (run-with-timer 0 3600 'synchronize-theme)))

(when (not (boundp 'dark-mode))
  (setq dark-mode 'dark)) ;; 'auto 'dark 'light

(synchronize-theme)

;; https://gml.noaa.gov/grad/solcalc/solareqns.PDF
;; https://en.wikipedia.org/wiki/Sunrise_equation

;;------------------------------------------------------------------------------
;; Theme Customization
;;------------------------------------------------------------------------------

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

(custom-theme-set-faces! 'standard-light
  '(font-latex-sectioning-0-face :height 2.0 :weight bold)
  '(font-latex-sectioning-1-face :height 1.8 :weight bold)
  '(font-latex-sectioning-2-face :height 1.3 :weight bold)
  '(font-latex-sectioning-3-face :height 1.1 :weight bold)
  '(font-latex-sectioning-4-face :height 1.0 :weight bold))

(custom-theme-set-faces! 'modus-operandi
  '(font-latex-sectioning-0-face :height 2.0 :weight bold)
  '(font-latex-sectioning-1-face :height 1.8 :weight bold)
  '(font-latex-sectioning-2-face :height 1.4 :weight bold)
  '(font-latex-sectioning-3-face :height 1.2 :weight bold)
  '(font-latex-sectioning-4-face :height 1.0 :weight bold))

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
  '(font-latex-bold-face :inherit default :weight bold))

;; HACK for using nobreak-space in modeline
(custom-theme-set-faces! 'doom-spacegrey
  '(nobreak-space :inherit modeline))

(custom-theme-set-faces! 'hima
  ;; '(font-lock-type-face :inherit default :family "Courier New" )
  '(font-latex-italic-face :inherit default :slant italic)
  '(font-latex-bold-face :inherit default :weight bold))

(custom-theme-set-faces! 'modus-operandi
  '(nobreak-space :inherit modeline))

(custom-theme-set-faces! 'modus-operandi
  '(hl-todo :inherit success))

(custom-theme-set-faces! 'standard-light
  '(fringe :inherit background)
  '(mode-line :background "#ddd"))
