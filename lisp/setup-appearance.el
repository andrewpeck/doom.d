;; -*- lexical-binding: t; -*-

;; Theme
(add-to-list 'custom-theme-load-path (expand-file-name "~/.doom.d/themes/"))

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

(defvar ap/dark-theme "Preferred dark theme.")
(defvar ap/light-theme "Preferred light theme.")

(setq ap/dark-theme
      (if (and (not (daemonp))
               (not (display-graphic-p)))
          'modus-vivendi-tinted
        (pcase (downcase (system-name))
          ;; doom-laserwave doom-one doom-gruvbox
          ("pepper"      'doom-laserwave)
          ("strange"     'doom-spacegrey)
          (_             'doom-gruvbox))))

(setq ap/light-theme
      (if (and (not (daemonp))
               (not (display-graphic-p))) 'summerfruit
    (pcase (downcase (system-name))
      ("pepper"       'doom-one-light)
      ("strange"      'summerfruit)
      (_              'ef-cyprus))))

;;------------------------------------------------------------------------------
;; Follow the session color scheme
;;------------------------------------------------------------------------------

;; ~/dotfiles/scripts/color-scheme owns the light/dark switch for the whole
;; session -- the i3 bar button, the GTK theme, the XDG portal preference that
;; firefox and the electron apps read -- and mirrors whichever one it settled
;; on into the file below.  Watching that file rather than the portal itself is
;; what makes this work in a terminal emacs and in the daemon, neither of which
;; is a D-Bus client; the cost is one inotify watch.

(defvar ap/color-scheme-file (expand-file-name "~/.local/state/color-scheme")
  "File in which the session publishes its current color scheme.")

(defvar ap/color-scheme-watch nil
  "The `file-notify' descriptor watching `ap/color-scheme-file'.")

(defun ap/color-scheme ()
  "Return the session color scheme as `dark' or `light', or nil if unknown."
  (when (file-readable-p ap/color-scheme-file)
    (with-temp-buffer
      (insert-file-contents ap/color-scheme-file)
      (pcase (string-trim (buffer-string))
        ("dark" 'dark)
        ("light" 'light)))))

(defun ap/sync-color-scheme ()
  "Match the theme to the session color scheme.
Does nothing when the scheme cannot be read, so an emacs on a machine
without the script keeps the theme it started with."
  (interactive)
  (pcase (ap/color-scheme)
    ('dark (set-dark-mode))
    ('light (set-light-mode))))

(defun ap/watch-color-scheme ()
  "Reload the theme whenever the session color scheme changes."
  (interactive)
  (require 'filenotify)
  (when (file-notify-valid-p ap/color-scheme-watch)
    (file-notify-rm-watch ap/color-scheme-watch))
  (setq ap/color-scheme-watch
        (when (file-exists-p ap/color-scheme-file)
          (file-notify-add-watch
           ap/color-scheme-file '(change)
           (lambda (event)
             ;; the script rewrites the file in place rather than renaming over
             ;; it -- precisely so this watch survives the switch -- so
             ;; `changed' is the only action that ever arrives here
             (when (eq (nth 1 event) 'changed)
               (ap/sync-color-scheme)))))))

(defvar dark-mode (or (ap/color-scheme) 'dark))

(setq doom-theme (if (eq dark-mode 'light) ap/light-theme ap/dark-theme))

;;------------------------------------------------------------------------------
;; Theme Customization
;;------------------------------------------------------------------------------

(use-package doom-gruvbox-theme
  :config
  ;; override some annoying defaults of doom-gruvbox
  (custom-theme-set-faces! 'doom-gruvbox
    ;; the default function name face is the same as the string face and looks awful for lisp
    '(font-lock-function-name-face :foreground "#d3869b")
    '(mu4e-unread-face :foreground "#cccccc" :weight bold)
    '(tab-bar-tab-inactive :foreground "#646464" :background "#151515") ; more contrast in inactive tabs
    '(whitespace-tab :background "#282828")
    '(org-todo :weight bold :foreground "orange red")
    '(org-table :weight normal :foreground "lightblue")
    '(org-link :weight normal :underline "lightpink" :foreground "light pink")
    '(org-level-1 :height 1.5 :weight bold   :foreground "#b8bb26")
    '(org-level-2 :height 1.2 :weight bold   :foreground "lightblue")
    '(org-level-3 :height 1.1 :weight normal :foreground "#d3869b")
    '(org-level-4 :height 1.1 :weight normal :foreground "#83a598")))

(use-package doom-spacegray-theme
  :config
  (custom-theme-set-faces! 'doom-spacegrey
    '(org-todo :weight bold :foreground "orange red")
    '(org-table :weight normal :foreground "lightblue")
    '(org-link :weight normal :underline "lightpink" :foreground "light pink")
    '(org-level-1 :height 1.5 :weight bold   :foreground "#c0c5ce")
    '(org-level-2 :height 1.2 :weight bold   :foreground "lightblue")
    '(org-level-3 :height 1.1 :weight normal :foreground "#d3869b")
    '(org-level-4 :height 1.1 :weight normal :foreground "#83a598")))


(use-package hima-theme
  :config
  (custom-theme-set-faces! 'hima
    '(org-level-1 :height 1.0 :weight bold)
    '(org-level-2 :height 1.0 :weight bold)
    '(org-level-3 :height 1.0 :weight bold)
    '(org-level-4 :height 1.0 :weight bold)
    '(font-latex-italic-face :inherit default :slant italic)
    '(font-latex-bold-face   :inherit default :weight bold)))

(use-package standard-light-theme
  :config
  (custom-theme-set-faces! 'standard-light
    '(fringe :inherit background)
    '(mode-line :background "#ddd")
    '(mu4e-unread-face :foreground "black" :weight bold)
    '(font-latex-sectioning-0-face :height 2.0 :weight bold)
    '(font-latex-sectioning-1-face :height 1.8 :weight bold)
    '(font-latex-sectioning-2-face :height 1.3 :weight bold)
    '(font-latex-sectioning-3-face :height 1.1 :weight bold)
    '(font-latex-sectioning-4-face :height 1.0 :weight bold)))

(use-package modus-operandi-theme
  :config
  (custom-theme-set-faces! 'modus-operandi
    '(font-latex-sectioning-0-face :height 2.0 :weight bold)
    '(font-latex-sectioning-1-face :height 1.8 :weight bold)
    '(font-latex-sectioning-2-face :height 1.4 :weight bold)
    '(font-latex-sectioning-3-face :height 1.2 :weight bold)
    '(font-latex-sectioning-4-face :height 1.0 :weight bold)
    '(nobreak-space :inherit modeline)
    '(hl-todo :inherit success)))

(use-package solarized-zenburn-theme
  :config
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
    '(font-latex-bold-face :inherit default :weight bold)))

(use-package almost-mono-black-theme
  :config
  (custom-theme-set-faces! 'almost-mono-black
    `(font-lock-comment-face :foreground "#666")
    `(font-lock-keyword-face :foreground "#ccc")
    `(font-lock-builtin-face :foreground "#ccc")
    `(font-lock-variable-name-face :foreground "#cecece")
    `(highlight :foreground "#ddd" :background "#3c5e2b")
    `(region    :inherit 'highlight)
    `(default :foreground "#ccc" :background "#080808")))

(use-package doom-zenburn-theme
  :config
  (custom-theme-set-faces! 'doom-zenburn
    '(font-latex-italic-face :inherit default :slant italic)
    '(font-latex-bold-face :inherit default :weight bold)))

(use-package doom-spacegray-theme
  :config
  ;; HACK for using nobreak-space in modeline
  (custom-theme-set-faces! 'doom-spacegrey
    '(nobreak-space :inherit modeline)))

(defun reload-frame-theme (frame)
  (with-selected-frame frame
    (load-theme doom-theme)))

(add-hook 'after-make-frame-functions
          #'reload-frame-theme)

(ap/watch-color-scheme)

;; A daemon that came up before the state file existed has no watch at all.  A
;; new frame is the moment that matters and a good one to notice, so re-arm
;; there and take the scheme that was missed.
(add-hook 'after-make-frame-functions
          (lambda (_frame)
            (unless (file-notify-valid-p ap/color-scheme-watch)
              (ap/watch-color-scheme)
              (ap/sync-color-scheme))))
