;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;;; Appearance
;;------------------------------------------------------------------------------

;; don't show line numbers by default
(setq display-line-numbers t)

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
      ("apeck-len01"  'modus-operandi)
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
  (setq dark-mode 'dark)) ;; 'auto 'dark 'light

(synchronize-theme)

;; https://gml.noaa.gov/grad/solcalc/solareqns.PDF
;; https://en.wikipedia.org/wiki/Sunrise_equation
