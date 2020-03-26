;;; doom-summerfruit-theme.el -- port of tomorrow theme -*- no-byte-compile: t; -*-
;;; Commentary:
;; This file is part of emacs-doom-themes, which provides license
;; information.
;;; Code:

(deftheme doom-summerfruit-theme "The doom-summerfruit color theme")

(require 'doom-themes)

(defgroup doom-summerfruit-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-summerfruit-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-summerfruit-theme
  :type '(choice integer boolean))

;; https://github.com/hlissner/emacs-doom-themes/blob/master/doom-themes-base.el
(def-doom-theme doom-summerfruit
  "A light theme based off of Chris Kempson's Tomorrow Dark."

  ;; name        gui       256       16
  ((bg         '("#ffffff" "white"   "white" ))
   (bg-alt     '("#eaeaea" nil       nil     ))
   (base0      '("#f2f2f2" "white"   "white" ))
   (base1      '("#e4e4e4" "#e4e4e4"         ))
   (base2      '("#dedede" "#cccccc"         ))
   (base3      '("#d6d4d4" "#cccccc" "silver"))
   (base4      '("#C0bfbf" "#c0c0c0" "silver"))
   (base5      '("#a3a1a1" "#adadad" "silver"))
   (base6      '("#8a8787" "#949494" "silver"))
   (base7      '("#696769" "#6b6b6b" "silver"))
   (base8      '("#000000" "#000000" "black" ))
   (fg         '("#4d4d4c" "#3a3a3a" "black"))
   (fg-alt     (doom-darken fg 0.6))

   (black      '("#000000" "#000000" "black"))
   (white      '("#ffffff" "#ffffff" "white"))
   (red        '("#ff0007" "#ff0007" "red"))
   (blue       '("#0086f7" "#0086f7" "brightblue"))
   (orange     '("#fb660a" "#fb660a" "brightred"))
   (pink       '("#ff0086" "#ff0086" "pink"))
   (green      '("#22a21f" "#22a21f" "green"))
   (green2     '("#66cd66" "#66cd66" "green"))

   (magenta    '("#c9b4cf" "#c9b4cf" "magenta"))
   (grey       '("#a5a4a5" "#999999" "silver"))
   (yellow     '("#eab700" "#ffcc00" "yellow"))
   (dark-blue  '("#4271ae" "#336699" "blue"))
   (teal       blue) ; FIXME replace with real teal
   (violet     '("#8959a8" "#996699" "brightmagenta"))
   (cyan       '("#8abeb7" "#8abeb7" "cyan"))
   (dark-cyan  (doom-lighten cyan 0.4))

   ;; face categories
   (comments       green2)

   (highlight      dark-blue)
   (vertical-bar   base0)
   (selection      base3)
   (builtin        blue)
   (doc-comments   (doom-darken grey 0.1))
   (constants      orange)
   (functions      black)
   (keywords       orange)
   (methods        blue)
   (operators      fg)
   (type           grey)
   (strings        green)
   (variables      black)
   (numbers        blue)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    fg-alt)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     `(,(doom-darken (car bg) 0.1) ,@(cdr blue)))
   (modeline-bg-alt `(,(doom-darken (car bg) 0.14) ,@(cdr base1)))
   (modeline-fg     base8)
   (modeline-fg-alt "#555555")
   (-modeline-pad
    (when doom-summerfruit-padded-modeline
      (if (integerp doom-summerfruit-padded-modeline)
          doom-summerfruit-padded-modeline
        4))))

  ;; --- faces ------------------------------
  ((doom-modeline-buffer-path       :foreground violet :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   ((line-number &override) :foreground white :background blue)
   ((line-number-current-line &override) :foreground blue :bold bold)

   (ivy-current-match :background region :distant-foreground grey :weight 'ultra-bold)
   (ivy-minibuffer-match-face-1
    :foreground base5
    :weight 'light)
   (ivy-minibuffer-match-face-2 :inherit 'ivy-minibuffer-match-face-1 :foreground violet :weight 'ultra-bold)
   (ivy-minibuffer-match-face-3 :inherit 'ivy-minibuffer-match-face-2 :foreground blue)
   (ivy-minibuffer-match-face-4 :inherit 'ivy-minibuffer-match-face-2 :foreground red)

   ;;
   ((font-lock-keyword-face &override) :foreground keywords :bold bold)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground teal)

   ;; org-mode
   ((org-block &override) :background "#ffffff")
   ((org-block-begin-line  &override)  :foreground "#555555" :background "#eeeeee" :extend t)

   ;;
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt))))

  ;; --- variables --------------------------
  ;; ()
  )
;;; doom-summerfruit-theme.el ends here


;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'doom-summerfruit-theme)
