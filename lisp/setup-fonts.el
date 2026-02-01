;; -*- lexical-binding: t; -*-

(defun hd? ()
  "Check if the display is larger than 1920 pixels."
  (> (display-pixel-width) 1920))

(defun font-exists? (font)
  "Check if FONT exists."
  (if (functionp 'doom-font-exists-p)
      (doom-font-exists-p font)
    (ignore-errors
      (if (null (x-list-fonts font))
          nil t))))

(defvar my/preferred-fonts nil "List of preferred fonts. (name hd-size size)")

 ;; M-x describe-font
 (setq my/preferred-fonts
   `(("Hack Nerd Font"           23 18)
     ("AdwaitaMono Nerd Font"    21 16)
     ("Berkeley Mono"            24 20)
     ("InconsolataGo Nerd Font"  26 19)
     ("Julia Mono"               21 16)
     ("JetBrains Mono"           23 20)
     ("Comic Code"               22 14)
     ("Inconsolata"              19 18)
     ("Roboto Mono"              22 19)
     ("Noto Mono"                19 15)
     ("DejaVu Sans Mono"         19 17)
     ("Consolas"                 20 19)
     ("Source Code Pro"          16 19)
     ("Terminus"                 16 19)
     ("Fira Code"                14 19)
     ("IBM Plex Mono"            16 19)
     ("Ubuntu Mono"              21 19)))

(defvar my/preferred-variable-pitch-font
  `(("Adwaita Sans" 18 18)
    ("Comic Code"   24 16)
    ("Cantarell"    24 16)
    ("Fira Code"    24 17)
    ("Calibri"      24 18))

  "List of preferred variable pitch fonts.")

(defun ap/update-font-list ()

  (let* ((font (cl-find-if (lambda (x) (font-exists? (car x))) my/preferred-fonts))
         (name (car font))
         (size (if (hd?) (nth 1 font) (nth 2 font))))

    (when font
      (setq doom-font (font-spec :family name :size size :weight 'regular)
            doom-big-font (font-spec :family name :size (+ 4 size))
            doom-serif-font (font-spec :family name :weight 'light))))

  (let* ((font (cl-find-if (lambda (x) (font-exists? (car x))) my/preferred-variable-pitch-font))
         (name (car font))
         (size (if (hd?) (nth 1 font) (nth 2 font))))
    (when font
      (setq doom-variable-pitch-font (font-spec :family name :size size)))))

(advice-add #'doom/reload-font :before #'ap/update-font-list)
(ap/update-font-list)
