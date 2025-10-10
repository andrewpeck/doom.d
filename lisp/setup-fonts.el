;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; * FONT
;;------------------------------------------------------------------------------

(defun set-font-interactive ()
  "Interactively choose and set a font."
  (interactive)
  (let ((font (completing-read "Font: " (font-family-list))))
    (when font
      (let ((size (string-to-number (read-string "Size: " (if (hd?) "22" "16")))))
        (when (numberp size)
          (setq doom-font (font-spec :name font :size size :weight 'regular))
          (doom/reload-font))))))

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
  `(("Adwaita Mono"             . ,(if (hd?) 24 16))
    ("Hack Nerd Font"           . ,(if (hd?) 21 16))
    ("Berkeley Mono"            . ,(if (hd?) 24 20))
    ("InconsolataGo Nerd Font"  . ,(if (hd?) 26 19))
    ("Julia Mono"               . ,(if (hd?) 21 16))
    ("JetBrains Mono"           . ,(if (hd?) 23 20))
    ("Comic Code"               . ,(if (hd?) 22 14))
    ("Inconsolata"              . ,(if (hd?) 19 18))
    ("Roboto Mono"              . ,(if (hd?) 22 19))
    ("Noto Mono"                . ,(if (hd?) 19 15))
    ("DejaVu Sans Mono"         . ,(if (hd?) 19 17))
    ("Consolas"                 . ,(if (hd?) 20 19))
    ("Source Code Pro"          . ,(if (hd?) 16 19))
    ("Terminus"                 . ,(if (hd?) 16 19))
    ("Fira Code"                . ,(if (hd?) 14 19))
    ("IBM Plex Mono"            . ,(if (hd?) 16 19))
    ("Ubuntu Mono"              . ,(if (hd?) 21 19))))

(defun variable-pitch-font-list ()
  ""
  `(("Adwaita Sans" . 18)
    ("Comic Code" . ,(if (hd?) 24 16))
    ("Cantarell"  . ,(if (hd?) 24 16))
    ("Fira Code"  . 17)
    ("Calibri"    . 18)))

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
        (setq doom-variable-pitch-font (font-spec :family (car my-font) :size (cdr my-font)))
        (cl-return t)))))

(ap/update-font-list)

(advice-add #'doom/reload-font :before #'ap/update-font-list)
