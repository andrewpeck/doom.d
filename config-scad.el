;;; ../.dotfiles/doom.d/config-scad.el -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; OpenSCAD Checker
;;------------------------------------------------------------------------------

(flycheck-define-checker openscad
  "Runs openscad"
  :command ("openscad"
            (eval (concat "-o" (flycheck-temp-dir-system) "/tmp.png"))
            source-inplace
            )
  :error-patterns
  ;; different versions of scad produce slightly different error messages... uhg
  ((error line-start "ERROR:" (message) " " (file-name)  ", line " line line-end)
   (error line-start "ERROR:" (message) "\"" (file-name) "\", line " line ": syntax error" line-end))
  :modes (scad-mode))
(add-to-list 'flycheck-checkers 'openscad)

;; Function to open the current file in openscad
(defun open-in-openscad ()
  (interactive)
  (start-process (format "*scad-%s*" (file-name-base))
                 nil "openscad" (buffer-file-name)))
(after! scad-mode
  (define-key scad-mode-map (kbd "C-c C-p")
    'open-in-openscad))
