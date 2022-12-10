;;; -*- lexical-binding: t; -*-

;; TODO: should copy the buffer to a temp file

;;  convert gerber to svg
(defun gerber->svg--file (filename)
  (let ((tmp-name (concat (make-temp-file (concat filename "-")) ".svg")))
    (shell-command  (concat  "gerbv --dpi=600 --border=0 --export=svg "
                             "--output=" tmp-name " " filename))
    tmp-name))

(defun gerber->svg ()
  (interactive)
  (progn
    (find-file
     (gerber->svg--file (buffer-file-name)))
    (revert-buffer)
    (image-mode)))
