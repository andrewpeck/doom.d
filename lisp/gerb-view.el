;;; -*- lexical-binding: t; -*-

;; TODO: should copy the buffer to a temp file.. sometimes e.g. if trying to open a zipped file then
;; trying to exec gerbv fails

;;  convert gerber to svg
(defun gerber->svg--file (filename)
  (let* ((tmp-name
          (format  "%s.svg" (make-temp-file (concat (file-name-base filename) "-"))))
         (command
          (format  "gerbv --dpi=600 --border=0 --export=svg --output=%s %s" tmp-name filename)))
    (message tmp-name)
    (shell-command  command) tmp-name))

(defun gerber->svg ()
  (interactive)
  (progn
    (find-file
     (gerber->svg--file (buffer-file-name)))
    (revert-buffer)
    (image-mode)))
