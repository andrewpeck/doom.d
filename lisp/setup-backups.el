;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Backups
;;------------------------------------------------------------------------------

(defun file-browse-backups ()
  "Browse the directory of backup-each-save files for the currently open buffer."
  (interactive)
  (let* ((b-f-n (buffer-file-name))
         (dir (file-name-directory (concat backup-each-save-mirror-location b-f-n)))
         (pat (concat  (file-name-nondirectory b-f-n) "*")))
    (if (fboundp 'dirvish-fd)
        (dirvish-fd dir pat)
      (find-name-dired dir pat))))

(use-package! backup-each-save
  :init

  (add-hook! 'find-file-hook
    (add-hook 'after-save-hook #'backup-each-save nil t))

  :config

  (require 'backup-each-save)

  (setq backup-each-save-mirror-location
        (expand-file-name "~/emacs-backups"))
  (when (not (file-directory-p backup-each-save-mirror-location))
    (make-directory backup-each-save-mirror-location)))
