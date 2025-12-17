;; config-packages.el -*- lexical-binding: t; -*-

(after! recentf
  (defun recentf-keep-default-predicate (file)
    "Return non-nil if FILE should be kept in the recent list.
It handles the case of remote files as well.

This version is modified from the original source to not record directories."
    (cond
     ((file-directory-p file) nil)
     ((file-remote-p file nil t) (recentf-access-file file))
     ((file-remote-p file))
     ((file-readable-p file)))))

(use-package consult

  :init

  (map! :leader :desc "Find file in project" "SPC" #'consult-project-find-file)

  :config
  (setq consult-buffer-sources
        '(consult--source-modified-buffer
          consult--source-recent-file
          consult--source-buffer-register
          consult--source-file-register
          consult--source-project-buffer-hidden
          consult--source-project-recent-file-hidden
          consult--source-project-root-hidden
          +vertico--consult-org-source)))
