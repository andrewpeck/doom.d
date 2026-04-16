;; -*- lexical-binding: t; -*-

(use-package! project

  :init

  (map! :leader (:prefix "g" :desc "Browse Projectile Homepage" "oH" 'project-vc-browse-at-remote))
  (map! :leader (:prefix "p" :desc "Open Project" "p" #'project-switch-project))
  (map! :leader :desc "Project Find File" "SPC" #'project-find-file)

  :config

  ;; doom overwrites this to ignore tramp uhg damnit
  ;; restore the original value
  ;;
  ;; but when it is restored the bookmarks don't work because the bookmark tool
  ;; apparently tries to create tramp connections? UHG
  (setopt vc-ignore-dir-regexp "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")

  (defun doom/find-file-in-private-config ()
    "Jump to a file in DIR (searched recursively).

If DIR is not a project, it will be indexed (but not cached)."
    (interactive)
    (let* ((pr (project-current t doom-private-dir))
           (root (project-root pr))
           (dirs (list root))
           (project-files-relative-names t))
      (project-find-file-in
       (delq nil (list (and buffer-file-name (project--find-default-from
                                              buffer-file-name pr))
                       (thing-at-point 'filename)))
       dirs pr t)))

  (defun project-remember-project-under-if-exists (dir)
    (when (file-directory-p dir)
      (project-remember-projects-under dir)))

  (defun my/project-discover-all ()
    "Search the work dir and reregister all directories."
    (interactive)
    (project-forget-zombie-projects)
    (project-remember-projects-under-if-exists "~/work")

  ;; periodically rescan for projects
  (run-with-timer 10 3600 'my/project-discover-all)

  (defun projectile-locate-dominating-file (&rest _)
    (locate-dominating-file "." ".git"))

  ;; doom has project.el calling projectile, just revert to original value
  (setopt project-find-functions (list #'project-try-vc))

  (setopt project-switch-commands 'project-find-file)

  (defun project-vc-browse-at-remote (&optional _)
    "Open in browser the VC repository for the selected project."
    (interactive "P")
    (let ((projects (project-known-project-roots)))
      (if projects
          (when-let
              ((project (completing-read "Open Project Git remote: " projects)))

            (find-file project)
            (+vc/browse-at-remote-homepage)
            (previous-buffer))
        (user-error "There are no known projects")))))
