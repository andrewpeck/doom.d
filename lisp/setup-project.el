;; -*- lexical-binding: t; -*-

(use-package! project

  :config

  ;; doom overwrites this to ignore tramp uhg damnit
  ;; restore the original value
  ;;
  ;; but when it is restored the bookmarks don't work because the bookmark tool
  ;; apparently tries to create tramp connections? UHG
  (setq vc-ignore-dir-regexp "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")

  ;; doom has project.el calling projectile, just revert to original value
  (setq project-find-functions (list #'project-try-vc))

  (defun my/project-discover-all ()
    "Search the work dir and reregister all directories."
    (interactive)
    (project-forget-zombie-projects)
    (project-remember-projects-under "~/work"))

  (defun projectile-locate-dominating-file (&rest _)
    (locate-dominating-file "." ".git"))

  ;; doom has project.el calling projectile, just revert to original value
  (setq project-find-functions (list #'project-try-vc))

  (setq project-switch-commands 'project-find-file)

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
        (user-error "There are no known projects"))))

  (map! :leader (:prefix "g" :desc "Browse Projectile Homepage" "oH" 'project-vc-browse-at-remote))
  (map! :leader (:prefix "p" :desc "Open Project" "p" #'project-switch-project))
  (map! :leader :desc "Project Find File" "SPC" #'project-find-file))
