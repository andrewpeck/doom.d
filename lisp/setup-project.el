;;------------------------------------------------------------------------------
;; Projectile
;;------------------------------------------------------------------------------

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

  ;; use project find file instead of projectile;
  ;; caching is more trouble than it is worth
  (map! :leader :desc "Find file in project" "SPC" #'project-find-file)

  ;; doom has project.el calling projectile, just revert to original value
  (setq project-find-functions (list #'project-try-vc))

  (setq project-switch-commands 'project-find-file)

  (map! :leader (:prefix "p" :desc "Open Project" "p" #'project-switch-project))
  (map! :leader :desc "Project Find File" "SPC" #'project-find-file))

(use-package! projectile
  :after project
  :config

  (setq projectile-project-search-path '(("~/work" . 2)))

  ;; re-add projects after clearing
  (advice-add 'projectile-cleanup-known-projects
              :after
              #'projectile-discover-projects-in-search-path)

  (defun project-vc-browse-at-remote (&optional arg)
    "Open in browser the VC repository for the selected project."
    (interactive "P")
    (let ((projects (project-known-project-roots)))
      (if projects
          (projectile-completing-read
           "Open Project Git remote: " projects
           :action (lambda (project)
                     (progn
                       (find-file project)
                       (+vc/browse-at-remote-homepage)
                       (previous-buffer))))
        (user-error "There are no known projects"))))


  (map! :leader
        (:prefix "g" :desc "Browse Projectile Homepage" "oH"
                 'project-vc-browse-at-remote))

  ;; use project.el instead of projectile
  ;; avoid caching pain
  (advice-add 'projectile-find-file
              :override
              (lambda (_) (project-find-file)))

  ;; (advice-add 'projectile-find-file :override
  ;;             (lambda (&optional _)
  ;;               (call-interactively #'project-find-file)))

  ;; (defun projectile-project-root (&optional _)
  ;;   (let ((pc (project-current)))
  ;;     (when pc (project-root pc))))

  ;; (defun projectile-switch-project (&optional _)
  ;;   (interactive)
  ;;   (call-interactively #'project-switch-project))

  ;; Due to a very obnoxious bug it seems that if I am on a host system where
  ;; the fd executable is fdfind but connecting to a remote-system where fd is
  ;; found as fd, emacs will try to execute fdfind on the remote system;
  ;; should report this
  (setq projectile-fd-executable "fdfind")

  (add-hook! 'find-file-hook
    (when (remote-host? default-directory)
      (setq-local projectile-git-use-fd nil)))

  (setq projectile-sort-order 'recently-active))
