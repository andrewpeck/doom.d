;; -*- lexical-binding: t; -*-

(use-package! project

  :init

  (map! :leader (:prefix "g" :desc "Browse Projectile Homepage" "oH" #'project-vc-browse-at-remote))
  (map! :leader (:prefix "p" :desc "Open Project" "p" #'project-switch-project))
  (map! :leader :desc "Project Find File" "SPC" #'project-find-file)

  :config

  ;; doom overwrites this to ignore tramp
  ;; restore the original value
  (setopt vc-ignore-dir-regexp locate-dominating-stop-dir-regexp)

  (defun doom/find-file-in-private-config ()
    "Jump to a file in DIR (searched recursively).

If DIR is not a project, it will be indexed (but not cached)."
    (interactive)
    (let* ((pr (project-current t doom-user-dir))
           (root (project-root pr))
           (dirs (list root))
           (project-files-relative-names t))
      (project-find-file-in
       (delq nil (list (and buffer-file-name (project--find-default-from
                                              buffer-file-name pr))
                       (thing-at-point 'filename)))
       dirs pr t)))

  (defun project-remember-projects-under-if-exists (dir)
    (when (file-directory-p dir)

      (project-remember-projects-under dir)))

  (defvar my/project-discover-last-run nil
    "Time `my/project-discover-all' last completed, or nil.")

  (defvar my/project-discover-interval (* 15 60)
    "Minimum seconds between automatic `my/project-discover-all' rescans.")

  (defun my/project-discover-all ()
    "Search the work dir and reregister all directories."
    (interactive)
    (let ((inhibit-message t))
      (project-forget-zombie-projects)
      (project-remember-projects-under-if-exists "~/work")
      (project-remember-projects-under-if-exists "/mnt/NAS/Sync/work")
      (project-remember-projects-under-if-exists "/mnt/NAS/Sync/work/emacs"))
    (setq my/project-discover-last-run (current-time)))

  (defun my/project-discover-maybe ()
    "Rescan for projects unless we already did so recently."
    (when (or (null my/project-discover-last-run)
              (> (float-time (time-since my/project-discover-last-run))
                 my/project-discover-interval))
      (my/project-discover-all)))

  ;; rescan for projects, but only while idle -- the scan stats every candidate
  ;; directory under ~/work and the NAS mounts, so it must never land in the
  ;; middle of a keystroke.
  ;;
  ;; NOTE: a repeating idle timer fires once per *idle period*, not once every
  ;; SECS, so without `my/project-discover-interval' every 60s pause would kick
  ;; off another full scan.  Keep a handle and cancel first -- this `:config'
  ;; block re-runs on every `doom/reload', and a bare `run-with-idle-timer'
  ;; would stack another scanner each time.
  (defvar my/project-discover-timer nil)
  (when (timerp my/project-discover-timer)
    (cancel-timer my/project-discover-timer))
  (setq my/project-discover-timer
        (run-with-idle-timer 60 t #'my/project-discover-maybe))

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
          (when-let*
              ((project (completing-read "Open Project Git remote: " projects)))

            (find-file project)
            (+vc/browse-at-remote-homepage)
            (previous-buffer))
        (user-error "There are no known projects")))))
