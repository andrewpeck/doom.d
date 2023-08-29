;; -*- lexical-binding: t; -*-

(setq +vc-gutter-in-remote-files t)

;; set vc-ignore-dir-regexp to the default emacs value; doom overwrites this to
;; a value that ignores any remote directories, causing git-gutter etc to not
;; work through tramp
(setq vc-ignore-dir-regexp
      "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")

;; Magit
;;------------------------------------------------------------------------------

(after! magit
  ;;(magit-todos-mode)
  (setq-default magit-mode-hook nil)
  (add-hook! 'magit-diff-mode-hook
    (setq-local truncate-lines nil))
  (setq magit-repository-directories '(("~/work" . 1)))
  (setq-default magit-diff-refine-hunk 'all)

  ;; from: https://www.reddit.com/r/emacs/comments/1187mmq/can_magit_update_synce_and_init_all_submodules/
  (unless (condition-case nil
              (transient-get-suffix 'magit-submodule "U")
            (error nil)) ;; catch error if suffix does not exist, and return nil to (unless)
    (transient-define-suffix magit-submodule-update-all (args)
      "Update all submodules"
      :class 'magit--git-submodule-suffix
      :description "Update all modules    git submodule update --init [--recursive]"
      (interactive (list (magit-submodule-arguments "--recursive")))
      (magit-with-toplevel
        (magit-run-git-async "submodule" "update" "--init" args)))
    (transient-append-suffix 'magit-submodule '(2 -1) ;; add as last entry in the 3rd section
      '("U" magit-submodule-update-all))))

(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-regexps
               '("^gitlab\\.cern.ch$" . "gitlab")))

(after! forge

  (add-to-list 'auth-sources "~/.authinfo")
  (setq forge-topic-list-limit '(60 . 0))

  (add-to-list 'forge-alist
               '("gitlab.cern.ch"
                 "gitlab.cern.ch/api/v4"
                 "gitlab.cern.ch"
                 forge-gitlab-repository))

  (setq forge-owned-accounts
        '(("andrewpeck")
          ("BU-EDF")
          ("atlas-tdaq-phase2-l0mdt-electronics")
          ("ucla-gaps-tof")
          ("emu")
          ("andrewpeck1")
          ("apeck"))))
