;; -*- lexical-binding: t; -*-

(setq +vc-gutter-in-remote-files t)

;; Magit
;;------------------------------------------------------------------------------

(after! magit
  ;;(magit-todos-mode)
  (setq-default magit-mode-hook nil)
  (add-hook 'magit-diff-mode-hook (lambda () (setq truncate-lines nil)))
  (setq magit-repository-directories '(("~/work" . 1)))
  (setq-default magit-diff-refine-hunk 'all))

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
