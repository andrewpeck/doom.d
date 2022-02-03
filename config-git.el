;; -*- lexical-binding: t; -*-

;; Magit
;;------------------------------------------------------------------------------

(defun my-wrap-lines ()
  "Disable `truncate-lines' in the current buffer."
  (setq truncate-lines nil))

(after! magit
  (add-hook 'magit-diff-mode-hook #'my-wrap-lines)
  ;;(magit-todos-mode)
  (setq-default magit-diff-refine-hunk 'all)
  ;;(setq magit-repository-directories '(("~/" . 1)))
  )

(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-regexps
               '("^gitlab\\.cern.ch$" . "gitlab")))

(after! forge

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
          ("apeck")))


  )

;; Git Gutter
;;------------------------------------------------------------------------------

;; for some reason this is slow to load :(
;;
;;(use-package! git-gutter
;;  :defer t
;;  :init
;;  (use-package! git-gutter-fringe
;;    :defer t
;;    :config
;;    (fringe-mode 9)
;;    (fringe-helper-define 'git-gutter-fr:added nil
;;      "....X...."
;;      "....X...."
;;      "....X...."
;;      "....X...."
;;      "XXXXXXXXX"
;;      "....X...."
;;      "....X...."
;;      "....X....")
;;
;;    (fringe-helper-define 'git-gutter-fr:deleted nil
;;      "........."
;;      "........."
;;      "........."
;;      "..XXXXXX."
;;      "........."
;;      "........."
;;      "........."
;;      ".........")
;;
;;    (fringe-helper-define 'git-gutter-fr:modified nil
;;      "........."
;;      "...X....."
;;      "...XXX..."
;;      "...XXXX.."
;;      "...XXXXX."
;;      "...XXXX.."
;;      "...XXX..."
;;      "...X.....")
;;    )
;;)
