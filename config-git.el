;; -*- lexical-binding: t; -*-

;; Magit
;;------------------------------------------------------------------------------

(setq forge-owned-accounts
      '(("andrewpeck")
        ("BU-EDF")
        ("ucla-gaps-tof")
        ("emu")
        ("andrewpeck1")
        ("apeck")))

(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-domains
               '("gitlab.cern.ch" . "gitlab")))

(after! magit
  ;;(magit-todos-mode)
  (setq-default magit-diff-refine-hunk 'all)
  ;;(setq magit-repository-directories '(("~/" . 1)))
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
