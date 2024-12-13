;;------------------------------------------------------------------------------
;; Magit
;;------------------------------------------------------------------------------

(use-package! magit

  :config

  ;; (add-to-list 'magit-status-sections-hook
  ;;              #'magit-insert-local-branches)
  (setq magit-status-sections-hook
        (list
         #'magit-insert-status-headers
         #'magit-insert-merge-log
         #'magit-insert-rebase-sequence
         #'magit-insert-am-sequence
         #'magit-insert-sequencer-sequence
         #'magit-insert-bisect-output
         #'magit-insert-bisect-rest
         #'magit-insert-bisect-log
         #'magit-insert-untracked-files
         #'magit-insert-unstaged-changes
         #'magit-insert-staged-changes
         #'magit-insert-unpushed-to-pushremote
         #'magit-insert-unpushed-to-upstream-or-recent
         #'magit-insert-unpulled-from-pushremote
         #'magit-insert-unpulled-from-upstream
         #'magit-insert-stashes
         #'magit-insert-local-branches
         ))

  ;; magit-describe-section-briefly
  (setq magit-section-initial-visibility-alist '((stashes . hide)
                                                 (unpushed . hide)
                                                 (local . hide)))

  ;; When you initiate a commit, then Magit by default automatically shows a diff
  ;; of the changes you are about to commit. For large commits this can take a
  ;; long time, which is especially distracting when you are committing large
  ;; amounts of generated data which you don’t actually intend to inspect before
  ;; committing. This behavior can be turned off using:

  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)

  (define-key transient-map (kbd "C-c C-c") #'transient-save)

  (setq magit-log-margin '(t "%Y/%m/%d" magit-log-margin-width t 18)

        ;; List of directories that are or contain Git repositories. ;
        magit-repository-directories '(("~/work" . 1))

        ;; we've seen the warnings about long lines,
        ;; don't need to keep showing them
        magit-show-long-lines-warning nil

        ;; Whether to show word-granularity differences within diff hunks.
        magit-diff-refine-hunk 'all)

  (setq-default magit-mode-hook nil)
  (add-hook! 'magit-diff-mode-hook
    (setq-local truncate-lines nil))

  ;; wrap in magit status mode
  (add-hook 'magit-status-mode-hook (lambda () (+word-wrap-mode nil)))

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

(use-package! browse-at-remote
  :demand t
  :config
  (setq browse-at-remote-prefer-symbolic t)
  (add-to-list 'browse-at-remote-remote-type-regexps
               '(:host "^gitlab\\.cern.ch$" :type "gitlab")))

(use-package! forge

  :after magit

  :config

  (add-to-list 'auth-sources "~/.authinfo")

  (add-to-list 'forge-alist '("gitlab.cern.ch" "gitlab.cern.ch/api/v4" "gitlab.cern.ch" forge-gitlab-repository))

  (setq forge-topic-list-limit '(60 . 0)
        forge-owned-accounts '(("andrewpeck")
                               ("andrewpeck1")
                               ("apeck"))))

;;------------------------------------------------------------------------------
;; Git Gutter
;;------------------------------------------------------------------------------

(use-package! diff-hl

  :init
  (remove-hook! 'find-file-hook #'diff-hl-mode)
  (remove-hook! 'find-file-hook #'diff-hl-update-once)
  (add-hook! 'prog-mode-hook #'diff-hl-mode)

  :config
  (setq diff-hl-disable-on-remote t)
  (setq diff-hl-global-modes '(not image-mode org-mode markdown-mode pdf-view-mode))

  (advice-add 'diff-hl-update-once :before-until
              (lambda () (remote-host? default-directory))))
