;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Magit
;;------------------------------------------------------------------------------

(map! :after transient :map transient-map "C-c C-c" #'transient-save)

(map! :after magit

      ;;------------------------------------------------------------------------------
      ;; Git rebase mode
      ;;------------------------------------------------------------------------------

      (:map git-rebase-mode-map
            "A" #'my/magit-change-commit-author)

      ;;------------------------------------------------------------------------------
      ;; Magit Leader Commands
      ;;------------------------------------------------------------------------------

      (:leader :prefix "g"
       :desc "Magit Amend" "L" #'magit-log-buffer-file-and-follow
       :desc "Magit Amend" "A" #'magit-commit-amend
       :desc "Magit Push" "p" #'magit-push
       :desc "Magit Pull" "u" #'magit-pull-from-pushremote
       :desc "Magit Push Current" "P" #'magit-push-current-to-pushremote
       :desc "Instant Fixup" "cF" #'magit-commit-instant-fixup
       (:prefix ("z" . "Stash")
        :desc "Stash Apply" "a" #'magit-stash-apply
        :desc "Stash Pop" "p" #'magit-stash-pop
        :desc "Stash Delete" "d" #'magit-stash-drop
        :desc "Stash Keep Index" "x" #'magit-stash-keep-index
        :desc "Stash Both" "z" #'magit-stash-both
        :desc "Stash Index" "i" #'magit-stash-index
        :desc "Stash Worktree" "w" #'magit-stash-worktree
        :desc "Snapshot Both" "Z" #'magit-snapshot-both
        :desc "Snapshot Index" "I" #'magit-snapshot-index
        :desc "Snapshot Worktree" "W" #'magit-snapshot-worktree
        :desc "Stash List" "l" #'magit-stash-list
        :desc "Stash Show" "v" #'magit-stash-show)))

(defun my/git-set-file-mode ()
  "Set git file mode to false.

Useful for working on NAS where permissions don't make sense."
  (interactive)
  (shell-command "git config core.fileMode false"))

(defun my/setup-git-author-peckandrew ()
  "Setup git author in this pwd."
  (interactive)
  (shell-command "git config user.name \"Andrew Peck\"")
  (shell-command "git config user.email peckandrew@gmail.com"))

(use-package! magit

  :config

  (defun magit-log-buffer-file-and-follow (&optional beg end)
    (interactive (cons current-prefix-arg (magit-file-region-line-numbers)))
    (require 'magit)
    (magit-log-buffer-file t beg end))

  (setq-default magit-buffer-log-args
                '("-n256" "--follow" "--graph" "--decorate"))

  (setq magit-diff-visit-prefer-worktree t)

  ;; I don't use projectile anymore
  (remove-hook 'magit-refresh-buffer-hook '+magit-invalidate-projectile-cache-h)

  ;; copy short hashes only
  (setq magit-copy-revision-abbreviated t)

  ;; https://gist.github.com/danielmartin/34bc36dafd8f900de483394087230f48
  (defun my/magit-change-commit-author (arg)
    "Change the commit author during an interactive rebase in Magit.
With a prefix argument, insert a new change commit author command
even when there is already another rebase command on the current
line.  With empty input, remove the change commit author action
on the current line, if any."
    (interactive "P")
    (let ((author (magit-transient-read-person "Select a new author for this commit" nil nil)))
      (git-rebase-set-noncommit-action
       "exec"
       (lambda (_) (if author (format "git commit --amend --author='%s'" author) "")) arg)))

  (setq magit-list-refs-sortby "-creatordate")

  (setq magit-prefer-remote-upstream t)
  ;; (setq magit-branch-prefer-remote-upstream nil)
  (setq magit-branch-adjust-remote-upstream-alist
        '(("origin/master" . ".*")))

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
         #'forge-insert-issues
         #'forge-insert-pullreqs
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

  (defun remove-and-sum-files (files confirm)
    (when (or (not confirm)
              (yes-or-no-p (concat "Ok to remove the following files? " (string-join files " "))))
      (let ((file-size 0)
            (file-count 0))
        (dolist (file files)
          (setq file-count
                (+ file-count
                   (if (not (file-directory-p file)) 1
                     (string-to-number (shell-command-to-string (format "find %s | wc -l" file))))))
          (setq file-size (+ file-size
                             (string-to-number (shell-command-to-string (concat "du -d0 " file " | awk '{print $1}'")))))
          (if (file-directory-p file)
              (delete-directory file t t)
            (delete-file file t)))
        (message "Removed %d files totaling %d kb" file-count file-size))))

  (defun clean-git-ignored-with-whitelist ()
    "Clean all files in gitignore except those filtered by a Whitelist."
    (interactive)
    (let* ((whitelist '(".venv/"))
           (ignored-files (split-string (shell-command-to-string (format "git clean -ndX -- %s | awk '{print $3}'" (vc-root-dir)))))
           (files-to-rm (-remove (lambda (x) (-contains? whitelist x)) ignored-files)))
      (if files-to-rm (remove-and-sum-files files-to-rm t)
        (message "No files to remove."))))

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

  (setq diff-hl-disable-on-remote t
        diff-hl-global-modes '(not image-mode org-mode markdown-mode pdf-view-mode))

  (advice-add 'diff-hl-update-once :before-until
              (lambda () (remote-host? default-directory))))
