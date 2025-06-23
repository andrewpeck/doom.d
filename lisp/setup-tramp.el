;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Tramp
;;------------------------------------------------------------------------------

(use-package! tramp

  :load-path "~/.emacs.d/.local/straight/repos/tramp"
  :config


  (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto"
        tramp-use-ssh-controlmaster-options t
        remote-file-name-access-timeout 1.0
        tramp-chunksize 5000
        tramp-histfile-override "~/.tramp_history"
        tramp-inline-compress-start-size 1000
        tramp-copy-size-limit (* 1024 1024)
        vc-handled-backends '(Git)
        tramp-verbose 1
        tramp-default-method "ssh")

  (setq auto-revert-check-vc-info nil)
  (setq auto-revert-remote-files nil)

  ;; https://www.gnu.org/software/tramp/#Remote-shell-setup

  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :machine "lab143")
   'remote-direct-async-process)

  (connection-local-set-profiles
   '(:application tramp :machine "strange")
   'remote-direct-async-process)

  (connection-local-set-profiles
   '(:application tramp :machine "larry")
   'remote-direct-async-process)

  ;; Another way to find the remote path is to use the path assigned to the remote user by the
  ;; remote host. TRAMP does not normally retain this remote path after login. However,
  ;; tramp-own-remote-path preserves the path value, which can be used to update tramp-remote-path.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; https://github.com/bbatsov/projectile/issues/1232
  ;; don't try to retrieve project name via projectile on remote dirs
  ;; slows down tramp really hard
  ;; (advice-add 'projectile-project-root
  ;;             :before-until
  ;;             (lambda (&optional _)
  ;;               (if (remote-host? default-directory)
  ;;                   (let ((pc (project-current)))
  ;;                     (if pc
  ;;                         (project-root pc))))))

  (advice-add 'projectile-project-root :before-while
              (lambda (&optional dir)
                (not (remote-host? (or dir default-directory)))))

  ;; HACK: avoid vc-refresh when on tramp
  (advice-add 'vc-refresh-state :before-while
              (lambda () (not (remote-host? default-directory))))

  ;; HACK: https://www.reddit.com/r/emacs/comments/yw3gpx/magit_autorevert_and_tramp/
  (defun my-magit-auto-revert-mode-advice (orig-fun &rest args)
    (unless (and buffer-file-name (remote-host? buffer-file-name))
      (apply orig-fun args)))

  (advice-add 'magit-turn-on-auto-revert-mode-if-desired :around 'my-magit-auto-revert-mode-advice)

  ;; HACK: tramp-get-home-directory gets called a lot and takes up a lot of CPU
  ;; time... memoizing it seems to result in a pretty significant speedup and it
  ;; doesn't seem like this is something that should change ever

  (unless (functionp 'm/tramp-get-home-directory)
    (require 'memoize)

    (defmemoize m/tramp-get-home-directory (vec &optional user)
      "The remote home directory for connection VEC as local file name.
If USER is a string, return its home directory instead of the
user identified by VEC.  If there is no user specified in either
VEC or USER, or if there is no home directory, return nil."
      (and (tramp-file-name-p vec)
           (with-tramp-connection-property vec (concat "~" user)
             (tramp-file-name-handler 'tramp-get-home-directory vec user)))))

  (advice-add 'tramp-get-home-directory :override
              'm/tramp-get-home-directory))
