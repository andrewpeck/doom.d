;;------------------------------------------------------------------------------
;; Tramp
;;------------------------------------------------------------------------------

(use-package! tramp

  :load-path "~/.emacs.d/.local/straight/repos/tramp"
  :config


  (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto"
        tramp-use-ssh-controlmaster-options t
        tramp-histfile-override "~/.tramp_history"
        tramp-inline-compress-start-size 1000
        tramp-copy-size-limit 10000
        vc-handled-backends '(Git)
        tramp-verbose 1
        tramp-default-method "ssh")

  ;; https://www.gnu.org/software/tramp/#Remote-shell-setup
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/sshx:user@host:")
                     "remote-shell" "/usr/bin/sh"))

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
             (tramp-file-name-handler #'tramp-get-home-directory vec user)))))

  (advice-add 'tramp-get-home-directory :override
              #'m/tramp-get-home-directory))
